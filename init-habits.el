;; -------------------------------------------------------------------
(require 'org-habit)
(require 'org-checklist)   ; for the reset_check_boxes property of repeated tasks

(setq org-habit-show-all-today t)
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)

(setq org-habit-graph-column 17)
(setq org-habit-preceding-days 48)
(setq org-habit-following-days 3)

;; (setq ym-timer-list-to-show-habits   ; (dolist (x ym-timer-list-to-show-habits) (cancel-timer x))
;;       (list
;;        (run-at-time "07:00pm" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))
;;        (run-at-time "07:00am" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))))

(defun org-habit-get-priority (habit &optional moment) 1000)   ; this disables sorting by scheduled time, shows in the same order as in org file

;; (setq org-habit-today-glyph ?╋)
(setq org-habit-today-glyph ?⬛)
(setq org-habit-completed-glyph ?●)
(setq org-habit-missed-glyph ?○)

;; -------------------------------------------------------------------

;; severe monkey-patching here

;; there's no org-habit-missed-glyph var originally
;; TODO: pr this  monkey-patched org-habit function to org-mode repo

;; taken from https://github.com/bzg/org-mode/blob/master/lisp/org-habit.el

;; beware, there's a bug in the code:
;; when org-habit-following-days < 2 and the glyph is not space (?/s), it behaves weirdly

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((all-done-dates (sort (org-habit-done-dates habit) #'<))
	 (done-dates all-done-dates)
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) org-habit-missed-glyph))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates (= start (car done-dates))))
	     (faces
	      (if (and in-the-past-p
		       (not last-done-date)
		       (not (< scheduled now)))
		  (if (and all-done-dates (= (car all-done-dates) start))
		      ;; This is the very first done of this habit.
		      '(org-habit-ready-face . org-habit-ready-future-face)
		    '(org-habit-clear-face . org-habit-clear-future-face))
		(org-habit-get-faces
		 habit start
		 (and in-the-past-p
		      last-done-date
		      ;; Compute scheduled time for habit at the time
		      ;; START was current.
		      (let ((type (org-habit-repeat-type habit)))
			(cond
			 ;; At the last done date, use current
			 ;; scheduling in all cases.
			 ((null done-dates) scheduled)
			 ((equal type ".+") (+ last-done-date s-repeat))
			 ((equal type "+")
			  ;; Since LAST-DONE-DATE, each done mark
			  ;; shifted scheduled date by S-REPEAT.
			  (- scheduled (* (length done-dates) s-repeat)))
			 (t
			  ;; Compute the scheduled time after the
			  ;; first repeat.  This is the closest time
			  ;; past FIRST-DONE which can reach SCHEDULED
			  ;; by a number of S-REPEAT hops.
			  ;;
			  ;; Then, play TODO state change history from
			  ;; the beginning in order to find current
			  ;; scheduled time.
			  (let* ((first-done (car all-done-dates))
				 (s (let ((shift (mod (- scheduled first-done)
						      s-repeat)))
				      (+ (if (= shift 0) s-repeat shift)
					 first-done))))
			    (if (= first-done last-done-date) s
			      (catch :exit
				(dolist (done (cdr all-done-dates) s)
				  ;; Each repeat shifts S by any
				  ;; number of S-REPEAT hops it takes
				  ;; to get past DONE, with a minimum
				  ;; of one hop.
				  (cl-incf s (* (1+ (/ (max (- done s) 0)
						       s-repeat))
						s-repeat))
				  (when (= done last-done-date)
				    (throw :exit s))))))))))
		 donep)))
	     markedp face)
	(cond
	 (donep
	  (aset graph index org-habit-completed-glyph)
	  (setq markedp t)
	  (while (and done-dates (= start (car done-dates)))
	    (setq last-done-date (car done-dates))
	    (setq done-dates (cdr done-dates))))
	 (todayp
	  (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(when (and in-the-past-p
		   (not (eq face 'org-habit-overdue-face))
		   (not markedp))
	  (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph)
	(put-text-property index (1+ index)
			   'help-echo
			   (concat (format-time-string
				    (org-time-stamp-format)
				    (time-add starting (days-to-time (- start (time-to-days starting)))))
				   (if donep " DONE" ""))
			   graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))

;; -------------------------------------------------------------------


(defun my-org-todo-with-date (&optional arg date-str)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (if date-str
				  (org-read-date t t date-str)
				(org-read-date t t nil "when:")
				  ))
             ((symbol-function #'current-time)
              #'(lambda () my-current-time))
             ((symbol-function #'org-today)
              #'(lambda () (time-to-days my-current-time)))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () my-current-time)))
    (if (eq major-mode 'org-agenda-mode)
	(org-agenda-todo arg)
      (org-todo arg))
    ))


(setq org-agenda-bulk-custom-functions
      '(
	(?9 (lambda () (interactive) (my-org-todo-with-date "HABIT SKIPPED" "now")))
	(?0 (lambda () (interactive) (my-org-todo-with-date "DONE" "now")))
	(?1 (lambda () (interactive) (my-org-todo-with-date "DONE" "-1d 23:59")))
	(?2 (lambda () (interactive) (my-org-todo-with-date "DONE" "-2d 23:59")))
	(?3 (lambda () (interactive) (my-org-todo-with-date "DONE" "-3d 23:59")))
	(?4 (lambda () (interactive) (my-org-todo-with-date "DONE" "-4d 23:59")))
	(?5 (lambda () (interactive) (my-org-todo-with-date "DONE" "-5d 23:59")))
	(?6 (lambda () (interactive) (my-org-todo-with-date "DONE" "-6d 23:59")))
	(?7 (lambda () (interactive) (my-org-todo-with-date "DONE" "-7d 23:59")))
	(?8 (lambda () (interactive) (my-org-todo-with-date "DONE" "-8d 23:59")))
	))

;; make two more functions like org-agenda-bulk-custom-functions-2d, org-agenda-bulk-custom-functions-3d


;; TODO: run the following after midnight in the Habits.org
;; https://www.emacswiki.org/emacs/MidnightMode

(defun ym-org-fix-habits-skipped ()
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-buffer)
      (while (search-forward "- State \"HABIT SKIPPED\"" nil t)
	(let* ((date-str-on-the-line (save-match-data
				       (re-search-forward "\\[\\(.*\\)\\]" (line-end-position))
				       (match-string 1)))
	       (today-str (org-read-date nil nil "today"))
	       (number-of-days-since-the-date (- (org-time-string-to-absolute today-str)
						 (org-time-string-to-absolute date-str-on-the-line)))
	       )
	  (when (>= number-of-days-since-the-date 1)
	    ;; I have no idea why, but the `save-match-data` above doesn't work, I get `args-out-of-range`
	    ;; but I don't care, it's a hack anyway, so I search for the substring once again
	    (save-match-data
	      (beginning-of-line)
	      (search-forward "- State \"HABIT SKIPPED\"" nil t)
	      (replace-match "- State \"HABIT\"")))
	  )))))

  ;; -------------------------------------------------------------------
