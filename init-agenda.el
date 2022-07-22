
(setq org-agenda-files '("~/werk"))


;; -------------------------------------------------------------------

(setq org-log-into-drawer t      ; log both into :LOGBOOK:
      org-clock-into-drawer t)
(setq org-log-repeat nil)   ; disable :LAST_REPEAT:
(setq org-todo-repeat-to-state t)   ; habit -> done -> habit, not todo

;; (setq org-agenda-log-mode-items '(state))
;; (setq org-log-refile 'time)
;; (setq org-log-states-order-reversed nil)
;; (setq org-log-redeadline 'note)
;; (setq org-log-reschedule 'note)
;; (setq org-agenda-skip-scheduled-if-done t)

;; -------------------------------------------------------------------

;; t vs. T
;; "PAUSED(p!)"
;; "WORKING(w!)"
(setq ym-org-todo-keywords-working '("TODAY(T!)" "NOW(n!)"))
(setq ym-org-todo-keywords-undone
      `("TODO(t!)" "NEXT(n!)"
	,@ym-org-todo-keywords-working
	"POSTPONED(P!)" "WAITING(W!)" "IN PROGRESS(i!)"
	"HABIT(h/@)" "REGULARLY(r!)" "SOMEDAY(S!)" "MAYBE(M!)"
	))
(setq ym-org-todo-keywords-done
      '("DONE(d!)"
	"CANCELED(c@)"
	"HABIT SKIPPED(!)"
	"REDIRECTED(R@)" "DELEGATED(D@)"
	"MERGED(m@)" "JIRA(j@)"))
(setq ym-org-todo-state-string-in-log "State:     (")
(setq org-todo-keywords
      `((sequence ,@ym-org-todo-keywords-undone "|" ,@ym-org-todo-keywords-done)))


;; -------------------------------------------------------------------

(use-package org-gcal
  :config
  (setq org-gcal-file-alist '(
			      ("alexander@superlearn.it" .  "~/werk/Google_calendar.superlearn.org")
			      ("alexander.scherbanov@gmail.com" . "~/werk/Google_calendar.gmail.org")
			      ))
  ;; (setq org-gcal-local-timezone "Europe/Moscow")


  ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/


  ;; TODO: ignore if missing
  ;; (load-file "~/.org-gcal.secrets.el")

  ;; TODO: make sure org-gcal-token-file is in no-littering-var

  )

;; -------------------------------------------------------------------

(use-package org-ql
  :config
  )

(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  )

;; -------------------------------------------------------------------


;; TODO: try https://github.com/alphapapa/org-ql#function-org-ql-block
;; https://github.com/alphapapa/org-ql/blob/master/examples.org
;; and https://github.com/alphapapa/org-super-agenda in one of the views
;; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org
;; https://github.com/alphapapa/org-sidebar





(let ((asdf '(org-agenda-overriding-header "\n\n\n-=*=- this title gets replaced -=*=-"))
      (my-agenda-view-detailed (lambda (files)
				 `(
				   (tags-todo "+SCHEDULED<\"<+0d>\"|+sa+TIMESTAMP<\"<+0d>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header " ")
					       
					       ))
				   (agenda ""
					   (
					    ;; ????
					    ;; (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
					    (org-agenda-overriding-header " ")
					    (org-agenda-files ',files)
					    (org-agenda-span 'day)
					    (org-scheduled-past-days 0)
					    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\*+.*:st:$"))
					    (org-agenda-skip-scheduled-if-done t)
					    ))
				   (tags-todo "+TIMESTAMP<=\"<today>\"-sa|-TIMESTAMP={.}+TIMESTAMP_IA<=\"<today>\"|-TIMESTAMP={.}-TIMESTAMP_IA={.}-SCHEDULED={.}"     ; |+SCHEDULED<=\"<today>\"|+DEADLINE<=\"<today>\"
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header " ")
					       ))
				   (todo "" (
					     (org-agenda-files nil)
					     (org-agenda-overriding-header "\n\n\n\n\n")
					     ))
				   (agenda ""
					   (
					    (org-agenda-overriding-header " ")
					    (org-agenda-files ',files)
					    (org-agenda-start-day "+1d")
					    (org-agenda-span 10)
					    (org-agenda-start-on-weekday nil)
					    (org-agenda-show-log t)
					    (org-scheduled-past-days 0)
					; (org-agenda-hide-tags-regexp ,(regexp-opt '("st" "sa")))
					    (org-agenda-skip-scheduled-if-done t)
					    (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\*+.*:st:$"))   ; quick and dirty -- hide tagged with :st: from agenda view, couldn't find a better way yet
					; neither of these work: (org-agenda-filter-by-tag "h"), (org-agenda-tag-filter-preset '("-h")); maybe try this to filter agenda view by tag: https://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view/33444799#33444799
					; excessive inactive timestamps show in agenda -- :st: -- Hide from Agenda
					; excessive active timestamps show in todo -- :sa: -- Hide from Todo
					; see org-agenda-hide-tags-regexp


					    ))
				   (tags-todo "+TIMESTAMP=\"<+1d>\"-sa|-TIMESTAMP={.}+TIMESTAMP_IA=\"<+1d>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "+1d")
					       ))
				   (tags-todo "+TIMESTAMP=\"<+2d>\"-sa|-TIMESTAMP={.}+TIMESTAMP_IA=\"<+2d>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "+2d")
					       ))
				   (tags-todo "+TIMESTAMP=\"<+3d>\"-sa|-TIMESTAMP={.}+TIMESTAMP_IA=\"<+3d>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "+3d")
					       ))
				   (tags-todo "+TIMESTAMP>\"<+3d>\"&+TIMESTAMP<=\"<+10d>\"-sa|-TIMESTAMP={.}+TIMESTAMP_IA>\"<+3d>\"&+TIMESTAMP_IA<=\"<+10d>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "Next 10d")
					       ))
				   (todo "" (
					     (org-agenda-files nil)
					     (org-agenda-overriding-header " \n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
					     ))
				   (tags-todo "+TIMESTAMP>\"<+10d>\"&+TIMESTAMP<=\"<+1m>\"|-TIMESTAMP={.}+TIMESTAMP_IA>\"<+10d>\"&+TIMESTAMP_IA<=\"<+1m>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "Next 1m")
					       ))
				   (tags-todo ,(concat "+TIMESTAMP>\"<+1m>\"&+TIMESTAMP<=\"<+3m>\"|-TIMESTAMP={.}+TIMESTAMP_IA>\"<+1m>\"&+TIMESTAMP_IA<=\"<+3m>\"")
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "Next 3m")
					       ))
				   ;; (tags-todo "+TIMESTAMP_IA>\"<+3m>\"|-TIMESTAMP={.}+TIMESTAMP_IA>\"<+3m>\""
				   (tags-todo "+TIMESTAMP>\"<+3m>\"|-TIMESTAMP={.}+TIMESTAMP_IA>\"<+3m>\""
					      (
					       (org-agenda-files ',files)
					       (org-agenda-overriding-header "Later")
					       ))
				   )))
      (my-read-watch-list (lambda (tag)   ; e.g., "read" or "watch"
			    (let ((my-read-watch-list-tags (concat
							    "-" tag "ing"
							    "-" tag "ing1"
							    "-" tag "ing2"
							    "-" tag "ing3"
							    "-" tag "ing4"
							    )))  ; e.g., "-reading-reading1-reading2-reading3-reading4")
			      `(
				(todo "" (
					  (org-agenda-files nil)
					  (org-agenda-overriding-header "-=*=- this title gets replaced -=*=-")
					  ))
				(tags ,(concat "+" tag "ing-done-cancelled")      ; e.g., "+reading-done-cancelled"
				      ((org-agenda-overriding-header ,(concat tag))
				       ))
				(tags ,(concat "+" tag "ing1-done-cancelled")
				      ((org-agenda-overriding-header ,(concat tag "1"))
				       ))
				(tags ,(concat "+" tag "ing2-done-cancelled")
				      ((org-agenda-overriding-header ,(concat tag "2"))
				       ))
				(tags ,(concat "+" tag "ing3-done-cancelled")
				      ((org-agenda-overriding-header ,(concat tag "3"))
				       ))
				(tags ,(concat "+" tag "ing4-done-cancelled")
				      ((org-agenda-overriding-header ,(concat tag "4"))
				       ))
				(todo "" (
					  (org-agenda-files nil)
					  (org-agenda-overriding-header "\n\n\n\n\n")
					  ))
				(tags ,(concat "+TIMESTAMP_IA<=\"<+0d>\"+" tag "-done-cancelled" my-read-watch-list-tags "|" "-TIMESTAMP_IA={.}+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header ,tag)
				       ))
				(todo "" (
					  (org-agenda-files nil)
					  (org-agenda-overriding-header "\n\n\n\n\n")
					  ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+0d>\"&+TIMESTAMP_IA<=\"<+10d>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "Next 10d")
				       ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+10d>\"&+TIMESTAMP_IA<=\"<+1m>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "Next month")
				       ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+1m>\"&+TIMESTAMP_IA<=\"<+3m>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "Next three months")
				       ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+3m>\"&+TIMESTAMP_IA<=\"<+6m>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "next half a year")
				       ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+6m>\"&+TIMESTAMP_IA<=\"<+12m>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "next year")
				       ))
				(tags ,(concat "+TIMESTAMP_IA>\"<+12m>\"+" tag "-done-cancelled" my-read-watch-list-tags)
				      ((org-agenda-overriding-header "later")
				       ))

				)
			      )))
      )

  (setq org-agenda-custom-commands
	`(
	  
	  ("dc" "Drill cards"
	   (              ; try org-agenda-file-regexp to exclude English.org
	    (tags "+drill+SCHEDULED<=\"<-3w>\"-english-spanish"
		  ((org-agenda-overriding-header "Overdue 3w")
		   ))
	    (tags "+drill+SCHEDULED<=\"<-1w>\"-english-spanish&+drill+SCHEDULED>\"<-3w>\"-english-spanish"
		  ((org-agenda-overriding-header "Overdue 1w")
		   ))
	    (tags "+drill+SCHEDULED<=\"<today>\"-english-spanish&+drill+SCHEDULED>\"<-1w>\"-english-spanish"
		  ((org-agenda-overriding-header "Due")
		   ))
	    (tags "+drill-SCHEDULED={.}-english-spanish"
		  ((org-agenda-overriding-header "New")
		   ))
	    (todo "" (
		      (org-agenda-files nil)
		      ,asdf
		      ))
	    (tags "+drilltodo"
		  ((org-agenda-overriding-header "Unfinished cards")
		   ))
	    ))
	  ("dC" "Show learned cards"
	   (
	    (tags "+drill+SCHEDULED>\"<today>\"-english-spanish")
	    ))
	  ;; ("dE" "org-ql"
	  ;;  ((org-agenda-files)
	  ;;   (org-ql-block   ; :sort '(date) isn't supported yet: https://github.com/alphapapa/org-ql/issues/79
	  ;;    '(and (tags "drill") (not (tags "english" "spanish" "humor"))))))
	  ("de" "Drill English"
	   (
	    (todo "" (
		      (org-agenda-files nil)
		      (org-agenda-overriding-header "-=*=- this title gets replaced -=*=-")
		      ))
	    (tags "+english+drill+SCHEDULED<=\"<-3w>\""
		  ((org-agenda-files '("~/werk/English.org"))
		   (org-agenda-overriding-header "Overdue 3w")
		   ))
	    (tags "+english+drill+SCHEDULED<=\"<-1w>\"&+english+drill+SCHEDULED>\"<-3w>\""
		  ((org-agenda-files '("~/werk/English.org"))
		   (org-agenda-overriding-header "Overdue 1w")
		   ))
	    (tags "+english+drill+SCHEDULED<=\"<today>\"&+english+drill+SCHEDULED>\"<-1w>\""
		  ((org-agenda-files '("~/werk/English.org"))
		   (org-agenda-overriding-header "Due")
		   ))
	    (tags "+english+drill-SCHEDULED={.}"
		  ((org-agenda-files '("~/werk/English.org"))
		   (org-agenda-overriding-header "New")
		   ))
	    ))
	  ("dr" "read"
	   ,(funcall my-read-watch-list "read")
	   )
	  ("dw" "watch"
	   ,(funcall my-read-watch-list "watch")
	   )
	  ("dl" "listen"
	   ,(funcall my-read-watch-list "listen")
	   )

	  ("dh" "habits"
	   (
	    (agenda "" (
			(org-agenda-files '("~/werk/Habits.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 1"))
			(org-agenda-span 1)
			(org-agenda-overriding-header "")
			(org-agenda-todo-keyword-format "")
			(org-agenda-prefix-format "")
			))
	    (agenda "" (
			(org-agenda-files '("~/werk/Habits.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 2"))
			(org-agenda-span 1)
			(org-agenda-overriding-header "")
			(org-agenda-todo-keyword-format "")
			(org-agenda-prefix-format "")
			))
	    (agenda "" (
			(org-agenda-files '("~/werk/Habits.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 3"))
			(org-agenda-span 1)
			(org-agenda-overriding-header "")
			(org-agenda-todo-keyword-format "")
			(org-agenda-prefix-format "")
			))
	    (agenda "" (
			(org-agenda-files '("~/werk/Habits.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 4"))
			(org-agenda-span 1)
			(org-agenda-overriding-header "")
			(org-agenda-todo-keyword-format "")
			(org-agenda-prefix-format "")
			))
	    (agenda "" (
			(org-agenda-files '("~/werk/Habits.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 5"))
			(org-agenda-span 1)
			(org-agenda-overriding-header "")
			(org-agenda-todo-keyword-format "")
			(org-agenda-prefix-format "")
			))
	    ))
	  ("x1" "today"
	   (
	    (agenda ""
		    (
		     (org-agenda-span 1)
                     (org-agenda-show-log t)
		     ))
	    ))
	  ("x2" "21 days"
	   (
	    (agenda ""
		    (
		     (org-agenda-files '("~/werk/Agenda.org"))
		     (org-agenda-start-day "-7d")
		     (org-agenda-span 35)
		     (org-agenda-start-on-weekday nil)
                     (org-agenda-show-log t)
		     ))
	    ))

	  ("x3" "test"
	   ,(funcall my-agenda-view-detailed
		     ;; '("~/werk")
		     (directory-files "~/werk" t "Agenda")
		     ;; '("~/werk/Agenda.org" "~/werk/Agenda-work.org" "~/werk/Agenda-superlearn.org")
		     )
	   (
	    ;; (org-agenda-files '("~/werk/Agenda.org"))
	    (org-agenda-tag-filter-preset '())
	    ;; (org-agenda-hide-tags-regexp (regexp-opt '("st" "sa")))
	    )
	   
	   )
	  ("x4" "test"
	   ,(funcall my-agenda-view-detailed '("~/werk/Agenda-work.org"))
	   (
	    ;; (org-agenda-files '("~/werk/Agenda.org"))
	    (org-agenda-tag-filter-preset '())
	    ;; (org-agenda-hide-tags-regexp (regexp-opt '("st" "sa")))
	    )
	   
	   )

	  ("x9" "test"
	   (
	    (agenda ""
		    (
		     (org-agenda-overriding-header " ")
		     (org-agenda-files '("~/werk/Agenda.org"))
		     (org-agenda-start-day "-7d")
		     (org-agenda-span 42)
		     (org-agenda-start-on-weekday nil)
                     (org-agenda-show-log t)
		     (org-scheduled-past-days 0)
		     (org-agenda-skip-scheduled-if-done t)
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "^\*+.*:st:$"))
		     ))
	    

	    )
	   ((org-agenda-tag-filter-preset '("+home")))
	   )

	  )))


(if nil
    (org-ql-search "~/werk/Habits.org"
      '(habit)
      :title "Habits"
      :super-groups '(
                      (:name "Personal"
                             )
		      )
      )
  )






;; https://emacs.stackexchange.com/questions/29585/key-binding-to-reschedule-agenda-todo-list-by-n-days




