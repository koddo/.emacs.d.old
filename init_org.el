(setq org-modules '(org-habit org-id org-inlinetask))        ; (add-to-list 'org-modules 'org-habit)
(require 'org)
(require 'org-id)
;;         -- (require 'org-special-blocks)
;; (require 'org-table)
;; (require 'org-colview)
;; (require 'org-datetree)
;; (require 'org-archive)


(setq org-log-repeat nil)


;; -------------------------------------------------------------------
(setq org-habit-show-all-today t)
(setq org-habit-show-habits nil)
(setq org-habit-show-habits-only-for-today t)
(setq org-habit-graph-column 55)
(setq org-habit-preceding-days 35)
(setq org-habit-following-days 2)
;; (setq ym-timer-list-to-show-habits   ; (dolist (x ym-timer-list-to-show-habits) (cancel-timer x))
;;       (list
;;        (run-at-time "07:00pm" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))
;;        (run-at-time "07:00am" (* 60 60 24)
;;                    (lambda () (setq org-habit-show-habits t)))))
(defun org-habit-get-priority (habit &optional moment) 1000)   ; this disables sorting by scheduled time, shows in the same order as in org file
;; -------------------------------------------------------------------
(setq org-startup-with-inline-images t)
(setq org-cycle-separator-lines 0)
(setq org-hide-leading-stars t)
(setq org-startup-folded 'content)
(setq org-empty-line-terminates-plain-lists t)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
(setq org-support-shift-select t)
(setq org-return-follows-link nil)
(setq org-catch-invisible-edits 'error)
(setq org-completion-use-ido t)
(setq-default org-use-sub-superscripts '{})
(setq org-src-fontify-natively t)
(setq org-deadline-warning-days 100000)
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
;; -------------------------------------------------------------------
(setq org-directory "~/workspace")
(setq ym-org-agenda-file "~/workspace/.org.agenda.org.gpg")
(setq org-default-notes-file "~/workspace/.org.notes.org")
(setq ym-org-birthdays-file "~/workspace/.org.birthdays.org")
(setq ym-org-journal-file "~/workspace/.org.journal.org.gpg")
(setq ym-ledger-file "~/workspace/.org.finances.ledger.gpg")
(setq ym-work-file "~/workspace/Work.org.gpg")
(setq org-agenda-files (list ym-org-agenda-file ym-work-file org-directory))
(setq ym-org-clock-buffer-name "*Org Clock Summary*")
(setq ym-org-contacts-view-buffer-name "*Org Contacts*")
(setq ym-org-problems-count-view-buffer-name "*Org Problems Count*")
(setq org-latex-preview-ltxpng-directory ".ltxpng/")
(setq org-archive-location ".org.archive.org.gpg::")
;; -------------------------------------------------------------------
(setq org-refile-targets '((org-agenda-files . (:tag . "PROJECT"))))
(setq org-refile-use-outline-path nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-tags-exclude-from-inheritance '("PROJECT"))   ; TODO: remove TASKS from list
(setq org-reverse-note-order t)
;; -------------------------------------------------------------------
(setq org-agenda-tags-column -110)
(setq org-complete-tags-always-offer-all-agenda-tags t)
;; -------------------------------------------------------------------
(setq org-agenda-log-mode-items '(state))
(setq org-log-refile 'time)
(setq org-log-into-drawer t)
(setq org-log-states-order-reversed nil)
(setq org-log-redeadline 'note)
(setq org-log-reschedule 'note)
(setq org-agenda-skip-scheduled-if-done t)
;; -------------------------------------------------------------------
(setq org-agenda-time-grid '((daily today)
                             "----------------"
                             (900 1000 1100 1200 1300 1400 1500 1600
                                  1700 1800 1900 2000 2100 2200 2300 2359)))
;; (setq org-agenda-time-grid '((daily today)
;;                              "----------------"
;;                              (800 1000 1200 1400 1600 1800 2000 2200 2350)))
;; (defun ym-org-agenda-sort-habits (a b)
;;   (let ((ha (get-text-property 0 'todo-state a))
;;         (hb (get-text-property 0 'todo-state b)))
;;     (when (and ha hb)
;;         (cond ((string> ha hb) +1)
;;               ((string< ha hb) -1)
;;               ((string= ha hb) nil)))))
;; (setq org-agenda-cmp-user-defined 'ym-org-agenda-sort-habits)
(setq org-agenda-sorting-strategy
      '(
        (agenda time-up habit-down
                ;; user-defined-down
                alpha-up priority-up category-keep)
        (todo todo-state-down priority-down category-up)
        (tags priority-down category-keep)
        (search category-keep)))
;; -------------------------------------------------------------------
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies nil)
(setq org-agenda-dim-blocked-tasks t)   ; was 'invisible, broke my stuck projects list
(setq org-agenda-todo-list-sublevels t)
;; -------------------------------------------------------------------
(setq ym-org-todo-keywords-working '("WORKING(w!)" "WAITING(W!)" "PAUSED(p!)"))   ; STATES
(setq ym-org-todo-keywords-undone `("REGULARLY(R!)" "NEXT(n!)" "TODO(t!)" ,@ym-org-todo-keywords-working))
(setq ym-org-todo-keywords-done '("DONE(d!)" "CANCELLED(c@)" "REDIRECTED(r@)" "MERGED(m@)"))
(setq ym-org-todo-state-string-in-log "State:     (")
(setq org-todo-keywords
      `((sequence ,@ym-org-todo-keywords-undone "|" ,@ym-org-todo-keywords-done)))
(setq ym-org-todo-keywords-working-regexp
      (concat "\\("
              (mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-working "\\|")
              "\\)"))
(setq ym-org-todo-keywords-undone-regexp
      (concat ym-org-todo-state-string-in-log "\\("
              (mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-undone "\\|")
              "\\))"))
(setq ym-org-todo-keywords-done-regexp
      (concat ym-org-todo-state-string-in-log "\\("
              (mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-done "\\|")
              "\\))"))
;; -------------------------------------------------------------------
(setq org-capture-templates
      '(("t" "todo" entry (file+headline ym-org-agenda-file "Tasks")
         "* TODO %?\n  :LOGBOOK:\n  - Added %U\n  :END:" :empty-lines 1)
        ("T" "todo with context" entry (file+headline ym-org-agenda-file "Tasks")
         "* TODO %?\n  :LOGBOOK:\n  - Added %U\n  :END:\n  %a" :empty-lines 1)
        ))
;; -------------------------------------------------------------------
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time (* 4 60))
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-modeline-total 'auto)
(setq org-clock-report-include-clocking-task t)
(setq org-columns-default-format "%10Effort(Effort){:} %10CLOCKSUM{:} %80ITEM(Task)")
(setq org-global-properties '(("Effort_ALL" . "1:00 2:00 3:00 4:00 5:00 0:50 0:40 0:30 0:20 0:10")))
(setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
;; -------------------------------------------------------------------
;; "unstuck" tag is for toplevel projects that I don't want to see in stuck projects, example:   * Ruby  :PROJECT:UNSTUCK:
(setq org-stuck-projects '("PROJECT" ("NEXT" "WORKING" "PAUSED") ("UNSTUCK") nil))
;; (setq org-stuck-projects '("PROJECT" ("NEXT") nil nil))

;; TODO: remove helper functions below
;; helper functions for finding stuck projects
;; a heading is a project if it is a TODO and has TODO children, but it does not have NEXT
;; pasted from http://doc.norang.ca/org-mode.html#Projects
;; (setq org-stuck-projects '("" nil nil ""))   ; for custom stuck project definition
;; (defun bh/is-project-p ()
;;   "Any task with a todo keyword subtask"
;;   (save-restriction
;;     (widen)
;;     (let ((has-subtask)
;;           (subtree-end (save-excursion (org-end-of-subtree t)))
;;           (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
;;       (save-excursion
;;         (forward-line 1)
;;         (while (and (not has-subtask)
;;                     (< (point) subtree-end)
;;                     (re-search-forward "^\*+ " subtree-end t))
;;           (when (member (org-get-todo-state) org-todo-keywords-1)
;;             (setq has-subtask t))))
;;       (and is-a-task has-subtask))))
;; (defun bh/list-sublevels-for-projects-indented ()
;;   "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
;;   This is normally used by skipping functions where this variable is already local to the agenda."
;;   (if (marker-buffer org-agenda-restrict-begin)
;;       (setq org-tags-match-list-sublevels 'indented)
;;     (setq org-tags-match-list-sublevels nil))
;;   nil)
;; (defun bh/skip-non-stuck-projects ()   ; used in org-agenda-custom-commands as skip function
;;   "Skip trees that are not stuck projects"
;;   (bh/list-sublevels-for-projects-indented)
;;   (save-restriction
;;     (widen)
;;     (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
;;       (if (bh/is-project-p)
;;           (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
;;                  (has-next ))
;;             (save-excursion
;;               (forward-line 1)
;;               (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ \\(NEXT\\|WORKING\\|WAITING\\) " subtree-end t))
;;                 (unless (member "WAITING" (org-get-tags-at))
;;                   (setq has-next t))))
;;             (if has-next
;;                 next-headline
;;               nil)) ; a stuck project, has subtasks but no next task
;;         next-headline))))
;; -------------------------------------------------------------------
(setq org-agenda-current-time-string "------------------------------- now -------------------------------")
(setq ym-org-agenda-planning-header-now "Now:")
(setq ym-org-agenda-planning-header-paused "Paused:")
(setq ym-org-agenda-planning-header-top "Top:")
(setq ym-org-agenda-planning-header-next-actions "Next actions:")
(setq ym-org-agenda-planning-header-non-scheduled "Non-scheduled tasks:")
(setq ym-org-agenda-planning-header-stuck-projects "Stuck projects:")
(setq org-agenda-custom-commands   ; STATES
      '(("0" "Only working to quickly switch between them"
         (
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-now)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WORKING")))))
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-paused)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("PAUSED" "WAITING")))))
          (tags-todo "+top"
                     ((org-agenda-overriding-header ym-org-agenda-planning-header-top)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("NEXT")))
                      ))
          ))
        ("1" "Daily schedule"
         (
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-now)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WORKING")))))
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-paused)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("PAUSED" "WAITING")))))
          (tags-todo "+top"
                     ((org-agenda-overriding-header ym-org-agenda-planning-header-top)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("NEXT")))
                      ))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-show-log t)))
          (tags-todo "-top"
          ;; (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-next-actions)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("NEXT")))))
          ))
        ("2" "Weekly schedule"
         ((agenda "" (
                      ;; (org-agenda-start-day "-3d")
                      (org-agenda-span 'week)    ; unfortunately, when set to number, it hides all non-scheduled tasks
                      (org-agenda-show-log t)
                      ;; (org-agenda-entry-types '(:deadline :scheduled :timestamp))
                      ;; (org-agenda-repeating-timestamp-show-all nil)
                      ))
          ))
        ("3" "Planning"
         ((todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-now)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("WORKING")))))
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-paused)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("PAUSED" "WAITING")))))
          (tags-todo "+top"
                     ((org-agenda-overriding-header ym-org-agenda-planning-header-top)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("NEXT")))
                      ))
          ;; (todo ""
          (tags-todo "-top"
                ((org-agenda-overriding-header ym-org-agenda-planning-header-next-actions)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("NEXT")))))
          ;; (tags-todo "-CANCELLED/!"
          ;;            ((org-agenda-overriding-header ym-org-agenda-planning-header-stuck-projects)
          ;;             (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          (stuck ""
                 ((org-agenda-overriding-header ym-org-agenda-planning-header-stuck-projects)))
          (todo ""
                ((org-agenda-overriding-header ym-org-agenda-planning-header-non-scheduled)
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'nottodo '("TODO")))))
          (todo ""
                ((org-agenda-overriding-header "Scheduled tasks:")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp))))
          ))
        ))
;; -------------------------------------------------------------------
;; (setq org-export-html-postamble nil)
;; (setq org-export-with-section-numbers nil)
;; (setq org-export-preserve-breaks t)
;; (setq org-export-latex-packages-alist nil)
;; (add-to-list 'org-export-latex-packages-alist '("russian" "babel" t))
;; (add-to-list 'org-export-latex-packages-alist '("" "amsmath" t))
;; (add-to-list 'org-export-latex-packages-alist '("" "bm" t))   ; bold math: $\bm \alpha$
;; -------------------------------------------------------------------
(add-hook 'org-clock-in-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" (concat "tell application \"org-clock-statusbar\" to clock in \"" (replace-regexp-in-string "\"" "\\\\\"" org-clock-current-task) "\""))))
(add-hook 'org-clock-out-hook (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e" "tell application \"org-clock-statusbar\" to clock out")))
;; -------------------------------------------------------------------




