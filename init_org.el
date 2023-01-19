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


