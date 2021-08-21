
(setq org-agenda-files '("~/workspace"))


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
	"REDIRECTED(R@)" "DELEGATED(D@)"
	"MERGED(m@)" "JIRA(j@)"))
(setq ym-org-todo-state-string-in-log "State:     (")
(setq org-todo-keywords
      `((sequence ,@ym-org-todo-keywords-undone "|" ,@ym-org-todo-keywords-done)))


;; -------------------------------------------------------------------

(use-package org-gcal
  :config
  (setq org-gcal-file-alist '(
			      ("alexander@superlearn.it" .  "~/workspace/Google_calendar.superlearn.org")
			      ("alexander.scherbanov@gmail.com" . "~/workspace/Google_calendar.gmail.org")
			      ))
  ;; (setq org-gcal-local-timezone "Europe/Moscow")


  ;; https://cestlaz.github.io/posts/using-emacs-26-gcal/


  ;; TODO: ignore if missing
  ;; (load-file "~/.org-gcal.secrets.el")

  ;; TODO: make sure org-gcal-token-file is in no-littering-var

  )

;; -------------------------------------------------------------------

(use-package org-ql)

(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  )

;; -------------------------------------------------------------------



(setq org-agenda-custom-commands
      '(
	("dc" "Drill cards"
	 (
	  (tags "+drill+SCHEDULED<=\"<today>\"-english-spanish" ((org-agenda-overriding-header "Due cards")))
	  (tags "+drilltodo" ((org-agenda-overriding-header "Unfinished cards")))
	  (tags "+drill-SCHEDULED={.}-english-spanish" ((org-agenda-overriding-header "New cards")))
	  ))
	("dC" "Show learned cards"
	 (
	  (tags "+drill+SCHEDULED>\"<today>\"-english-spanish")
	  ))
	("de" "Drill English"
	 (
	  (todo "" (
		    (org-agenda-files nil)
		    (org-agenda-overriding-header "test")
		    ))
	  (org-agenda-files '("~/workspace/English.org"))
	  (tags "+english+drill+SCHEDULED<=\"<today>\"" ((org-agenda-overriding-header "Due")))
	  (tags "+english+drill-SCHEDULED={.}" ((org-agenda-overriding-header "New")))
	  ))
	("dh" "habits"
	 (
	  (agenda "" (
		      (org-agenda-files '("~/workspace/Habits.org"))
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 1"))
		      (org-agenda-span 1)
		      (org-agenda-overriding-header "")
		      (org-agenda-todo-keyword-format "")
		      (org-agenda-prefix-format "")
		      ))
	  (agenda "" (
		      (org-agenda-files '("~/workspace/Habits.org"))
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 2"))
		      (org-agenda-span 1)
		      (org-agenda-overriding-header "")
		      (org-agenda-todo-keyword-format "")
		      (org-agenda-prefix-format "")
		      ))
	  (agenda "" (
		      (org-agenda-files '("~/workspace/Habits.org"))
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 3"))
		      (org-agenda-span 1)
		      (org-agenda-overriding-header "")
		      (org-agenda-todo-keyword-format "")
		      (org-agenda-prefix-format "")
		      ))
	  (agenda "" (
		      (org-agenda-files '("~/workspace/Habits.org"))
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":MY_HABITS_GROUP: 4"))
		      (org-agenda-span 1)
		      (org-agenda-overriding-header "")
		      (org-agenda-todo-keyword-format "")
		      (org-agenda-prefix-format "")
		      ))
	  (agenda "" (
		      (org-agenda-files '("~/workspace/Habits.org"))
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

	 )
	))


(if nil
    (org-ql-search "~/workspace/Habits.org"
      '(habit)
      :title "Habits"
      :super-groups '(
                      (:name "Personal"
                             )
		      )
      )
  )
