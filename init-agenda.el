
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

(use-package org-ql)

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

(let ((asdf
       '(org-agenda-overriding-header "























-=*=- this title gets replaced -=*=-")
	    ))
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
	   (
	    (todo "" (
		      (org-agenda-files nil)
		      (org-agenda-overriding-header "-=*=- this title gets replaced -=*=-")
		      ))
	    (tags "+reading"
		  ((org-agenda-overriding-header "Reading")
		   ))
	    (tags "+reading1"
		  ((org-agenda-overriding-header "Reading 1")
		   ))
	    (tags "+reading2"
		  ((org-agenda-overriding-header "Reading 2")
		   ))
	    (tags "+reading3"
		  ((org-agenda-overriding-header "Reading 3")
		   ))
	    (tags "+reading4"
		  ((org-agenda-overriding-header "Reading 4")
		   ))
	    (tags "+reading5"
		  ((org-agenda-overriding-header "Reading 5")
		   ))
	    (tags "+reading6"
		  ((org-agenda-overriding-header "Reading 6")
		   ))
	    (tags "+reading7"
		  ((org-agenda-overriding-header "Reading 7")
		   ))
	    (todo "" (
		      (org-agenda-files nil)
		      ,asdf
		      ))
	    (tags "+read-reading-reading2-reading3-reading4-reading5"
		  ((org-agenda-overriding-header "")
		   ))

	    ))
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
	   (
	    (tags-todo "+SCHEDULED<\"<-5d>\""
		       (
			(org-agenda-files '("~/werk/Agenda.org"))
			(org-agenda-overriding-header " ")
			))
	    (agenda ""
		    (
		     (org-agenda-files '("~/werk/Agenda.org"))
		     (org-agenda-start-day "-5d")
		     (org-agenda-span 20)
		     (org-agenda-start-on-weekday nil)
                     (org-agenda-show-log t)
		     (org-scheduled-past-days 0)
		     ))
	    (tags-todo "-TIMESTAMP_IA={.}-SCHEDULED={.}|+TIMESTAMP_IA<=\"<today>\""
		       (
			(org-agenda-files '("~/werk/Agenda.org"))
			(org-agenda-overriding-header "TODO")
			))
	    (tags-todo "+TIMESTAMP_IA>\"<today>\"&+TIMESTAMP_IA<=\"<+1w>\""
		       (
			(org-agenda-files '("~/werk/Agenda.org"))
			(org-agenda-overriding-header "Postponed for a couple of days")
			))
	    (tags-todo "+TIMESTAMP_IA>\"<+1w>\"&+TIMESTAMP_IA<=\"<+4w>\""
		       (
			(org-agenda-files '("~/werk/Agenda.org"))
			(org-agenda-overriding-header "Postponed > +1w")
			))
	    (tags-todo "+TIMESTAMP_IA>\"<+4w>\""
		       (
			(org-agenda-files '("~/werk/Agenda.org"))
			(org-agenda-overriding-header "Postponed > +4w")
			))
	    ))
	  ("x4" "test"
	   (

	    ))

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
