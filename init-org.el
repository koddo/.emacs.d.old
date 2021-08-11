;;; -*- lexical-binding: t; -*-

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib

  :init
  (require 'face-remap)
  (setq ym-org-latex-preview-scale 1.0)   ; depends on the font used in emacs or just on user preference
  (defun org-latex-preview-advice (orig-func &rest args)
    (let ((old-val (copy-tree org-format-latex-options)))     ; plist-put is maybe-destructive, weird. So, we have to restore old value ourselves
      (setq org-format-latex-options (plist-put org-format-latex-options
                                                :scale
                                                (* ym-org-latex-preview-scale (expt text-scale-mode-step text-scale-mode-amount))))
      (apply orig-func args)
      (setq org-format-latex-options old-val)))
  (advice-add 'org-latex-preview :around #'org-latex-preview-advice)

  :config
  (setq-default org-adapt-indentation nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)   ; usage: org-id, org-store-link, org-insert-link, org-id-update-id-locations
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-preview-latex-image-directory ".ltximg/")


  ;; t vs. T
  ;; "PAUSED(p!)"
  ;; "WORKING(w!)"
  (setq ym-org-todo-keywords-working '("TODAY(T!)" "NOW(n!)"))
  (setq ym-org-todo-keywords-undone `("TODO(t!)" "NEXT(n!)" ,@ym-org-todo-keywords-working "POSTPONED(P!)" "WAITING(W!)" "IN PROGRESS(i!)" "REGULARLY(r!)" "SOMEDAY(S!)" "MAYBE(M!)"))
  (setq ym-org-todo-keywords-done '("DONE(d!)" "CANCELLED(c@)" "REDIRECTED(R@)" "DELEGATED(D@)" "MERGED(m@)" "JIRA(j@)"))
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

  )



;; take screenshots and drag-n-drop copies of images
;; and put them to gitroot/.images -- this makes moving links around easy
;; we'll figure out how to garbage collect later when it becomes a problem
;; https://github.com/abo-abo/org-download
;; functions of interest: org-download-screenshot, org-download-image, org-download-edit, org-download-delete
(use-package org-download
  :after org

  :init
  (defun org-download-advice (orig-func &rest args)
    (let ((org-download-image-dir
           (expand-file-name ".images" (vc-root-dir))))
     (apply orig-func args)))
  (advice-add 'org-download-screenshot :around #'org-download-advice)
  (advice-add 'org-download-image :around #'org-download-advice)

  :config
  (setq-default org-download-heading-lvl nil)   ; don't take header text into account, just put everything into the specified folder
  (setq org-download-annotate-function (lambda (link)   ; don't annotate screenshots, but annotate other images
                                         (if (equal link org-download-screenshot-file)   ; see the org-download source code
                                             ""
                                           (format "#+DOWNLOADED: %s @ %s\n" link (format-time-string "%Y-%m-%d %H:%M:%S"))))))




(use-package org-drill
  :after org
  ;; :commands (org-drill)
  :init
  (setq
   org-drill-spaced-repetition-algorithm  'sm2
   org-drill-learn-fraction                0.4
   ;; https://orgmode.org/worg/org-contrib/org-drill.html#text-org42a64b5
   ;; 0.4 means 5 times in the first month, and 10 times in five months

   org-drill-leech-method             'warn
   org-drill-leech-failure-threshold   5

   org-drill-maximum-duration                     nil
   org-drill-maximum-items-per-session            nil
   org-drill-add-random-noise-to-intervals-p      t
   org-drill-save-buffers-after-drill-sessions-p  nil
   org-drill-hide-item-headings-p                 nil
   org-drill-overdue-interval-factor              1.2
   org-drill-days-before-old                      10
   ;; org-drill-failure-quality                      2

   ;; org-drill-adjust-intervals-for-early-and-late-repetitions-p t    ; doesn't have any effect on sm2

   org-drill-scope 'file-no-restriction     ; https://orgmode.org/worg/org-contrib/org-drill.html#orgf1d69c8
   ;; TODO: write functions: ym-drill-file, ym-drill-math, ym-drill-eng, ym-drill-list
   ;; or within each file, set org-drill-scope to 'directory'

   ;; defaults
   ;; org-drill-days-before-old 7
   org-drill--help-key ?`   ; the key to the left of the 1 key
   )
  :config
  (defun org-drill-entry-empty-p () nil)   ; don't ignore "empty" cards -- https://emacs.stackexchange.com/questions/38440/make-org-drill-allow-reviewing-empty-cards/58568#58568
  )





;; ob-ipython is abandoned, apparently and unfortunately
(use-package jupyter
  :config
  ;; don't forget to run the following:
  ;; $ pip install ipykernel && python -m ipykernel install --user
  (setq org-babel-python-command "/Users/alex/.python_venv/default360/bin/python")
  )




(use-package ob-http)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (http . t)
   ;; (clojure . t)
   ;; (haskell . t)
   ;; (java . t)
   ;; (javascript . t)
   ;; (lisp . t)
   ;; (R . t)
   ;; (shell . t)
   ;; (sql . t)
   ;; (sqlite . t)
   ;; (typescript . t)     ; (use-package ob-typescript)
   ;; (mongo . t)     ; (use-package ob-mongo)
   ;; (jupyter . t)
   ))
;; (setq org-src-tab-acts-natively t)
;; (setq org-babel-min-lines-for-block-output 9999)   ;; this forces indenting results with colons, because I don't like how #+end_example is inserted at the beginning of line, not indented at all



(use-package org-recent-headings
  :config
  (defun org-recent-headings-my-aux-fn () nil)
  (add-to-list 'org-recent-headings-advise-functions #'org-recent-headings-my-aux-fn)
  (let ((repeat-interval 20))
    (if (boundp 'org-recent-headings-my-timer)
      (cancel-timer org-recent-headings-my-timer))
    (setq org-recent-headings-my-timer
	  (run-with-timer 0 repeat-interval #'org-recent-headings-my-aux-fn))
    )
  (org-recent-headings-mode)  ; this clears the list, unfortunately
  )


(use-package org-ql)

(use-package org-super-agenda
  :config
  (org-super-agenda-mode 1)
  )
