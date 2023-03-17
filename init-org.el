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
  ;; (setq org-refile-targets '(("~/werk" :maxlevel . 3)))
					; TODO: variable
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-preview-latex-image-directory ".ltximg/")

  (setq org-startup-folded nil)   ; nil means only fold the :PROPERTIES: drawer

  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-agenda-window-setup 'only-window)

  (setq org-agenda-sticky t)

  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-startup-with-inline-images t)

  ;; number of blank lines between trees when folded
  ;; default is 2
  ;; set to -1 to preserve all whitespace
  ;; mine is set to 0 to have more content on screen
  (setq org-cycle-separator-lines 0)


  (setq org-hide-leading-stars t)   ; customize the org-hide face for this, set it to light gray

  (setq   ; from https://emacs.stackexchange.com/questions/18877/how-to-indent-without-the-two-extra-spaces-at-the-beginning-of-code-blocks-in-or
   ;; org-src-fontify-natively t
   ;; org-src-window-setup 'current-window
   ;; org-src-strip-leading-and-trailing-blank-lines t
   org-src-preserve-indentation t
   ;; org-src-tab-acts-natively t
   )
  )
  


;; TODO: why can't I do (require 'org-checklist) without this?
(use-package org-contrib)





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
  (setq org-babel-python-command "~/.python_venv/python3.8/bin/python")
  )




;; there's also a package named quickrun, for those who don't use babel
(use-package ob-http
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (http . t)
     (shell . t)
     (sqlite . t)
     ;; (clojure . t)
     ;; (haskell . t)
     ;; (java . t)
     ;; (javascript . t)
     ;; (lisp . t)
     ;; (R . t)
     ;; (sql . t)
     ;; (typescript . t)     ; (use-package ob-typescript)
     ;; (mongo . t)     ; (use-package ob-mongo)
     ;; (jupyter . t)
     ))
  )
;; (setq org-src-tab-acts-natively t)
;; (setq org-babel-min-lines-for-block-output 9999)   ;; this forces indenting results with colons, because I don't like how #+end_example is inserted at the beginning of line, not indented at all



;; (use-package org-recent-headings
;;   :config
;;   (defun org-recent-headings--my-advised-aux-fn () nil)
;;   (defun org-recent-headings--my-major-mode-check-and-run-on-timer ()
;;     (if (eq major-mode 'clojure-mode)
;; 	(org-recent-headings--my-advised-aux-fn)
;;       ))
;;   (add-to-list 'org-recent-headings-advise-functions #'org-recent-headings--my-advised-aux-fn)
;;   (let ((repeat-interval 20))
;;     (if (boundp 'org-recent-headings-my-timer)
;;       (cancel-timer org-recent-headings-my-timer))
;;     (setq org-recent-headings-my-timer
;; 	  (run-with-timer 0 repeat-interval #'org-recent-headings--my-major-mode-check-and-run-on-timer))
;;     )
;;   (org-recent-headings-mode)  ; this clears the list, unfortunately
;;   )





;; (use-package ox
;;   :config
;;   ;; (setq org-export-preserve-breaks t)
;;   ;; or set #+OPTIONS: \n:t
;;   )




;; TODO: markdown-mode




;; strike-though text color in org-mode
(setq org-emphasis-alist         ; from https://stackoverflow.com/questions/13190195/org-mode-strike-through-color
      (cons '("+" '(:strike-through t :foreground "gray"))
            (cl-delete "+" org-emphasis-alist :key 'car :test 'equal)))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks t)
  :config
  (setq org-hide-emphasis-markers t)
  (defun org-insert-literal-character (c)       ; from https://stackoverflow.com/questions/15324852/how-do-i-escape-slash-in-org-mode/75398146#75398146
    "Insert a literal character at point."
    (interactive "cWhat character?")
    (insert ?\u200B c ?\u200B))
  ;; (define-key org-mode-map (kbd "C-c i l") 'org-insert-literal-character)
  )



;; taken from https://hungyi.net/posts/group-emacs-commands-single-undo/
;; (defmacro with-single-undo (&rest body)
;;   "Execute BODY as a single undo step."
;;   `(let ((marker (prepare-change-group)))
;;      (unwind-protect ,@body
;;        (undo-amalgamate-change-group marker))))

;; wrapping lines with +- and -+
;; spaces at the beginning of line is common in code
;; a space after the plus breaks the strike-through
;; hence minuses
(defun m/strike-through-org-region (beg end)
  (interactive "r")
  (when mark-active
    (save-match-data
      (save-excursion
        (replace-regexp "^\\(.*\\)$" "+-\\1-+" nil beg end)
        ))))



