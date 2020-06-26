;;; -*- lexical-binding: t; -*-

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib

  :init
  ;; (require 'face-remap)
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
  (setq org-download-screenshot-method "screencapture -i %s"   ; TODO: move to preinit
        org-download-edit-cmd "open -a Krita %s"   ; TODO: move to preinit
        org-download-backend "wget \"%s\" -O \"%s\"")
  (setq-default org-download-heading-lvl nil)   ; don't take header text into account, just put everything into the specified folder
  (setq org-download-annotate-function (lambda (link)   ; don't annotate screenshots, but annotate other images
                                         (if (equal link org-download-screenshot-file)   ; see the org-download source code
                                             ""
                                           (format "#+DOWNLOADED: %s @ %s\n" link (format-time-string "%Y-%m-%d %H:%M:%S"))))))




(use-package org-drill
  :after org
  :commands (org-drill)
  :init
  (setq
   org-drill-spaced-repetition-algorithm  'sm2
   org-drill-learn-fraction                0.4

   org-drill-leech-method             'warn
   org-drill-leech-failure-threshold   5

   org-drill-maximum-duration                     nil
   org-drill-maximum-items-per-session            nil
   org-drill-add-random-noise-to-intervals-p      t
   org-drill-save-buffers-after-drill-sessions-p  nil

   org-drill-adjust-intervals-for-early-and-late-repetitions-p t    ; doesn't have any effect with sm2 though

   ;; org-drill-scope 'file     ; https://orgmode.org/worg/org-contrib/org-drill.html#orgf1d69c8

   ;; defaults
   ;; org-drill-days-before-old 7
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
   (jupyter . t)
   ))
(setq org-src-tab-acts-natively t)
(setq org-babel-min-lines-for-block-output 9999)   ;; this forces indenting results with colons, because I don't like how #+end_example is inserted at the beginning of line, not indented at all
