;;; -*- lexical-binding: t; -*-

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (setq-default org-adapt-indentation nil)
  )

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
  (defun org-drill-entry-empty-p () nil)   ; https://emacs.stackexchange.com/questions/38440/make-org-drill-allow-reviewing-empty-cards/58568#58568
  )


