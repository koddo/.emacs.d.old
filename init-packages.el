;;; -*- lexical-binding: t; -*-

;; (use-package maxframe
;;   :init
;;   ;; (add-hook 'window-setup-hook 'maximize-frame t)
;;   )




(use-package diminish)
;; to rename minor modes see https://github.com/myrjola/diminish.el
;; to diminish a major mode, (setq mode-name "whatever") in the mode hook
;; e.g., (add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))




;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )




(use-package emojify
  :config
   ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
  )




;; https://github.com/emacsorphanage/popwin
(use-package popwin
  :config
  (popwin-mode 1))




;; ido
;; flx-ido
;; smex



;; (use-package ag)
;; (use-package projectile-ripgrep)
(use-package deadgrep)
;; (use-package emacs-wgrep)   ;; for editing grep buffer; deadgrep support -- https://github.com/mhayashi1120/Emacs-wgrep/pull/58

(use-package projectile
  :config
  (projectile-mode +1)
  (setq frame-title-format     ;; taken from https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  )




;; TODO: https://alhassy.github.io/org-special-block-extras/





;; https://github.com/emacsorphanage/helm-ag
