;;; init-dev.el ---                                  -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------


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


;; -------------------------------------------------------------------


(use-package treemacs)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)
(use-package lsp-treemacs)


;; -------------------------------------------------------------------



;; -------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode
