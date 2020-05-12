(setq user-emacs-directory "~/.emacs.d.new")
(setq package-user-dir "~/.emacs.d.packages")

(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; melpa, etc
(when
    (not (gnutls-available-p))
  (error "No gnutls available")
  )
(require 'package)
(setq package-archives   ;; taken from https://emacs.stackexchange.com/questions/2969/is-it-possible-to-use-both-melpa-and-melpa-stable-at-the-same-time
      '(("gnu elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")
        ;; ("melpa stable" . "https://stable.melpa.org/packages/")
        )
      package-archive-priorities
      '(("org"          . 20)
        ("melpa"        . 10)
        ("gnu elpa"     . 0)
        ;; ("melpa stable" . 10)
        ))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; litter goes to ~/.emacs.d/etc/ and ~/.emacs.d/var/ by default
(use-package no-littering)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
             :mode (("\\.org$" . org-mode))
             :ensure org-plus-contrib
             :config (progn
                       
                       ))


;; M-x list-packages
;; then U to mark available updates
;; x to upgrade
(use-package auto-package-update
  :config (progn
            (setq auto-package-update-interval 7)
            (setq auto-package-update-delete-old-versions t)
            (setq auto-package-update-hide-results nil)
            (setq auto-package-update-prompt-before-update t)
            ;; (auto-package-update-maybe)       ; I have a regular org todo for this
            ;; (auto-package-update-now)
            )
  )


;; (load-file "~/.emacs.d.new/init_emacs.el")
;; (load-file "~/.emacs.d.new/init_macos.el")
;; (load-file "~/.emacs.d.new/init_keybindings.el")









