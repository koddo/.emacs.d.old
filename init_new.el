(setq user-emacs-directory "~/.emacs.d.new")
(setq package-user-dir "~/.emacs-elpa")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; melpa, etc
(when
    (not (gnutls-available-p))
  (error "No gnutls available")
  )
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
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

;; M-x list-packages
;; then U to mark available updates
;; x to upgrade
(use-package auto-package-update
             :config
             (progn
               (setq auto-package-update-interval 7)
               (setq auto-package-update-delete-old-versions t)
               (setq auto-package-update-hide-results t)
               (setq auto-package-update-prompt-before-update t))
             (auto-package-update-maybe)
             ;; (auto-package-update-now)
             )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package org
             :mode (("\\.org$" . org-mode))
             :ensure org-plus-contrib
             :config
             (progn
               ;; config stuff
               ))


(load-file "~/.emacs.d.new/init_emacs.el")
(load-file "~/.emacs.d.new/init_macos.el")
(load-file "~/.emacs.d.new/init_keybindings.el")









