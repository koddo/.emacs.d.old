;;; init-new.el ---                         -*- lexical-binding: t; -*-




;; TODO: remove this
;; or use this: https://emacs.stackexchange.com/questions/19936/running-spacemacs-alongside-regular-emacs-how-to-keep-a-separate-emacs-d/20508#20508
;(setq user-emacs-directory "~/.emacs.d.new")
;(setq package-user-dir "~/.emacs.d.packages")











(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)   ; to avoid putting ":straight t" everywhere

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "no-littering-etc/" user-emacs-directory))
  (setq no-littering-var-directory (expand-file-name "no-littering-var/" user-emacs-directory)))




(load-file (expand-file-name "init-preinit-softlink.el" user-emacs-directory))

(load-file (expand-file-name "init-emacs.el" user-emacs-directory))
(load-file (expand-file-name "init-packages.el" user-emacs-directory))
(load-file (expand-file-name "init-org.el" user-emacs-directory))
(load-file (expand-file-name "init-agenda.el" user-emacs-directory))
(load-file (expand-file-name "init-habits.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-3-ym-keys-minor-mode.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-1.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-2-russian-keyboard.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-4.el" user-emacs-directory))
(load-file (expand-file-name "init-colors.el" user-emacs-directory))

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-file (expand-file-name "init-customize.el" user-emacs-directory))
(load custom-file 'noerror)


;; (find-file "~/drill/drill.org")
;; (find-file "~/workspace/Notes.org")
