;;; init_new_straight.el ---                         -*- lexical-binding: t; -*-




;; TODO: remove this
;; or use this: https://emacs.stackexchange.com/questions/19936/running-spacemacs-alongside-regular-emacs-how-to-keep-a-separate-emacs-d/20508#20508
(setq user-emacs-directory "~/.emacs.d.new")
(setq package-user-dir "~/.emacs.d.packages")











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





(load-file (expand-file-name "init__preinit_this_machine.el" user-emacs-directory))
;; (load-file (expand-file-name "init_keybindings.el" user-emacs-directory))

;; taken from https://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab/2253044#2253044
(keyboard-translate ?\C-i ?\H-i)
(keyboard-translate ?\C-m ?\H-m)
(keyboard-translate 27 16777307)    ; C-[ is 27 and H-[ is 16777307
;; Rebind then accordantly: (global-set-key (kbd "H-i") (lambda () (interactive) (message "C-i hello")))



;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-file (expand-file-name "init_custom.el" user-emacs-directory))
(load custom-file 'noerror)








;; (load-file "~/.emacs.d/init_emacs.el")
;; (load-file "~/.emacs.d/init_org.el")
;; (load-file "~/.emacs.d/init_development.el")
;; (load-file "~/.emacs.d/init_functions.el")
;; (load-file "~/.emacs.d/init_colors.el")

;; (dired "~/")
;; (dired "~/Downloads")
;; (dired "~/workspace")
