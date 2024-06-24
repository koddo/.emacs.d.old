




(load-file (expand-file-name "init-org.el" user-emacs-directory))
(load-file (expand-file-name "init-packages.el" user-emacs-directory))
(load-file (expand-file-name "init-agenda.el" user-emacs-directory))
(load-file (expand-file-name "init-habits.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-3-ym-keys-minor-mode.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-1.el" user-emacs-directory))
;; (load-file (expand-file-name "init-keybindings-2-russian-keyboard.el" user-emacs-directory))
(load-file (expand-file-name "init-keybindings-4.el" user-emacs-directory))
(load-file (expand-file-name "init-colors.el" user-emacs-directory))

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq custom-file (expand-file-name "init-customize.el" user-emacs-directory))
(load custom-file 'noerror)


;; (find-file "~/drill/drill.org")
;; (find-file "~/workspace/Notes.org")
