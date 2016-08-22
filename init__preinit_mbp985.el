(progn
  (set-face-attribute 'default nil :family "Monaco" :height 100)
  (set-fontset-font t 'cyrillic "Monaco")
  )
(setq mac-allow-anti-aliasing t)
(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'super)
(setq browse-url-generic-program (executable-find "open")
      browse-url-generic-args '("-a" "firefox"))
(menu-bar-mode t)
(setq trash-directory "~/.Trash")

(load "/opt/local/share/emacs/site-lisp/ledger.el")


(defun open-file-with-os (filename)   ; TODO: why?
  (shell-command (concat "open '" filename "'")))

(require 'exec-path-from-shell)   ; TODO: move to init.el after checking it on linux
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME"))
(exec-path-from-shell-initialize)   ; exec-path-from-shell-variables






