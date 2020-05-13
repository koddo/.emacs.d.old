(progn
  (set-face-attribute 'default nil :family "Monaco" :height 100)
  (set-fontset-font t 'cyrillic "Monaco")
  )
(setq mac-allow-anti-aliasing t)
(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'super
      ns-function-modifier 'hyper)  ; make Fn key do Hyper
(setq browse-url-generic-program (executable-find "open")
      browse-url-generic-args '("-a" "firefox"))
(menu-bar-mode t)
(setq trash-directory "~/.Trash")

;;(load "/opt/local/share/emacs/site-lisp/ledger.el")


(defun open-file-with-os (filename)   ; TODO: should be customized in 
  (shell-command (concat "open '" filename "'")))

(use-package exec-path-from-shell
  :config (progn
            ;; (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME"))
            (exec-path-from-shell-initialize)))

(use-package reveal-in-osx-finder)



