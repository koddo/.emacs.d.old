(add-to-list 'default-frame-alist
             '(font . "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'super)
(setq browse-url-generic-program (executable-find "open")
      browse-url-generic-args '("-a" "firefox"))
(menu-bar-mode t)
(setq trash-directory "~/.Trash")

(load "/opt/local/share/emacs/site-lisp/ledger.el")


(defun open-file-with-os (filename)
  (shell-command (concat "open '" filename "'")))





