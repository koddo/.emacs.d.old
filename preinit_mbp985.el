;; (set-frame-font "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
(add-to-list 'default-frame-alist
             '(font . "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1"))
(setq mac-command-modifier 'control
      mac-option-modifier 'meta
      mac-control-modifier 'super)
(setq browse-url-generic-program (executable-find "open")
      browse-url-generic-args '("-a" "opera"))
(menu-bar-mode t)
(setq trash-directory "~/.Trash")

(load "/opt/local/share/emacs/site-lisp/ledger.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extview -- handling file associacions
;; (require 'extview)
;; (push '("\\.pdf$" . "xpdf %s") extview-application-associations)
;; (push '("\\.ps$" . "gv %s") extview-application-associations)
;; (push '("\\.djvu?$" . "djview %s") extview-application-associations)
;; (push '("\\.chm$" . "kchmviewer %s") extview-application-associations)
;; (push '("\\.mpg$\\|\\.mp4$\\|\\.qt$\\|\\.mov$\\|\\.wmv$\\|\\.avi$\\|\\.flv$" . "mplayer -fs -zoom -xy 1024 -framedrop -vo x11 %s") extview-application-associations)
;; (push '("\\.html?$" . "opera %s") extview-application-associations)
;; (push '("\\.jpg$" . "xnview %s") extview-application-associations)
;; (push '("\\.dvi$" . "xdvi  -paper a5 %s") extview-application-associations)

(defun open-file-with-os (filename)
  (shell-command (concat "open '" filename "'")))





