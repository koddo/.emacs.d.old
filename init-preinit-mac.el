(set-face-attribute 'default nil :family "Monaco" :height 100)

(menu-bar-mode -1)

(setq
 org-download-screenshot-method "screencapture -i %s"
 org-download-edit-cmd "open -a Krita %s"
 org-download-backend "wget \"%s\" -O \"%s\"")






;;; from config for mpb985
;; (progn
;;   (set-face-attribute 'default nil :family "Monaco" :height 100)
;;   (set-fontset-font t 'cyrillic "Monaco")
;;   )
;; (setq mac-allow-anti-aliasing t)
;; (setq
;;  mac-command-modifier 'super
;;  mac-option-modifier 'meta
;;  mac-control-modifier 'control
;;  ns-function-modifier 'hyper      ; make Fn key do Hyper
;;  )
;; (setq browse-url-generic-program (executable-find "open")
;;       browse-url-generic-args '("-a" "firefox"))
;; (menu-bar-mode t)
;; (setq trash-directory "~/.Trash")

;; ;;(load "/opt/local/share/emacs/site-lisp/ledger.el")


;; (defun open-file-with-os (filename)   ; TODO: should be customized in
;;   (shell-command (concat "open '" filename "'")))

;; (use-package exec-path-from-shell
;;   :config (progn
;;             ;; (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME"))
;;             (exec-path-from-shell-initialize)))

;; (use-package reveal-in-osx-finder)
