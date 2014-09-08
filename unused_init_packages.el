(require 'cl-lib)   ; TODO: move this line to init.el

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; TODO: after updating to 24.4 remove package-filter and replace this with package-pinned-packages var
(setq package-archive-enable-alist '(
				     ("gnu"
				      )
				     ("melpa-stable" 
				      autopair
				      diminish
				      ledger-mode
				      yasnippet
				      )
				     ("melpa"
				      dired+
				      dired-details
				      load-theme-buffer-local
				      )
				     ("marmalade"
				      )
				     ("org"
				      org-plus-contrib
				      )
)) ; (ym-install-packages)

(defun ym-install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (p 
	   (cl-reduce #'append (mapcar #'cdr package-archive-enable-alist)))
    (when (not (package-installed-p p))  ; should I check (assoc pkg package-archive-contents)?
      (package-install p))))
(unless (file-exists-p package-user-dir)   ; or maybe test package-archive-contents? -- first start of emacs after installation
  (package-initialize)
  (package-refresh-contents)
  (package-install 'package-filter)
  (ym-install-packages))

(progn
  (setq package-enable-at-startup nil)
  (package-initialize))



;; (defun package-list-unaccounted-packages ()
;;   "Like `package-list-packages', but shows only the packages that
;;   are installed and are not in `jpk-packages'.  Useful for
;;   cleaning out unwanted packages."
;;   (interactive)
;;   (package-show-package-list
;;    (remove-if-not (lambda (x) (and (not (memq x jpk-packages))
;;                             (not (package-built-in-p x))
;;                             (package-installed-p x)))
;;                   (mapcar 'car package-archive-contents))))
