;;; -*- lexical-binding: t; -*-

;; (use-package maxframe
;;   :init
;;   ;; (add-hook 'window-setup-hook 'maximize-frame t)
;;   )


;; -------------------------------------------------------------------


(use-package diminish)
;; to rename minor modes see https://github.com/myrjola/diminish.el
;; to diminish a major mode, (setq mode-name "whatever") in the mode hook
;; e.g., (add-hook 'lisp-mode-hook (lambda () (setq mode-name "λ")))


;; -------------------------------------------------------------------


;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )


;; -------------------------------------------------------------------


(use-package emojify
  :config
   ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
  )


;; -------------------------------------------------------------------


;; https://github.com/emacsorphanage/popwin
(use-package popwin
  :config
  (popwin-mode 1)
  )


;; -------------------------------------------------------------------

(use-package ido
  :config
  (setq
   ido-use-virtual-buffers t   ; keep a list of closed buffers
   ido-enable-flex-matching t
   ;; ido-use-faces nil   ; turned off for flx
   ido-default-buffer-method 'selected-window    ; do not switch frames if a buffer is opened -- http://ergoemacs.org/misc/emacs_ido_switch_window.html
   ido-auto-merge-work-directories-length -1     ; disable search for a file in other recent used directories -- https://stackoverflow.com/questions/17986194/emacs-disable-automatic-file-search-in-ido-mode
   )
  (ido-mode 1)
  (ido-everywhere 1)
  ;; I don't use flx-ido, because it doesn't take recency into account. C-space or toggling regex matching works great for me.
  )
(use-package ido-grid-mode
  :config
  ;; (setq ido-grid-mode-start-collapsed t)
  )
;; C-h f, while Amx is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command. (Via where-is.)
;; there is also amx-major-mode-commands
(use-package amx   ; smex successor, for M-x
  :config
  (setq
   amx-ignored-command-matchers nil
   ido-cr+-max-items 50000   ; default is 30000
   amx-show-key-bindings nil
   ;; and set amx-save-file to do-not-litter
   )
  (amx-mode 1)
  (add-to-list 'warning-suppress-types '(amx))
  ;;; I used to have the following to update smex cache
  ;; (defun ym-smex-update-after-load-file (unused)
  ;;   (when (boundp 'smex-cache)
  ;;     (smex-update)))
  ;; (add-hook 'after-load-functions 'ym-smex-update-after-load-file)   ; see init_keybindings.el
  )
(use-package ido-completing-read+   ; enhanced ido-everywhere
  :config
  (ido-ubiquitous-mode 1)
  )
(use-package ido-yes-or-no
  :config
  (ido-yes-or-no-mode)
  )

;;; tried ido-vertical-mode, but didn't like it
;; (use-package ido-vertical-mode
;;   :config
;;   (ido-vertical-mode 0)
;; )

;; (add-to-list 'ido-ignore-buffers "\\` ")
(add-to-list 'ido-ignore-buffers "\\*Ido Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Ibuffer\\*")
(add-to-list 'ido-ignore-buffers "\\*Messages\\*")
(add-to-list 'ido-ignore-buffers "\\*Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Scratch\\*")
(add-to-list 'ido-ignore-buffers "\\*Help\\*")
(setq ido-use-filename-at-point 'guess)   ;; nil, guess and t for literal filename
(setq ido-use-url-at-point t)
(setq ido-file-extensions-order '(".org" ".md"))
;; (setq ido-enter-matching-directory nil)
;; (setq ido-show-dot-for-dired t)
;; (setq ido-enable-tramp-completion nil)
;; (setq ido-max-prospects 7)

(use-package ido-sort-mtime
  :config
  (setq
   ido-sort-mtime-limit 2000
   ;; ido-sort-mtime-tramp-files-at-end nil
   )
  )

;; -------------------------------------------------------------------



;; TODO: https://alhassy.github.io/org-special-block-extras/





;; -------------------------------------------------------------------



;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

(use-package expand-region
  )

;; TODO: add expand-region package to hydra


;; -------------------------------------------------------------------


(use-package smartparens
  :config
  (require 'smartparens-config)   ; default configuration
  (require 'paren)   ; I prefer stock show-paren-mode over show-smartparen-mode because it's ultra-fast
  (setq show-paren-delay 0)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis)

  ;; https://stackoverflow.com/questions/34846531/show-parentheses-when-inside-them-emacs
  ;; (define-advice show-paren-function (:around (fn) fix)
  ;;   "Highlight enclosing parens."
  ;;   (cond ((looking-at-p "\\s(") (funcall fn))
  ;; 	  (t (save-excursion
  ;; 	       (ignore-errors (backward-up-list))
  ;; 	       (funcall fn)))))
  ;; (advice-remove 'show-paren-function 'fix)


  :bind (:map smartparens-mode-map

	      ("s-n" . sp-backward-up-sexp)
	      ("s-," . (lambda () (interactive) (sp-backward-sexp)))
	      ("s-." . (lambda () (interactive) (sp-forward-sexp 2) (sp-backward-sexp)))
	      ("s-m" . (lambda () (interactive)
	      		 (let ((end-of-thing    (sp-get (sp-get-thing) :end)))
	      		   (if (> end-of-thing (point))
	      		       (goto-char end-of-thing))
			   )))

	      ("H-a" . sp-splice-sexp)
	      ("H-s" . sp-splice-sexp-killing-forward)
	      ("H-d" . sp-splice-sexp-killing-backward)
	      ("H-f" . sp-splice-sexp-killing-around)

  ))



;; https://github.com/Fuco1/smartparens/wiki/Working-with-expressions

;; cheatsheet
;; if you realized you had a let form inside an if and realized you needed those bindings in the other branch of the if form as well.
;; (if a (let [foo bar] b c))


	      ;; ("s-," . sp-backward-parallel-sexp)
	      ;; ("s-." . (lambda () (interactive)
	      ;; 		 (let ((current (sp-get-thing)))
	      ;; 		   (goto-char (sp-get current :end))
	      ;; 		   (sp-forward-parallel-sexp)
	      ;; 		   (let ((next (sp-get-thing 'before)))
	      ;; 		       (goto-char (sp-get next :beg))
	      ;; 		     )
	      ;; 		   )
	      ;; 		 ))


	      ;; , sp-select-previous-thing

	      ;; ("H-o" . sp-unwrap-sexp)
	      ;; ("H-m" . sp-select-next-thing-exchange)
	      ;; ("H-n" . sp-select-previous-thing)

	      ;; select forward, backward
	      ;;




;; -------------------------------------------------------------------

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  )

;; -------------------------------------------------------------------

(use-package hydra
  :config
  ;; (setq sp-successive-kill-preserve-whitespace 1)   ; default is 1, https://github.com/Fuco1/smartparens/issues/197
  )
(use-package pretty-hydra)

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------


;; (use-package ag)
;; (use-package projectile-ripgrep)
;; (use-package deadgrep)
;; (use-package emacs-wgrep)   ;; for editing grep buffer; deadgrep support -- https://github.com/mhayashi1120/Emacs-wgrep/pull/58

(use-package rg
  :config
  (rg-enable-menu)

  ;; https://rgel.readthedocs.io/en/latest/configuration.html

  ;; https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#configuration-file
  )


(use-package projectile
  :config
  (projectile-mode +1)
  (setq frame-title-format     ;; taken from https://emacs.stackexchange.com/questions/35432/how-to-set-projectile-project-name-as-frame-title
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))

  ;; monkey-patching
  ;; because originally removes current buffer from suggestions
  ;; TODO: pull request for code with an additional option for this
  ;; (defun projectile-read-buffer-to-switch (prompt)
  ;;   (projectile-completing-read
  ;;    prompt
  ;;    (projectile-project-buffer-names)))

  )


(let* ((project-root (projectile-project-root))
       (prj-buffers (delete (buffer-name (current-buffer))
			    (projectile-project-buffer-names)))
       (virtual-buffers (cl-remove-if-not (lambda (bufpair)
					    (interactive)
					    (projectile-verify-file (cdr bufpair))
					    )
					  ido-virtual-buffers))
       (buflist (append prj-buffers virtual-buffers)))
  (find-file (ido-completing-read+ "asdf: " buflist))

  ;; TODO: remove duplicates in buflist
)

;; -------------------------------------------------------------------


(use-package treemacs)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)
(use-package lsp-treemacs)


;; -------------------------------------------------------------------

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	(list (concat
	       (file-name-as-directory user-emacs-directory)
	       "yasnippets")))
  (yas-reload-all)

  ;; TODO: bind yas-expand

  ;; there's also grep-edit.el, which could be used to modify snippets in its buffer, instead of doing C-x C-f at their paths here, but the difference is not worth it at the moment
  (cl-defun ym-list-all-yasnippets (&optional (dirs yas-snippet-dirs))
    (interactive)
    (switch-to-buffer (get-buffer-create "*yasnippets*"))
    (erase-buffer)
    (dolist (dir dirs)
      (dolist (filename (directory-files-recursively dir "" nil))
	(insert "---------------------------------------------------------")
	(newline)
	(insert filename)
	(newline)
	(insert-file-contents filename)
	(end-of-buffer)   ; any better way to jump after the insertion?
	(newline)
	(newline)
	(newline)
	(newline)
	))
    (beginning-of-buffer)
    (view-mode)
    )
  (defun ym-list-all-yasnippets-official ()
    (interactive)
    (ym-list-all-yasnippets '("~/.emacs.d.new/straight/repos/yasnippet-snippets/snippets/"))
    )
  )

;; -------------------------------------------------------------------

(use-package company
  :config
  (add-to-list
   'company-backends 'company-yasnippet)
  (defun ym-adfadf () (interactive) (company-abort) (company-begin-backend 'company-yasnippet))
  (setq company-minimum-prefix-length 3)

  ;; FIXME: (low) company-yasnippet doesn't work for me, figure out why
  )

(use-package company-quickhelp
  :config
  (company-quickhelp-mode))



;; I use my own set of snippets instead of the official one
;; (use-package yasnippet-snippets)

;; -------------------------------------------------------------------

;; https://github.com/emacs-lsp/lsp-mode

;; -------------------------------------------------------------------

(use-package buffer-move)

;; -------------------------------------------------------------------

;; python

;; traad, rope for refactoring
;; jedi

;; LSP?

;; -------------------------------------------------------------------

(use-package multiple-cursors)

;; -------------------------------------------------------------------

(use-package drag-stuff)

;; -------------------------------------------------------------------
