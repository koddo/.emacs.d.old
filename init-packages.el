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
(setq ido-show-dot-for-dired t)
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
  :demand t
  
  :config
  (require 'smartparens-config)   ; default configuration
  (setq sp-navigate-reindent-after-up-in-string nil)
  (setq sp-navigate-reindent-after-up nil)
  (smartparens-global-mode 1)     ; used to be (smartparens-global-strict-mode 1), but I don't need it to be that strict
  (show-smartparens-global-mode 1)
  
  ;; customize sp-show-pair-match-content-face if you want to highlight not only parens but also the content of the s-exp
  ;; '(sp-show-pair-enclosing ((t (:inherit show-paren-match))))  
  )


;; (require 'paren)   ; I prefer stock show-paren-mode over show-smartparen-mode because it's ultra-fast
;; (setq show-paren-delay 0.1)
;; (show-paren-mode t)
;; (setq show-paren-style 'parenthesis)

;; (progn (require 'autopair)   ; insert paired parenthesis
;;      (autopair-global-mode)
;;      (setq autopair-blink nil)
;;      (setq autopair-skip-whitespace 'chomp))   ; ) ) => )) when closing
;; here's a modified snippet from https://stackoverflow.com/questions/34846531/show-parentheses-when-inside-them-emacs/34861578#34861578
;; rename the advice and make it toggleable through hydra
;; (define-advice show-paren-function (:around (fn) fix)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))      ; \s( and \s) are open and close delimiter character
;; 	  ((save-match-data (looking-back "\\s)" 1)) (funcall fn))   ; if performance is an issue, replace looking-back with char-before and 
;; 	  (t (save-excursion
;; 	       (ignore-errors (backward-up-list))
;; 	       (funcall fn)))))

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
(use-package major-mode-hydra
  ;; includes pretty-hydra

  :config
  ;; https://github.com/jerrypnz/major-mode-hydra.el
  )

;; (use-package posframe)
;; (use-package hydra-posframe
;;   :straight (:host github :repo "Ladicle/hydra-posframe")
;;   :hook (after-init . hydra-posframe-enable)
;;   )

;; completion for hydras:
;; https://sachachua.com/blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/
;; https://sachachua.com/dotemacs/index.html#hydra-completion

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


;; (let* ((project-root (projectile-project-root))
;;        (prj-buffers (delete (buffer-name (current-buffer))
;; 			    (projectile-project-buffer-names)))
;;        (virtual-buffers (cl-remove-if-not (lambda (bufpair)
;; 					    (interactive)
;; 					    (projectile-verify-file (cdr bufpair))
;; 					    )
;; 					  ido-virtual-buffers))
;;        (buflist (append prj-buffers virtual-buffers)))
;;   (find-file (ido-completing-read+ "asdf: " buflist))

;; TODO: remove duplicates in buflist
;; )

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

(use-package avy
  :config

  (defun avy-goto-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "[][{}()]+")   ; any sequence of parens, brackets, curly braces        ; was (avy-jump "\\[+\\|\\]+\\|(+\\|)+\\|{+\\|}+")
      ))
  


  
  (add-to-list 'avy-orders-alist '(avy-goto-parens . avy-order-closest))
  ;; see keybindings.el

  (setq avy-keys
	(list
	 ?r ?e ?w          ; ?q -- I often can't distinguish q from g
	 ?v ?c ?x ?z       ; ?b -- o
	 ?m ;; ?n


	 ?j ?k ?l

	 ?s ?d ?f     ; ?a -- looks too similar to ?d
	 
	 ; ?u -- it's fine, but hard to reach after the semicolon         ; ?i=?l  ; ?o -- similar to a   ;; ?p -- vertical line is not visible enough
	 ; ?h -- hard to reach          ; ?g -- similar to a
	 ; ?y ?t -- hard to reach

	 ; ?, ?. -- these don't work for some reason
	 )
  	)

  (setq avy-background nil)
  (setq avy-highlight-first nil)
  (setq avy-all-windows t) ; 'all-frames
  (setq avy-style 'at-full)
  (setq avy-single-candidate-jump nil)

  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))
  (add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))

  (dolist (x '(avy-lead-face
	       avy-lead-face-0
	       avy-lead-face-1
	       avy-lead-face-2))
    (set-face-attribute x nil :foreground "white" :background "#dc9656"))
  ;; (set-face-attribute 'avy-background-face nil :foreground "grey90" :background "grey98")

  ;; https://github.com/abo-abo/avy/issues/268
  ;; TODO:this should be a pull request with adding an option for excluding current point
  (defun avy-jump-advice-exclude-current-point (orig-func &rest args)
    (let* ((between-inclusive (lambda (val low high) (and (<= low val) (<= val high))))
	   (current-point (point))
	   (oldpred (plist-get :pred args))
	   (pred (lambda ()
		   (and
		    (let ((candidate (point))) (or (< candidate current-point) (> candidate (+ 3 current-point))))   ; +3 should be enough: with avy-goto-word-1 it's +1, with avy-goto-char-2 it's +2
		    (or (null oldpred) (funcall oldpred))))))
      (apply orig-func (append args (list :pred pred)))))
  (advice-add 'avy-jump :around #'avy-jump-advice-exclude-current-point)

  ;; When I have multiple windows, the cursor in other windows is hollow,
  ;; which means that if it's on one of the targets, they both become white and I can't read the letter.
  ;; This fix makes all cursors filled temporarily.
  (defun avy-jump-advice-cursor-background-fix (orig-func &rest args)
    (let ((old-color (face-attribute 'cursor :background))
	  (old-type cursor-in-non-selected-windows))
      (set-cursor-color "white")
      (setq-default cursor-in-non-selected-windows 'box)
      (apply orig-func args)
      (setq-default cursor-in-non-selected-windows old-type)
      (set-cursor-color old-color)))
  (advice-add 'avy-jump :around #'avy-jump-advice-cursor-background-fix)   ; just in case: (advice-remove 'avy-jump #'avy-jump-cursor-background-fix-advice)

  ;; (defun avy-subdiv (n b)
  ;; "Distribute N in B terms in a balanced way."
  ;; (let* ((p (1- (floor (+ (log n b) 1e-6))))
  ;;        (x1 (expt b p))
  ;;        (x2 (* b x1))
  ;;        (delta (- n x2))
  ;;        (n2 (/ delta (- x2 x1)))
  ;;        (n1 (- b n2 1)))
  ;;   (append
  ;;    (make-list n1 x1)
  ;;    (make-list n2 x2)   ; originally this goes last, but I'd like the heaviest subtree to be bound to the first key in the list
  ;;    (list
  ;;     (- n (* n1 x1) (* n2 x2)))
  ;;    )))

  ;; (defun avy-tree (lst keys)   ; https://github.com/abo-abo/avy/issues/164#issuecomment-631785903
  ;;   "Coerce LST into a balanced tree.
  ;;    The degree of the tree is the length of KEYS.
  ;;    KEYS are placed appropriately on internal nodes."
  ;;   (let* ((len (length keys))
  ;; 	   (order-fn (cdr (assq avy-command avy-orders-alist)))
  ;; 	   (lst (if order-fn
  ;; 		    (cl-sort lst #'< :key order-fn)
  ;; 		  lst)))
  ;;     (cl-labels
  ;; 	  ((rd (ls)
  ;; 	       (let ((ln (length ls)))
  ;; 		 (if (< ln len)
  ;; 		     (cl-pairlis keys
  ;; 				 (mapcar (lambda (x) (cons 'leaf x)) ls))
  ;; 		   (let* ((ks (copy-sequence keys))
  ;; 			  (subdiv (avy-subdiv ln len))
  ;; 			  (number-of-ones (cl-count 1 subdiv))
  ;; 			  (number-of-non-ones (- len number-of-ones))
  ;; 			  res)
  ;; 		     (dolist (s subdiv)
  ;; 		       (push (cons (pop (if (eq s 1)
  ;; 					    (nthcdr number-of-non-ones ks)
  ;; 					  ks)
  ;; 					)
  ;; 				   (if (eq s 1)
  ;; 				       (cons 'leaf (pop ls))
  ;; 				     (rd (avy-multipop ls s))))
  ;; 			     res))
  ;; 		     (nreverse res))))))
  ;; 	(rd lst))))
  )

;; -------------------------------------------------------------------

(use-package bm
  :demand t
  :config


  ;; TODO: configure bm
  ;; https://readingworldmagazine.com/emacs/2019-10-20-emacs-bookmarks/
  ;; https://github.com/joodland/bm
  )

;; -------------------------------------------------------------------

(defun m/dot-emacs-reload-init-file ()
  (interactive)
  (ym-load-path)
  (load-file "~/.emacs.d/init.el"))

;; -------------------------------------------------------------------

;; disable version control enabled by default, it slows down emacs, and i don't use it
;; this probably breaks some functions like vc-annotate and vc-diff
;; TODO: read the faq, disabling the built-in version control is no longer recommended
;; https://magit.vc/manual/magit/Should-I-disable-VC_003f.html


(setq vc-handled-backends nil)

(use-package compat)    ; temporarily here, see https://github.com/magit/magit/issues/4836

(use-package magit
  :config


  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer)

  ;; https://magit.vc/manual/magit/Action-Confirmation.html
  ;; stage-all-changes
  
(defun kisaragi/magit-log-visit-changed-file ()
  "Visit a changed file of revision under point in `magit-log-mode'.

Uses `general-simulate-key', so `general-simulate-RET' will
become defined after invocation."
  (interactive)
  (general-simulate-key "RET")
  ;; visit the commit
  (general-simulate-RET)
  ;; move to first changed file in diff buffer
  (setf (point) (point-min))
  (search-forward "|" nil t)
  ;; open the revision
  (general-simulate-RET))



  ;; (setq magit-git-debug nil)   ; useful for checking out the actual commands behind the views

  )


(add-to-list 'magit-repository-directories '("~/.emacs.d.old" . 0))
(add-to-list 'magit-repository-directories '("~/werk" . 0))
(add-to-list 'magit-repository-directories '("~/workspaces" . 1))
;; (setq magit-repository-directories nil)
;; (setq magit-git-debug nil)

(setq magit-repolist-columns
      '(
        ;; ("Name" 25 magit-repolist-column-ident nil)
        ("Version" 30 magit-repolist-column-version
         ((:sort magit-repolist-version<)))
        ("B<U" 3 magit-repolist-column-unpulled-from-upstream
         (
          ;; (:right-align t)
          (:sort <)))
        ("B>U" 3 magit-repolist-column-unpushed-to-upstream
         (
          ;; (:right-align t)
          (:sort <)))
        (" " 3 magit-repolist-column-flag nil)
        (" " 12 magit-repolist-column-branch ((:right-align t)))
        ("Path" 300 magit-repolist-column-path nil))
      )

;; (setq magit-repolist-column-flag-alist )

;; -------------------------------------------------------------------

(use-package cider
  :config

  ;; This file is project local. Apparently, you don't need to set it in .dir-locals.el
  (setq cider-repl-history-file ".cider-repl-history")
  )


;; -------------------------------------------------------------------

(use-package git-gutter
  :config
  ;; (add-hook 'ruby-mode-hook 'git-gutter-mode)
  ;; (add-hook 'python-mode-hook 'git-gutter-mode)

  :custom
  (git-gutter:update-interval 1)         ; the default is 0, which means update on file save
  (git-gutter:lighter " gg")
  (git-gutter:ask-p nil)    ; revert hunks without confirmation, this is safe, as there's undo

  ;; (custom-set-variables
  ;;  '(git-gutter:modified-sign "  ") ;; two space
  ;;  '(git-gutter:added-sign "++")    ;; multiple character is OK
  ;;  '(git-gutter:deleted-sign "--"))
  ;; (set-face-background 'git-gutter:modified "purple") ;; background color
  ;; (set-face-foreground 'git-gutter:added "green")
  ;; (set-face-foreground 'git-gutter:deleted "red")
  )

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [#b00111100] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b01100110] nil nil '(center repeated))    ; can also be [#b01100110 #b00000000]
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b00111100] nil nil '(center repeated))
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:modified "purple")
  (set-face-foreground 'git-gutter-fr:deleted  "red")

  ;; bitmaps can be drawn this way:
  ;; (fringe-helper-define 'git-gutter-fr:added nil
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "XXXXXXXX"
  ;;                     "XXXXXXXX"
  ;;                     "...XX..."
  ;;                     "...XX..."
  ;;                     "...XX...")
  )

;; -------------------------------------------------------------------

(use-package nix-mode)


;; -------------------------------------------------------------------





