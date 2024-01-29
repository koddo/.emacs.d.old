;;; -*- lexical-binding: t; -*-

;; (use-package maxframe
;;   :init
;;   ;; (add-hook 'window-setup-hook 'maximize-frame t)
;;   )


;; -------------------------------------------------------------------




;; -------------------------------------------------------------------


;; functions of interest: undo-tree-visualize
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode)
  
  (progn
    (defun ym/suppress-message--undo-tree-undo (undo-tree-undo &rest args)
      (let ((message-log-max nil)              ; from https://emacs.stackexchange.com/questions/59942/is-it-possible-suppress-save-message-for-undo-tree
            (inhibit-message t))
        (apply undo-tree-undo args)))
    (advice-add 'undo-tree-undo :around 'ym/suppress-message--undo-tree-undo))
  ;; (progn
  ;;   (defun ym/suppress-message--undo-tree-save-history (undo-tree-save-history &rest args)
  ;;     (let ((message-log-max nil)              ; from https://emacs.stackexchange.com/questions/59942/is-it-possible-suppress-save-message-for-undo-tree
  ;;           (inhibit-message t))
  ;;       (apply undo-tree-save-history args)))
  ;;   (advice-add 'undo-tree-save-history :around 'ym/suppress-message--undo-tree-save-history))

  )

;; -------------------------------------------------------------------


(use-package emojify
  :config
  ;; (remove-hook 'emojify-inhibit-functions #'emojify-in-org-tags-p)
  )


;; -------------------------------------------------------------------


;; https://github.com/emacsorphanage/popwin
;; (use-package popwin
;;   :config
;;   (popwin-mode 1)
;;   )
;; it's buggy, leaves cursor in minibuffer

(use-package popper
  :config
  (popper-mode +1)
  (add-to-list 'popper-reference-buffers "\\*rg\\*")
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


;; (use-package smartparens
;;   :demand t
;;   :diminish smartparens-mode smartparens-global-mode show-smartparens-mode show-smartparens-global-mode
;;   :config
;;   (require 'smartparens-config)   ; default configuration
;;   (setq sp-navigate-reindent-after-up-in-string nil)
;;   (setq sp-navigate-reindent-after-up nil)
;;   ;; (smartparens-global-mode 1)     ; used to be (smartparens-global-strict-mode 1), but I don't need it to be that strict
;;   ;; (show-smartparens-global-mode 1)
  
;;   ;; customize sp-show-pair-match-content-face if you want to highlight not only parens but also the content of the s-exp
;;   ;; '(sp-show-pair-enclosing ((t (:inherit show-paren-match))))  
;;   )


;; (require 'paren)   ; I prefer stock show-paren-mode over show-smartparen-mode because it's ultra-fast
;; (setq show-paren-delay 0)
;; (setq show-paren-delay 0.1)
;; (setq show-paren-delay 0.01)
;; (show-paren-mode 1)
;; (show-paren-mode -1)
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


;; (use-package helpful
;;   :config
;;   (global-set-key (kbd "C-h f") #'helpful-callable)
;;   (global-set-key (kbd "C-h v") #'helpful-variable)
;;   (global-set-key (kbd "C-h k") #'helpful-key)
;;   (global-set-key (kbd "C-c C-d") #'helpful-at-point)
;;   (global-set-key (kbd "C-h F") #'helpful-function)
;;   (global-set-key (kbd "C-h C") #'helpful-command)
;;   )

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





(defun my/hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (if cmd
        (format "%s (%s) - %s" hint key-binding cmd)
      (format "%s (%s)" hint key-binding))))

(defun my/hydra-current-heads-as-candidates ()
  (let ((base (replace-regexp-in-string "/body$" "" (symbol-name hydra-curr-body-fn))))
    (mapcar (lambda (h)
              (cons (my/hydra-format-head h) (hydra--head-name h (intern base))))
            (symbol-value (intern (concat base "/heads"))))))

(defun my/hydra-execute-extended (prefixarg &optional command-name typed)
  (declare (interactive-only command-execute))
  (interactive (let ((execute-extended-command--last-typed nil)
                     (candidates (my/hydra-current-heads-as-candidates)))
                 (hydra-keyboard-quit)
                 (list current-prefix-arg
                       (completing-read "Cmd: " candidates)
                       execute-extended-command--last-typed)))
  (let* ((candidates (my/hydra-current-heads-as-candidates))
         (bind (assoc-default command-name candidates 'string=)))
    (cond
     ((null bind) nil)
     ((hydra--callablep bind) (call-interactively bind)))))

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "tab") #'my/hydra-execute-extended))

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



(setq projectile-switch-project-action #'projectile-dired)
(projectile-discover-projects-in-directory "~/workspaces")
;; projectile-add-known-project


;; (use-package perspective)
;; (use-package persp-mode)
;; (use-package persp-projectile)


;; -------------------------------------------------------------------


(use-package treemacs
  :config
  (setq treemacs-no-png-images t)
)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)

;; (use-package all-the-icons
;;   :if (display-graphic-p))
;; (use-package treemacs-all-the-icons
;;   :after treemacs
;;   )
;; (treemacs-load-theme "all-the-icons")
;; ;; (use-package lsp-treemacs)

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :config
;;   (setq mode-line-percent-position nil
;;         doom-modeline-workspace-name nil
;;         )
;;   )

;; (use-package doom-themes
;;   :config
;;   ;; (load-theme 'doom-one-light t)
;;   (load-theme 'doom-tomorrow-day t)
  
;;   )

;; (custom-set-faces
;;  `(fringe ((t (:background "grey90")))))



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
;; buf-move-up
;; buf-move-down
;; buf-move-left
;; buf-move-right

;; -------------------------------------------------------------------

;; python

;; traad, rope for refactoring
;; jedi

;; LSP?

;; -------------------------------------------------------------------

;; (use-package multiple-cursors)

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
         ?j ?k ?l
	     ?s ?d ?f     ; ?a -- looks too similar to ?d


         ?r ?e ?w          ; ?q -- I often can't distinguish q from g

	     ?v ?c ?x ?z       ; ?b -- o
	     ?m ;; ?n


         ;; ?1 ?2 ?3 ?4
         ;; ?5 ?6
         ;; ?7 ?8 ?9 ?0
	     
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
  ;; (setq avy-style 'de-bruijn)
  (setq avy-single-candidate-jump nil)
  ;; (setq avy-timeout-seconds 0.3)

  (add-to-list 'avy-orders-alist '(avy-goto-char-2 . avy-order-closest))
  ;; (add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))

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

  ;; https://emacs.stackexchange.com/questions/74840/hide-cursor-and-marks-in-all-windows-while-using-avy-or-ace-window/76656#76656
  ;; This snippet works under the assumption the cursor type is same everywhere. It uses setq-default for buffer-local variables cursor-in-non-selected-windows and cursor-in-non-selected-windows.
  ;; When the inactive cursor is on one of the avy targets,
  ;; the overlay and cursor colors mix and the avy sequence gets hard to read.
  (defun avy-jump-advice--hide-cursor-temporarily (orig-func &rest args)
    (let ((ct  cursor-type)
	      (ctn cursor-in-non-selected-windows))
      (setq-default cursor-in-non-selected-windows nil)
      (setq-default cursor-type nil)
      (apply orig-func args)
      (setq-default cursor-type ct)
      (setq-default cursor-in-non-selected-windows ctn)))
  (advice-add 'avy-jump :around #'avy-jump-advice--hide-cursor-temporarily)
  ;; just in case: (advice-remove 'avy-jump #'avy-jump-advice--hide-cursor-temporarily)


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


  (defun ym/magit-repolist-column--date-last-touched (_)
    "20230329"
    )
  ;; M-x tabulated-list-sort


  ;; (setq magit-git-debug nil)   ; useful for checking out the actual commands behind the views



  (add-to-list 'magit-repository-directories '("~/.emacs.d.old" . 0))
  (add-to-list 'magit-repository-directories '("~/.setuplets" . 0))
  (add-to-list 'magit-repository-directories '("~/werk" . 0))
  (add-to-list 'magit-repository-directories '("~/workspaces" . 1))
  ;; (setq magit-repository-directories nil)
  ;; (setq magit-git-debug nil)

  (setq magit-repolist-columns
        '(
          (" " 10 ym/magit-repolist-column--date-last-touched nil)
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
          ("Path" 300 magit-repolist-column-path nil)
          ))

  ;; (setq magit-repolist-column-flag-alist )

  )

;; -------------------------------------------------------------------

(use-package cider
  :config

  ;; This file is project local. Apparently, you don't need to set it in .dir-locals.el
  (setq cider-repl-history-file ".cider-repl-history")
  (setq cider-eldoc-display-context-dependent-info t)
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

;; install https://addons.mozilla.org/en-US/firefox/addon/ghosttext/
;; https://github.com/fregante/GhostText

(use-package atomic-chrome)
;; (atomic-chrome-start-server)

(use-package code-cells)
(use-package jupyter)

;; -------------------------------------------------------------------

(use-package web-mode)


;; -------------------------------------------------------------------

(use-package highlight)


;; -------------------------------------------------------------------

(use-package key-chord
  :config
  ;; (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.15)
  (setq key-chord-two-keys-delay 0.01)
  ;; (key-chord-define-global "qq"     "cool")
  ;; (key-chord-define-global "kl"     "cool")
  ;; (key-chord-define-global " a"     "cool")
  ;; (key-chord-define-global "((" 'sp-wrap-round)
  ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
  )

(use-package key-seq
  ;; (key-seq-define-global ";a" "cool")
  ;; (key-seq-define text-mode-map "qf" 'flyspell-buffer)
  ;; (key-chord-mode 1)
  )
;; https://www.johndcook.com/blog/2015/02/01/rare-bigrams/
;; https://www.reddit.com/r/emacs/comments/22hzx7/what_are_your_keychord_abbreviations/ 

;; btw, ordered chords: https://github.com/vlevit/key-seq.el
;; e.g., for ';j'

;; ("[f" . "{")
;; ("]f" . "}")



;; -------------------------------------------------------------------

(use-package copy-as-format)

;; -------------------------------------------------------------------
(use-package ignoramus)
(ignoramus-setup)
;; (ignoramus-setup '(pcomplete shell ido))


;; -------------------------------------------------------------------

(use-package olivetti)


;; -------------------------------------------------------------------

(tab-bar-mode)

;; instead of winner-mode
(tab-bar-history-mode)

;; tab-bar-history-back
;; tab-bar-history-forward


;; -------------------------------------------------------------------

;; (use-package minions)
(use-package manage-minor-mode-table)



;; -------------------------------------------------------------------

;; c++
(setq-default c-basic-offset 4)


;; (use-package eldoc
;;   :straight nil
;;   :diminish
;;   :config
;;   ;; (eldoc-mode -1)
;;   )

(use-package eldoc-box)

;; -------------------------------------------------------------------

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)

  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

 
;; (setq path-to-ctags "/opt/local/bin/ctags")

;; -------------------------------------------------------------------

(use-package ggtags

  )

;; -------------------------------------------------------------------

(use-package direnv
 :config
 (direnv-mode))


;; -------------------------------------------------------------------

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))


;; -------------------------------------------------------------------

(use-package puni
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode))

(electric-pair-mode t)





;; -------------------------------------------------------------------

(use-package minimap
  :config
  (setq minimap-recenter-type 'middle)
  )



;; -------------------------------------------------------------------

(use-package beacon
  ;; it highlights the cursor when you scroll, usefull for presentations
  )

;; -------------------------------------------------------------------


;; -------------------------------------------------------------------










