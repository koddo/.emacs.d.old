;;; -*- lexical-binding: t; -*-

;; (use-package maxframe
;;   :init
;;   ;; (add-hook 'window-setup-hook 'maximize-frame t)
;;   )



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

;; -------------------------------------------------------------------

;; completion for hydras
;; this is my slightly modified version of Sacha Chua's code
;; made completions look like "f: follow mode"

;; https://sachachua.com/blog/2021/04/emacs-hydra-allow-completion-when-i-can-t-remember-the-command-name/
;; https://sachachua.com/dotemacs/index.html#hydra-completion
;; https://www.reddit.com/r/emacs/comments/123l17j/completions_of_functions_in_hydra_when_you_forget/

(defun my/hydra-format-head (h)
  (let ((key-binding (elt h 0))
        (hint (elt h 2))
        (cmd (and (elt h 1) (prin1-to-string (elt h 1)))))
    (format "%s: %s" key-binding hint)
    ;; (if cmd
    ;;     (format "%s (%s) - %s" hint key-binding cmd)
    ;;   (format "%s (%s)" hint key-binding))
    ))

(defun my/hydra-current-heads-as-candidates ()
  (let* ((base (replace-regexp-in-string "/body$" "" (symbol-name hydra-curr-body-fn)))
         (heads-plist (symbol-value (intern (concat base "/heads-plist"))))
         (heads-plist-values (cl-loop for (key value) on heads-plist by 'cddr collect value))
         (heads (apply #'append heads-plist-values)))
    (mapcar (lambda (h)
              (cons (my/hydra-format-head h) (hydra--head-name h (intern base))))
            heads)))      ; fixed: used to be (symbol-value (intern (concat base "/heads"))), but instead of /heads-plisp it somehow doesn't contain hints, they all are nil

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
  (define-key hydra-base-map (kbd "<tab>") #'my/hydra-execute-extended))

;; -------------------------------------------------------------------

;; jump to definition of hydra

(defun ym/go-to-definition-of-hydra ()
  (interactive)
  (hydra-keyboard-quit)
  (find-function hydra-curr-body-fn)
  )

(with-eval-after-load 'hydra
  (define-key hydra-base-map (kbd "M-s-u") #'ym/go-to-definition-of-hydra))

;; -------------------------------------------------------------------

;; from https://github.com/abo-abo/hydra/issues/268
;; delay showing of hydra while when we do something like navigating windows or moving buffers
;; without this code the hydra is shown after the very first keypress

;; usage: ("j" (csb-wrap-ignore-error 'user-error (windmove-left)) "windmove-left")

(defun timer-reset (timer-sym secs fun)
  (let ((timer (and (boundp timer-sym) (symbol-value timer-sym))))
    (if (timerp timer)
        (cancel-timer timer)
      (setq timer (set timer-sym (timer-create))))
    (timer-set-time
     timer
     (timer-relative-time (current-time) secs))
    (timer-set-function timer fun)
    (timer-activate timer)))

(defun csb-hide ()
  ;; (hydra-disable)
  (hydra-set-property 'hydra-window :verbosity 0)
  ;; (hydra-window/body)
  )

(defun csb-show ()
  (hydra-set-property 'hydra-window :verbosity t)      ; 1 is for terminal emacs, otherwise t; see the hydra-show-hint function definition
  (let ((hydra-active (eq hydra-curr-map hydra-window/keymap))
        (f (timer--function hydra-message-timer))
        (a (timer--args hydra-message-timer))
        )
    (when hydra-active
      (hydra-window/body)
      (progn   ; show hydra immediately, without the idle delay, because we already waited for the moment to show
       (cancel-timer hydra-message-timer)
       (apply f a))
      )))

(defmacro csb-wrap (&rest body)
  `(progn
     ,@body
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)
     ))

(defmacro csb-wrap-ignore-error (condition &rest body)
  `(progn
     (ignore-error ,condition ,@body)
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)))

(defmacro csb-wrap-ignore-all-errors (&rest body)
  `(progn
     (ignore-errors ,@body)
     (csb-hide)
     (timer-reset 'csb-timer 0.7 'csb-show)))

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


;; (use-package treemacs
;;   :config
;;   (setq treemacs-no-png-images t)
;;   )
;; (use-package treemacs-projectile
;;   :after treemacs projectile)
;; (use-package treemacs-magit
;;   :after treemacs magit)

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

  ;; unbind tab -- I use yas-insert-snippet, not expand
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (yas-global-mode 1)

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

(use-package markdown-mode)

;; -------------------------------------------------------------------

;; python

;; traad, rope for refactoring
;; jedi

;; LSP?


;; display plots and images in ipython
;; https://github.com/astoff/comint-mime


;; polymode for markdown and python is somewhat similar to org-mode+babel for everything else
(use-package polymode)
(use-package poly-markdown)

;; https://www.masteringemacs.org/article/polymode-multiple-major-modes-how-to-use-sql-python-in-one-buffer
;; to configure it for evaluating python, try this: https://emacs.stackexchange.com/questions/74478/evaluate-single-python-code-blocks-in-a-quarto-file-like-in-r-studio-or-jupyter

;; difference between the built-in python mode and the python-mode package: https://www.reddit.com/r/emacs/comments/sshhdi/difference_between_inbuild_python_and_pythonmode/
;; tldr: the built-in python-mode is fine
;; (use-package python-mode)

(use-package pyvenv
  ;; :config
  ;; (pyvenv-mode 1)       ; shows current venv in modeline

  ;; try pyvenv-tracking-mode
  ;; https://blog.allardhendriksen.nl/posts/tracking-project-virtual-environments-with-pyvenv-tracking-mode/
  ;; https://stackoverflow.com/questions/37472595/how-to-activate-the-anancondas-env-python-in-emacs/37489343#37489343
  ;; https://github.com/jorgenschaefer/pyvenv/issues/6
  )


;; System Crafters -- Python Development Configuration
;; https://www.youtube.com/watch?v=jPXIP46BnNA
;; https://systemcrafters.net/emacs-ide/python-development-config/

;; Python Executable Tracker
;; https://github.com/wyuenho/emacs-pet




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

  (setq avy-background nil)      ; (set-face-attribute 'avy-background-face nil :foreground "grey90" :background "grey98") -- doesn't work with my highlighting of active window
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

;; from reddit: "Mine is just the default nil. I don't use any of the packages. But I prefer magit to be fullscreen and to restore back to where I was on quit:"
;; ???
;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
;; (setq magit-bury-buffer-function 'magit-restore-window-configuration)


;;;; links to revisions
;;;; from https://www.reddit.com/r/emacs/comments/asbjai/comment/egv5ymf/
;;;; or [[elisp:(magit-status "/my/repo")][magit status for my repository]]
;; (use-package orgit
;;   ;; Automatically copy orgit link to last commit after commit
;;   :hook (git-commit-post-finish . orgit-store-after-commit)
;;   :config
;;   (defun orgit-store-after-commit ()
;;     "Store orgit-link for latest commit after commit message editor is finished."
;;     (let* ((repo (abbreviate-file-name default-directory))
;;            (rev (magit-git-string "rev-parse" "HEAD"))
;;            (link (format "orgit-rev:%s::%s" repo rev))
;;            (summary (substring-no-properties (magit-format-rev-summary rev)))
;;            (desc (format "%s (%s)" summary repo)))
;;       (push (list link desc) org-stored-links))))


(comment
(use-package forge
  :after magit)
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

;; (use-package atomic-chrome)
;; (atomic-chrome-start-server)

;; (use-package code-cells)
;; (use-package jupyter)

;; -------------------------------------------------------------------

(use-package web-mode)


;; -------------------------------------------------------------------

(use-package highlight)


;; -------------------------------------------------------------------

;; (use-package key-chord
;;   :config
;;   ;; (key-chord-mode 1)
;;   (setq key-chord-one-key-delay 0.15)
;;   (setq key-chord-two-keys-delay 0.01)
;;   ;; (key-chord-define-global "qq"     "cool")
;;   ;; (key-chord-define-global "kl"     "cool")
;;   ;; (key-chord-define-global " a"     "cool")
;;   ;; (key-chord-define-global "((" 'sp-wrap-round)
;;   ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
;;   )

;; (use-package key-seq
;;   ;; (key-seq-define-global ";a" "cool")
;;   ;; (key-seq-define text-mode-map "qf" 'flyspell-buffer)
;;   ;; (key-chord-mode 1)
;;   )
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

;; this is the new winner-mode
(tab-bar-mode)

(tab-bar-history-mode 1)
(setq tab-bar-history-limit 100)

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

(setq tab-always-indent t)
;; (setq tab-always-indent 'complete)
;; c-tab-always-indent has to be set separately




(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))



(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (
                              ;; corfu-info
                              corfu-popupinfo
                              ))     ; this is the way to add these packages, see https://www.reddit.com/r/emacs/comments/z6sk1f/how_to_update_corfudoc_to_the_new_corfuinfo/
  
  ;; Optional customizations
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 4)
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
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
  (corfu-popupinfo-mode)
  
  ;; :bind
  ;; (:map corfu-map
  ;;       ("SPC" . corfu-insert-separator))      ; no need for this, when using flex
  )





(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex)   ; '(orderless flex) lets you input multiple parts of words out of order, separated by space; flex lets you avoid space, this is faster to type
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )




;; (setq path-to-ctags "/opt/local/bin/ctags")

;; https://gist.github.com/kborling/13f2300e60ae4878d5d96f5f4d041664#file-init-el-L414




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

(use-package minimap
  :config
  (setq minimap-recenter-type 'middle)
  )


;; -------------------------------------------------------------------

(use-package beacon
  ;; it highlights the cursor when you scroll, usefull for presentations
  )

;; -------------------------------------------------------------------

(use-package dumb-jump
  :config
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t)   ; to the end of list, which means fall back to dumb-jump when there are no better options
  ;; (setq dumb-jump-force-searcher 'rg)   ; tries searches in this order: git-grep, ag, rg, grep
  ;; (dumb-jump-debug t)   ; try to jump and see *messages*
  )


;; -------------------------------------------------------------------

;; (use-package json-mode)    ; jsons-print-path doesn't work


(use-package jsonian)
;; jsonian-path



;; see also
;; https://github.com/DamienCassou/json-navigator

;; -------------------------------------------------------------------

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; -------------------------------------------------------------------

;; Always open popups in the active window.
;; The back button through winner-undo is available.
;; There's a package named current-window-only for this, but this is much simpler.

;; (setq display-buffer-alist '((".*" (display-buffer-same-window))
;;                              ))

; The switch-to-buffer-other-window function chooses an arbitrary window, I don't like this.
;; This function defines the behaviour.
;; At the moment it chooses next window in cyclic order.
;; (defun my/switch-to-buffer-other-window-advised (buffer &optional norecord)
;;   (when (one-window-p)
;;     (split-window nil nil t))  
;;   (other-window 1)
;;   (switch-to-buffer buffer norecord))
;; (advice-add 'switch-to-buffer-other-window :override #'my/switch-to-buffer-other-window-advised)
;; ;; (advice-remove 'switch-to-buffer-other-window #'my/switch-to-buffer-other-window-advised)



(defun display-buffer-same-or-next-window (buffer alist)     ; slightly modified definition of display-buffer-same-window
  (if (not (or
            (cdr (assq 'inhibit-same-window alist))      ; some functions like occur-mode-display-occurrence signal the request to show occurences in some other window using inhibit-same-window, and we abide below by choosing next window
            ;; (window-dedicated-p)   ; TODO: make it skip all dedicated windows until it finds a free one
	        (window-minibuffer-p)))
      (window--display-buffer buffer (selected-window) 'reuse alist)
    (window--display-buffer buffer (next-window) 'reuse alist)
    ))
(setq display-buffer-alist
      '((".*" (display-buffer-same-or-next-window))))



;; saving for history of how I struggled, this is not needed anymore
;; (defun display-buffer-next-window (buffer alist)
;;   (unless (or
;;            ;; (cdr (assq 'inhibit-same-window alist))
;;            ;; (window-dedicated-p)
;; 	       ;; (window-minibuffer-p)
;;            )
;;     (window--display-buffer buffer (next-window) 'reuse alist)))
;; (defun ym/occur-mode-display-occurrence-advice (orig-fun &rest args)
;;   (let ((old-display-buffer-fn (symbol-function #'display-buffer)))
;;     (cl-letf (((symbol-function #'display-buffer) (lambda (&rest old-display-buffer-args)
;;                                                     (let ((display-buffer-overriding-action '((display-buffer-next-window))))
;;                                                       (apply old-display-buffer-fn old-display-buffer-args)))))
;;       (apply orig-fun args)
;;       )))
;; (advice-add 'occur-mode-display-occurrence :around #'ym/occur-mode-display-occurrence-advice)
;; ;; ;; (advice-remove 'occur-mode-display-occurrence #'ym/occur-mode-display-occurrence-advice)





;;; I don't remember why I had this:
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'prompt)    ; I'm not sure what this does, so I configured it to ask me.


;; -------------------------------------------------------------------

;; clone in order to prevent reusing buffers

(defun jue-clone-buffer ()
  "jue clone current buffer. Useful to have multiple help buffers."
  (interactive)
  (rename-buffer (generate-new-buffer-name
                  (concat (buffer-name) " -- "                 ; create name from old name and
                          (save-excursion                   ; use first word in buffer for new name
                            (goto-char 0)
                            (thing-at-point 'symbol t))))
                 t))                                      ; show cloned buffer now

;; write the same for occur using occur-rename-buffer
;; for rg clone?


(defvar bubbles-empty-buffer-fn
  (lambda (i) (get-buffer-create "*scratch*"))
  "When there are more windows than buffers, fill them in with the results of this function. It must accept an index arg. Added just in case someone wants to implement something fancy, like adding dired buffers to the end.")

(defun bubbles (&rest numbers)
  (let* ((bufs (mapcar
                (lambda (w) (window-buffer w))
                (window-list)))
         (i 0)
         (main-area-width (if (< bubbles/main-area-colsize (/ (frame-width) (length numbers)))
                              (/ (frame-width) (length numbers))
                            bubbles/main-area-colsize
                              ))
         )
    (cl-flet ((switch-to-next-buffer ()    ; We could avoid switching to the same buffer at the very first entry in the list, but this would hurt simplicity of the body.
                (if bufs
                    (progn
                      (let ((next-buf (car bufs)))
                        (unless (eq next-buf (current-buffer))      ; Handling a special case here, when there are multiple windows for the same buffer, with different positions. Otherwise, point may jump.
                         (switch-to-buffer next-buf)))
                      (pop bufs))
                  (progn
                    (switch-to-buffer (funcall bubbles-empty-buffer-fn i))
                    (cl-incf i)))))
      (delete-other-windows)
      (dotimes (idx (length numbers))    ; instead of dolist, because we want to handle edge cases
        (unless (= idx (- (length numbers) 1))
         (if (= idx 0)
            (split-window-right main-area-width)
          (split-window-right
           (round (/ (window-total-width)
                     (- (length numbers) idx)))
           )))
        (switch-to-next-buffer)
        (dotimes (idx-v (- (nth idx numbers) 1))
          (split-window-below
           (round (/ (window-total-height)
                     (- (nth idx numbers) idx-v)))
           )
          (other-window 1)
          (switch-to-next-buffer))
        (other-window 1))
      (while (ignore-error user-error (windmove-left))
        (comment do nothing))
      (while (ignore-error user-error (windmove-up))
        (comment do nothing))
      )))

(defun bubbles-window-configuration-is-ok-p ()
  (cl-flet ((next-and-below-are-same-p (_)    ; I'm not entirely sure this catches all possible problems, but it's good enough.
              (if (window-in-direction 'below)
                  (eq (window-in-direction 'below)
                      (next-window))
                (let ((cur-wnd (selected-window))
                      (top-wnd-next-column (progn (ignore-error user-error (windmove-right))
                                                  (while (ignore-error user-error (windmove-up)))
                                                  (selected-window))))
                  (select-window cur-wnd)
                  (eq (next-window)
                      top-wnd-next-column)))))
    (let ((list-of-broken-assumptions
           (seq-filter #'not
                       (mapcar
                        #'next-and-below-are-same-p          ; the crux of it
                        (window-list)))))
     (if (seq-empty-p list-of-broken-assumptions)
        t
       nil))))


(defun my-wnd-info (w)
  (with-selected-window w
      (with-current-buffer (window-buffer w)
        (append
         (list
          :buffer-filename-or-name (if (buffer-file-name)
                                       (file-relative-name buffer-file-name (projectile-project-root))
                                     (window-buffer w))
          :projectile-project-root (projectile-project-root)
          :is-file (when (buffer-file-name) t)
          :line-number (save-restriction (widen) (line-number-at-pos))
          :window-width  (window-width)
          :window-height (window-height))
         (when (eq w (selected-window))
             (list :is-selected-window t))
         ))))

;; (defun bubbles-detect-window-configuration-str-2 (get-relevant-window-data-fn)
;;   (interactive)
;;   (if (not (bubbles-window-configuration-is-ok-p))
;;       (error "Bubbles detected a broken window configuration.")
;;     (let ((list-of-wnd-data (mapcar
;;                              get-relevant-window-data-fn
;;                              (window-list))))
;;       (while (ignore-error user-error (windmove-left)))
;;       (while (ignore-error user-error (windmove-up)))
;;       (let ((wnd-rows-count-list '()))
;;         (cl-flet ((count-rows-in-a-column () (let ((r 1))
;;                              (while (ignore-error user-error (windmove-up)))
;;                              (while (ignore-error user-error (windmove-down))
;;                                (cl-incf r)
;;                                )
;;                              ;; r
;;                              (push r wnd-rows-count-list)
;;                              )))
;;           (count-rows-in-a-column)
;;           (while (ignore-error user-error (windmove-right)
;;                                (count-rows-in-a-column))))
;;         (push (nreverse wnd-rows-count-list) list-of-wnd-data)
;;         )
;;       )
;;     )
;;   )


;;;; an example usage of do-while construct in elisp using the widely disliked loop macro, for reference
;; (let ((x 10))
;;   (loop
;;    do (progn
;;         (decf x)
;;         (print x))
;;    while (plusp x)))

(defun bubbles-detect-window-configuration-str-2 (get-relevant-window-data-fn)
  (interactive)
  (cl-flet ((windmove-left-or-nil ()   (ignore-error user-error (windmove-left)))   ; otherwise it throws an error when it's the leftmost window
            (windmove-right-or-nil ()  (ignore-error user-error (windmove-right)))
            (windmove-up-or-nil ()     (ignore-error user-error (windmove-up)))
            (windmove-down-or-nil ()   (ignore-error user-error (windmove-down))))
    (if (not (bubbles-window-configuration-is-ok-p))
        (error "Bubbles detected a broken window configuration.")
      (let ((the-window-before-we-started-walking (selected-window)))
       (while (windmove-left-or-nil))
       (while (windmove-up-or-nil)q)
       (let ((cols '()))     ; our configuration is a list of columns, which are lists of rows, which are descriptions of windows
         (cl-loop
          do (progn   ; wrapped the body of do-while, just for readability
               (while (windmove-up-or-nil))   ; it's a different while, not related to the loop macro
               (let ((rows '()))
                 (cl-loop
                  do (push (funcall get-relevant-window-data-fn (selected-window)) rows)
                  while (windmove-down-or-nil))
                 (push (nreverse rows) cols))
               )
          while (windmove-right-or-nil))
         (select-window the-window-before-we-started-walking)
         (nreverse cols)
         )))))

(defun rebuild-bubbles (conf)
  (interactive)
  (let ((columns conf))   ; just renaming it
    (delete-other-windows)
    (cl-loop for idx from 0
             for col in columns do
             (unless (= idx
                        (- (length columns) 1))   ; the last one
               (split-window-right))
             (cl-loop for idx-v from 0
                      for row in col
                      do
                      (switch-to-buffer (plist-get row :buffer-filename-or-name))
                      (unless (= idx-v
                                 (- (length col) 1))   ; the last one
                        (split-window-below)
                        (other-window 1)
                        ))
             (other-window 1))
    ))

;; (bubbles-detect-window-configuration-str #'my-wnd-info)

;; (setq my-test-wm-conf (bubbles-detect-window-configuration-str-2 #'my-wnd-info))
;; (pp my-test-wm-conf)
;; (rebuild-bubbles my-test-wm-conf)

;; (rebuild-bubbles my-test-wm-conf)

      ;; (other-window 1)  ; After all splitting we're left in the last window, this transfers us to the first one.
      ;; (while (ignore-error user-error (windmove-left)))
      ;; (while (ignore-error user-error (windmove-up)))



(defvar bubbles/main-area-colsize 130)

(defun bubbles/enlarge-main-area (ncols &rest args)
  (interactive)
  (let* ((save-to-preferences (plist-get args :save-to-preferences))
         (cur-wnd (selected-window))
         (n-divisions 1))
    (while (ignore-error user-error (windmove-right))
      (comment do nothing while we move to the right edge of the frame))
    (while (ignore-error user-error (windmove-left))
      (cl-incf n-divisions))
    (while (ignore-error user-error
             (windmove-right))
      (shrink-window-horizontally ncols))
    (select-window cur-wnd)
    (when save-to-preferences
      (let* ((cur-main-area-size (or (gethash n-divisions bubbles/main-area-enlarments-at-different-divisions)
                                     0))
             (increased-size (+ cur-main-area-size ncols)))
        (puthash n-divisions
                 increased-size
                 bubbles/main-area-enlarments-at-different-divisions)))))

(defun bubbles-from-str (s)
  (interactive "s")
  (let ((l (mapcar #'string-to-number (mapcar #'char-to-string s))))
    (apply #'bubbles l)))

(defun bubbles-balance-windows ()
  (interactive)
  
  )


(defun my-enlarge-right (delta)
  (interactive "p")
  (if (window-in-direction 'right)
      (with-selected-window (window-in-direction 'right)
        (shrink-window-horizontally delta))))


(comment
 (bubbles 1 2)
 (bubbles 2 3)
 (bubbles 3 3)
 (bubbles 1 2 3)
 (bubbles 1 2 4)
 (bubbles 1 2 4)
 (bubbles 1 2 5)
 (bubbles 1 3 4)
 (bubbles 2 3 4)
 (bubbles 2 3 4 5)

 (xah-print-hash bubbles/main-area-enlarments-at-different-divisions)

 (defun xah-print-hash (hashtable)
   "Prints the hashtable, each line is key, val"
   (maphash
    (lambda (k v)
      (princ (format "%s , %s" k v))
      (princ "\n"))
    hashtable
    ))
 
 (enlarge-window-horizontally 3)
 (enlarge-window (round (* (window-width) .5)) t)

 
 ;; (defvar bubbles-main-area-increase-factor 0.4)       ; I don't bother to calculate the right percentage, this factor is not seen by the user, so it can be 
 (shrink-window (round (* (window-width) bubbles-main-area-percentage)) t)


 ;; (defvar bubbles-main-area-enlarge-by-number-of-columns 30)

 

 
 (progn
   (delete-other-windows)
   (split-window-horizontally)
   (split-window-vertically)
   (other-window 1)
   (split-window-vertically)
   (other-window 1)
   ;; (split-window-horizontally)
   (other-window 1)
   (split-window-vertically)
   (other-window 1)
   (balance-windows)
   )

 
 ;; (window-parent (selected-window))
 (split-window (window-parent (selected-window)) nil 'right)   ; instead of (split-window-horizontally)
 (split-window (frame-root-window) nil 'right)   ; instead of (split-window-horizontally)

 
 

 (window-in-direction 'down)   ; https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows-and-Frames.html
 (window-full-height-p)
 

 (mapcar
  (lambda (w)
    (with-current-buffer (window-buffer w)
      (format "[[%s::%d]]"
              (if (buffer-file-name)
                  (file-relative-name buffer-file-name (projectile-project-root))
                (window-buffer w))
              (save-restriction (widen) (line-number-at-pos)))))
  (window-list))

 (car (seq-filter
       (lambda (w)
         (and (window-at-side-p w 'bottom)
              (window-at-side-p w 'right)))
       (window-list)))


 (split-window-vertically (floor (* 0.68 (window-height))))

 
 (setq display-buffer-alist '((popper-display-control-p          ; the original value
                               (popper-select-popup-at-bottom))))

 (setq display-buffer-alist '((popper-display-control-p
                               (display-buffer-no-window))
                              (".*"
                               (display-buffer-same-window))))
 
 (setq display-buffer-alist '(("\\*Help\\*"
                               (display-buffer-in-side-window)
                               (side . right)
                               (slot . 99999999)
                               )))

 (defun my-switch-to-buffer-list (buffer alist)
   ;; (split-window-below)
   (select-window
    ;; (display-buffer-below-selected buffer alist)
    (display-buffer-use-some-window buffer alist)
    ))
 (setq display-buffer-alist '((".*" (my-switch-to-buffer-list))))
 (setq display-buffer-alist '((popper-display-control-p          ; the original value
                               (my-switch-to-buffer-list))))

 ;; see https://www.masteringemacs.org/article/demystifying-emacs-window-manager
 ;; see https://e17i.github.io/articles-emacs-display-1/
 )



;; (advice-add 'split-window-right :after #'balance-windows)

;; -------------------------------------------------------------------

(use-package org-transclusion)

(defun org-dblock-write:transclusion (params)
  (progn
    (with-temp-buffer
      
      (insert-file-contents (plist-get params :filename))
      (let ((range-start (or (plist-get params :min) (line-number-at-pos (point-min))))
            (range-end (or (plist-get params :max) (line-number-at-pos (point-max)))))
        (copy-region-as-kill (line-beginning-position range-start)
                             (line-end-position range-end)))
      )
    (insert "\n#+begin_src elisp\n")
    (yank)
    (insert "\n#+end_src\n")
    ))

;; -------------------------------------------------------------------

;; for searching symbols with a single keybinding
(use-package smartscan
  )

;; -------------------------------------------------------------------

(use-package hide-lines)


;; -------------------------------------------------------------------










