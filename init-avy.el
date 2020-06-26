(use-package avy
  :config

  ;; keybindings of interest:
  (define-key isearch-mode-map (kbd "s-;") 'avy-isearch)
  (ym-define-key (kbd "s-;") #'avy-goto-word-1)
  (ym-define-key (kbd "M-s-…") #'avy-goto-char-2)   ; == "M-s-;" -- this only looks like an underscore, but in fact it's some unicode symbol

  ;; (defun avy-goto-parens ()
  ;;   (interactive)
  ;;   (let ((avy-command this-command))   ; for look up in avy-orders-alist
  ;;     (avy-jump "(+")))
  ;; (add-to-list 'avy-orders-alist '(avy-goto-parens . avy-order-closest))
  ;; (ym-define-key (kbd "s-p") 'avy-goto-parens)

  (setq avy-keys
	(list
	 ?j ?k ?l
	 ?f ?d ?s ?a
	 ?u ?i ?o ?p
	 ?r ?e ?w ?q
	 ?h ?g
	 ?y ?t
	 ?m ?n
	 ?b ?v ?c ?x ?z
	 )
  	)

  (setq avy-background nil)
  (setq avy-highlight-first nil)
  (setq avy-all-windows t) ; 'all-frames
  (setq avy-style 'at-full)

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

  (defun avy-subdiv (n b)
  "Distribute N in B terms in a balanced way."
  (let* ((p (1- (floor (+ (log n b) 1e-6))))
         (x1 (expt b p))
         (x2 (* b x1))
         (delta (- n x2))
         (n2 (/ delta (- x2 x1)))
         (n1 (- b n2 1)))
    (append
     (make-list n1 x1)
     (make-list n2 x2)   ; originally this goes last, but I'd like the heaviest subtree to be bound to the first key in the list
     (list
      (- n (* n1 x1) (* n2 x2)))
     )))

  (defun avy-tree (lst keys)   ; https://github.com/abo-abo/avy/issues/164#issuecomment-631785903
    "Coerce LST into a balanced tree.
     The degree of the tree is the length of KEYS.
     KEYS are placed appropriately on internal nodes."
    (let* ((len (length keys))
	   (order-fn (cdr (assq avy-command avy-orders-alist)))
	   (lst (if order-fn
		    (cl-sort lst #'< :key order-fn)
		  lst)))
      (cl-labels
	  ((rd (ls)
	       (let ((ln (length ls)))
		 (if (< ln len)
		     (cl-pairlis keys
				 (mapcar (lambda (x) (cons 'leaf x)) ls))
		   (let* ((ks (copy-sequence keys))
			  (subdiv (avy-subdiv ln len))
			  (number-of-ones (cl-count 1 subdiv))
			  (number-of-non-ones (- len number-of-ones))
			  res)
		     (dolist (s subdiv)
		       (push (cons (pop (if (eq s 1)
					    (nthcdr number-of-non-ones ks)
					  ks)
					)
				   (if (eq s 1)
				       (cons 'leaf (pop ls))
				     (rd (avy-multipop ls s))))
			     res))
		     (nreverse res))))))
	(rd lst))))
  )
