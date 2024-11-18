;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key/14759#14759
;; let the escape key do its thing
;; yeah, I feel the judgemental stare
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))   ; = quit

;; -------------------------------------------------------------------

(defun dont-kill-emacs ()   ;; disable C-x C-c
  (interactive)
  (message (substitute-command-keys "To exit emacs: \\[save-buffers-kill-emacs]")))
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

;; -------------------------------------------------------------------

(ym-define-key (kbd "M-<tab>") #'completion-at-point)      ; dabbrev, corfu


;; -------------------------------------------------------------------

;; I no longer use cua-mode
;; (cua-mode t)   ; CUA mode: C-x, C-c, C-v for copying, pasting, C-z for undo
;; (setq cua-keep-region-after-copy t)   ; Standard Windows behaviour

;; -------------------------------------------------------------------

;; (defhydra hydra-zoom ()
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))
;; (global-set-key (kbd "H-z") 'hydra-zoom/body)



;; (defhydra hydra-smartparens ()
;;   "smartparens"
;;   ("g" text-scale-increase "in")
;;   )


(defun ym/highlight-enclosing-pairs ()
  (interactive)
  
;; (define-advice show-paren-function (:around (fn) fix)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") (funcall fn))      ; \s( and \s) are open and close delimiter character
;; 	  ((save-match-data (looking-back "\\s)" 1)) (funcall fn))   ; if performance is an issue, replace looking-back with char-before and 
;; 	  (t (save-excursion
;; 	       (ignore-errors (backward-up-list))
;; 	       (funcall fn)))))


  )

;; (pretty-hydra-define hydra-smartparens (
;;                                         :title "smartparens"
;;                                                ;; :idle 0.5
;;                                         )

  ;; TODO: toggle show-parens-mode/show-smartparens-mode
  ;; TODO: toggle smartparens-mode/smartparens-strict-mode
  ;; smartparens-global-strict-mode
  ;; turn-on-smartparens-strict-mode, turn-off-smartparens-strict-mode

  ;; sp-narrow-to-sexp
  

  ;; TODO: color things :pink and :blue, see https://github.com/abo-abo/hydra#foreign-keys

  ;; TODO: let us use hydra funcs by name
  ;; (ido-completing-read "test: " '("hello" "world"))
  
  ;; ("Move" (

           ;; (" " sp-forward-parallel-sexp)
	       ;; (" " sp-backward-parallel-sexp)

	       ;; (" " sp-backward-up-sexp)
	       ;; (" " sp-up-sexp)

	   ;; these are cool
	   ;; the only thing I'd fix in them is they cycle through the list, e.g., (a| b c) -> (a b| c) -> (a b c|) and then back to the first one
	   ;; TODO: stop the sp-forward-parallel-sexp and sp-backward-parallel-sexp navigation from cycling in lists

	       ;; sp-highlight-current-sexp -- useless for me
	   ;; TODO: configure faces, see sp-show-pair-enclosing and sp-show-pair-match-content-face, and https://github.com/Fuco1/smartparens/wiki/User-interface
	   ;; TODO: show three levels of enclosing pairs

	   ;; not used, because they combine going left/right and up, too much to me 
	   ;; ("" sp-next-sexp)
	   ;; ("" sp-backward-sexp)
	   ;; ("" sp-forward-sexp)
	   ;; ("" sp-previous-sexp)

	   ;; not used, because these are for jumping between (a|   b) (a   |b), it's easier to just use arrow keys
	   ;; ("" sp-skip-forward-to-symbol)
	   ;; ("" sp-skip-backward-to-symbol)
	   
	   ;; not used, because these jump to these two positions: (|   ) and (    |)
	   ;; ("" sp-beginning-of-sexp)
	   ;; ("" sp-end-of-sexp)
	   ;; ("" sp-beginning-of-next-sexp)
	   ;; ("" sp-beginning-of-previous-sexp)
	   ;; ("" sp-end-of-next-sexp)
	   ;; ("" sp-end-of-previous-sexp)

	   ;; not used, because it's easier to just move the cursor one position right, when I'm beforea paren, e.g., |(   ) -> (|   )
	   ;; ("" sp-down-sexp)
	   ;; ("" sp-backward-down-sexp)

	   ;; these are internal functions: sp-forward-symbol, sp-backward-symbol
	   ;; )

   ;; "Wrapping" (
	       ;; not used, because these three only work forward, and we can use selection anyway
	       ;; ("(" sp-wrap-round)
	       ;; ("{" sp-wrap-curly)
	       ;; ("[" sp-wrap-square)
	       ;; TODO: maybe add my own bindings like sp-wrap-round-backwards later

	       ;; not used, because it's easier to rewrap than remembering this command
	           ;; ("" sp-swap-enclosing-sexp)   ; should ask for a number


               ;; ("y" sp-unwrap-sexp)   ; unwrap next sexp, I don't need this; I use splice
	           ;; ("Y" sp-backward-unwrap-sexp)

	       ;; )
   ;; "Selecting" (
		;; ("m" sp-select-next-thing)        ; expand-region is more intuitive, when you place cursorn on parens
	    ;; ("M" sp-select-previous-thing)
        
		;; these are the same as above, but exchange the point and the mark
		;; ("" sp-select-next-thing-exchange)
		;; ("" sp-select-previous-thing-exchange)
		
		;; )

   ;; "Slurp, barf, extract, absorb, emit" (
					 

					 ;; these two are like slurp, when you add a thing to an s-exp, but it does more, which is too much to me, and it's easier to just use slurp
					 ;; ("" sp-add-to-next-sexp)
					 ;; ("" sp-add-to-previous-sexp)

					 ;; this is useful, but it indents too much
					 ;; and forgets to remove a whitespace
					 ;; and its understanding of at point is quirky, cursor must be exactly at a thing, not after
					 ;; TODO: indent after extracting using sp-extract-before-sexp
					 ;; )
   ;; "Sexp juggling" (
		    
		    ;; sp-splice-sexp-killing-forward, sp-splice-sexp-killing-backward, sp-splice-sexp-killing-around   ; easier to cut and paste


   ;; "Destructive editing" (
			  ;; not used, easier to do this manually
			  ;; sp-comment -- in strict mode it moves the closing parenthesis on a new line
			  ;; ( ;
			  ;;    )
			  ;; instead of ( ; )

			  ;; ("" sp-change-inner :exit t)    ; (a| [b]) -> (a [|]), I do this manually
			  ;; ("" sp-change-enclosing :exit t)   ; (a |b c) -> (|)

			  ;; ("" sp-kill-sexp)   ; kill list forward, easier to do manually
			  ;; ("" sp-backward-kill-sexp)
			  ;; ("" sp-copy-sexp)
			  ;; ("" sp-backward-copy-sexp)
			  ;; sp--kill-or-copy-region -- internal

			  ;; )



   ;; see for examples: https://github.com/Fuco1/smartparens/wiki/Hybrid-S-expressions
   ;; sp-kill-hybrid-sexp
   ;; sp-transpose-hybrid-sexp
   ;; sp-push-hybrid-sexp
   ;; sp-slurp-hybrid-sexp
   ;; sp-indent-adjust-sexp, sp-dedent-adjust-sexp

   ;; see https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks#use-the-type-prefixes
   ;; sp-prefix-tag-object
   ;; sp-prefix-pair-object
   ;; sp-prefix-symbol-object
   ;; sp-prefix-save-excursion

   ;; these are for restricting to moving across curly braces, for example
   ;; sp-restrict-to-pairs, sp-restrict-to-pairs-interactive
   ;; sp-restrict-to-object

   ;; internal: sp-escape-wrapped-region, sp-escape-quotes-after-insert

   ;; sp-forward-whitespace, sp-backward-whitespace

   ;; sp-mark-sexp -- easier to just select text

   ;; internal funcs?
   ;; sp-delete-char, sp-backward-delete-char
   ;; sp-kill-symbol, sp-delete-symbol, sp-backward-kill-symbol, sp-backward-delete-symbol
   ;; sp-kill-word, sp-delete-word, sp-backward-kill-word, sp-backward-delete-word

   ;; sp-delete-region
   ;; sp-indent-defun
   ;; sp-newline


   
   ;; this is from an old use-package config
   ;; :bind (:map smartparens-mode-map
   ;; 	      ("s-n" . sp-backward-up-sexp)
   ;; 	      ("s-," . (lambda () (interactive) (sp-backward-sexp)))
   ;; 	      ("s-." . (lambda () (interactive) (sp-forward-sexp 2) (sp-backward-sexp)))
   ;; 	      ("s-m" . (lambda () (interactive)
   ;; 	      		 (let ((end-of-thing    (sp-get (sp-get-thing) :end)))
   ;; 	      		   (if (> end-of-thing (point))
   ;; 	      		       (goto-char end-of-thing))
   ;; 			   )))
   ;; 	      ("H-s" . sp-splice-sexp-killing-forward)
   ;; 	      ("H-d" . sp-splice-sexp-killing-backward)
   ;; 	      ("H-f" . sp-splice-sexp-killing-around)
   ;; 	      )
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

   ;; )))
;; (global-set-key (kbd "s-q") 'hydra-smartparens/body)


;; (pretty-hydra-define hydra-puni ()  ; :title "puni" :idle 0.5
           ;; (" " puni-forward-sexp)
	       ;; (" " puni-backward-sexp)

           ;; (" " puni-beginning-of-sexp)
	       ;; (" " puni-end-of-sexp)
           
       ;; bind puni-backward-delete-char to backspace?
       
       ;; puni-forward-delete-char
       ;; puni-backward-delete-char
       ;; puni-forward-kill-word
       ;; puni-backward-kill-word
       ;; puni-kill-line
       ;; puni-backward-kill-line

       ;; (setq puni-confirm-when-delete-unbalanced-active-region nil)
       ;; puni-kill-active-region
   ;; puni-force-delete


           ;; ("s" puni-slurp-forward)
	       ;; ("S" puni-slurp-backward)
	       ;; ("b" puni-barf-forward)
	       ;; ("B" puni-barf-backward)
           ;; ("s" sp-forward-slurp-sexp)
	       ;; ("S" sp-backward-slurp-sexp)
	       ;; ("b" sp-forward-barf-sexp)
	       ;; ("B" sp-backward-barf-sexp)


   ;; ("(" puni-wrap-round)
   ;; ("{" puni-wrap-curly)
   ;; ("[" puni-wrap-square)
   ;; ("<" puni-wrap-angle)
   ;; ("R" sp-show-enclosing-pair)
   
   ;; no need for these:
   ;; puni-mark-sexp-at-point
   ;; puni-mark-list-around-point
   ;; puni-mark-sexp-around-point
   ;; puni-squeeze = expand region and cut
;; )



  ;; "Snippets"
  ;; (("y" yas-expand)
  ;;  ("y" yas-insert-snippet)
  ;;  ("y" yas-visit-snippet-file)
  ;;  ("y" yas-new-snippet)
  ;;  ("y" yas-reload-all)
  ;;  ("y" ym-list-all-yasnippets)
  ;;  ("y" ym-list-all-yasnippets-official "official snippets"))




(pretty-hydra-define hydra-window (:idle 0.7
                                         :after-exit (hydra-set-property 'hydra-window :verbosity t)
                                         )
  ("move"
   (
    ;; (" " winner-undo)
    ;; (" " winner-redo)
    (" " visual-line-mode :toggle t)
    (" " (lambda () (interactive)
	       (follow-mode t) "hint")
     "follow-mode 2" :exit t)
    )
   "window"
   (("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)
    ("4" tab-bar-history-back)
    ("5" tab-bar-history-forward)
    ("0" delete-window)
    (" " tear-off-window)
    (" " balance-windows)
    (" " delete-other-windows)
    (" " delete-other-windows-vertically)
    (" " follow-delete-other-windows-and-split)
    (" " shrink-window-if-larger-than-buffer)
    ("[" (bubbles/enlarge-main-area -5 :save-to-preferences t) "main shrink")
    ("]" (bubbles/enlarge-main-area  5 :save-to-preferences t) "main enrarge")
    )

   ))
;; (hydra-set-property 'hydra-window :verbosity 1)
;; (global-set-key (kbd "s-p") #'hydra-window/body)
;; (global-set-key (kbd "s-^") #'hydra-window/body)
(ym-define-key (kbd "s-^") #'hydra-window/body)



(pretty-hydra-define hydra-agenda (:exit t)
  ("asdfasdf" (("1" (lambda () (interactive)
		      (org-agenda nil "x1")
		      (delete-other-windows)
		      )
		"habits")
	       ("2" (lambda () (interactive)
		      (org-agenda nil "x2")
		      (delete-other-windows)
		      )
		    "full")
	       ("3" (lambda () (interactive)
		      (org-agenda nil "x3")
		      (delete-other-windows)
		      )
		"month")
	       )
   "bbbbbbb" (("q" (lambda () (interactive)
		     (org-agenda nil "xq")
		     (delete-other-windows)
		     )
	       "try-read-watch-listen")
	      ("w" (lambda () (interactive)
		     (org-agenda nil "xw")
		     (delete-other-windows)
		     )
	       "watch")
	      ("e" (lambda () (interactive)
		     (org-agenda nil "xe")
		     (delete-other-windows)
		     )
	       "listen")
	      ("r" (lambda () (interactive)
		     (org-agenda nil "xr")
		     (delete-other-windows)
		     )
	       "read")
	      ("t" (lambda () (interactive)
		     (org-agenda nil "xt")
		     (delete-other-windows)
		     )
	       "try")
	      ("b" (lambda () (interactive)
		     (org-agenda nil "xb")
		     (delete-other-windows)
		     )
	       "blog")
	      
	      ;; ("7" (lambda () (interactive)
	      ;; 	      (org-ql-search (org-agenda-files) '(and (tags "try") (not (tags "english" "spanish" "humor"))) :sort 'date)
	      ;; 	      (delete-other-windows)
	      ;; 	      )
	      ;; 	"try")

	      ;; ("a" (lambda () (interactive)
	      ;; 	      (org-ql-search (org-agenda-files) '(and (todo) (not (tags "english" "spanish" "humor")) (not (todo "HABIT"))) :sort 'date)
	      ;; 	      (delete-other-windows)
	      ;; 	      )
	      ;; 	"agenda")
	      
	      )

   "ccc" (("7" (lambda () (interactive)
		 (org-ql-search "~/werk/English.org" '(tags "drill") :sort 'date)
		 (delete-other-windows)
		 )
	   "english")
	  ("8" (lambda () (interactive)
		 (org-ql-search "~/werk/Spanish.org" '(tags "drill") :sort 'date)
		 (delete-other-windows)
		 )
	   "spanish")
	  ("9" (lambda () (interactive)
		 (org-ql-search (org-agenda-files) '(and (tags "clojure") (tags "drill" "drilltodo")) :sort 'date)
		 (delete-other-windows)
		 )
	   "clojure")
	  ("u" (lambda () (interactive)
		 (org-ql-search (org-agenda-files) '(and (tags "drill" "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
		 (delete-other-windows)
		 )
	   "drill and drilltodo")
	  ("i" (lambda () (interactive)
		 (org-ql-search (org-agenda-files) '(and (tags "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
		 (delete-other-windows)
		 )
	   "drilltodo only")
	  ("o" (lambda () (interactive)
		     (let ((tt (read-string "drill tag: ")))
               (org-ql-search (org-agenda-files) `(and (tags "drill") (tags ,tt) (not (tags "english" "spanish" "humor"))) :sort '(date))
		       (delete-other-windows)))
	   "drill only")

	  )

   ))
(global-set-key (kbd "<f1>") 'hydra-agenda/body)

(global-set-key (kbd "<f2>")
                (lambda ()
                  (interactive)
                  (ym/find-and-run-last-file "^Notes-.*\\.org$" "~/werk" (lambda () (interactive) (ym/find-last-heading-with-tag "nnnnn")))
                  ))

;; add narrow-to-subtree
;; org-agenda-drag-line-forward, org-agenda-drag-line-backward


(defun ym/org-agenda-goto-timestamp ()
     (interactive)
     (org-agenda-goto)
     (progn (end-of-line)
	    (next-line)
	    (beginning-of-line))
     (ignore-errors
      (let ((cur-line (line-number-at-pos))
	    (ts-line (save-excursion (re-search-forward org-ts-regexp-both)
				     (line-number-at-pos)
				     )))
	(when (= cur-line ts-line)
	  (re-search-forward org-ts-regexp-both)
	  (beginning-of-line)
	  (forward-char 7)      ; to the day pos [2022-01-31 Mon]
	  )
	)
      )
     )
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "t") 'ym/org-agenda-goto-timestamp)))
;;  (define-key org-agenda-keymap (kbd "t") #'ym/org-agenda-goto-timestamp)
;;  (define-key org-agenda-mode-map (kbd "t") #'ym/org-agenda-goto-timestamp)


;; -------------------------------------------------------------------

;; C-backspace
;; C-d = delete line
;; C-D = cut line


;; = and | in agenda views to regex-filter entries

;; org-timer-start, then C-c C-x .






;; -------------------------------------------------------------------

;; by default C-u C-c C-w
;; (defun my/org-search ()
;;   (interactive)
;;   (let ((org-refile-targets '((org-agenda-files :maxlevel . 1))))
;;     (org-refile '(4))))



;; -------------------------------------------------------------------

;; C-c C-n (org-next-visible-heading)
;; C-c C-p (org-previous-visible-heading)
;; C-c C-f (org-forward-heading-same-level)
;; C-c C-b (org-backward-heading-same-level)
;; C-c C-u (outline-up-heading)
;; C-c C-j (org-goto)



;; -------------------------------------------------------------------

;; https://www.masteringemacs.org/article/highlighting-by-word-line-regexp
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Highlight-Interactively.html


;; -------------------------------------------------------------------

(defun match-paren ()
  "Move point to the matching parenthesis."
  (interactive)
  (cond
   (mark-active
    (exchange-point-and-mark))
   ((looking-at "\\s\(") ; If on an opening parenthesis
    (forward-sexp 1)) 
   ((save-excursion (backward-char  1) (looking-at "\\s\)")) ; If on a closing parenthesis
    (backward-sexp 1)
    ;; (forward-char 1)
    )))


;; -------------------------------------------------------------------

(use-package expand-region)
(use-package lispy)
(use-package symex)

(pretty-hydra-define hydra-aaa (:exit nil)
  (
   "Move"
   (
    ("s-e" er/expand-region :exit nil)
	("s-w" er/contract-region :exit nil)
    ("s-q" match-paren :exit t)
    ;; puni-expand-region -- can't contract
    

    
    ("s-i" lispy-slurp)      ; from the current side
    ("s-o" lispy-barf)
    ("^" sp-raise-sexp) ; (" " puni-raise)  ; raise next expr, (a |b c) -> b



    
    ;; TODO: indent after convolute
    ("rc" sp-convolute-sexp) ; ("_" puni-convolute)   ; (outer (inner target)) -> (inner (outer target))     ; https://hungyi.net/posts/convolute-lisp-sexp-with-smartparens/
    
    ("a" sp-extract-before-sexp)
	("A" sp-extract-after-sexp)

	("z" sp-absorb-sexp)   ; a (save-excurtion b) -> (save-excurtion a b)
	("Z" sp-emit-sexp)    ; (save-excurtion a b) -> a (save-excurtion b)

    ("v" sp-split-sexp) ; (" " puni-split)
	("V" sp-join-sexp)
	
    

    

    ("." sp-transpose-sexp)            ; = drag forward/backward
	(","
	 (lambda () (interactive) (sp-transpose-sexp -1))
	 "sp-transpose-sexp backwards")
    (">" symex-shift-forward)
    ("<" symex-shift-backward)
    ;; ("" puni-transpose) -- sp-transpose works better, it allows to drag sexps

    
    
    (" " sp-clone-sexp)
	(" " sp-kill-whole-line)
    )


   "Wrap"
   (
    ("rr" sp-rewrap-sexp "rewrap")
    ("ru" sp-splice-sexp "unwrap")        ; (" " puni-splice)     ; = unwrap at point
    ("rc" symex-comment)
    ;; TODO: add angle brackets to electcic-pair-mode
    ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
    )

   "Misc"
   (
    (" " smartscan)
    (" " goto-line)
    (" " rg)
    (" " rg-project)
    (" " rg-menu)

    (" " anzu "toggle anzu")   ; TODO: https://github.com/emacsorphanage/anzu
    (" " beacon-mode :toggle t)
    (" " rainbow-mode "show hex colors" :toggle t )
	(" " rainbow-delimiters-mode :toggle t)
    (" " prism-mode :toggle t)
    (" " org-toggle-link-display :toggle t)

    ("" eval-sexp--imp)
    ("" eval-defun--imp)
    ("" eval-after--imp)
    (" " eval-and-insert-after-the-sexp)
    ("" eval-more-functions--imp)

    (" " bind-variable-in-let)
    (" " unbind-variable)
    
    ("" see-symex-control--imp)
    (" " eval-whole-buffer)
    (" " switch-to-repl)
    (" " switch-to-messages-buffer)
    (" " documentation)


    (" " eval-and-insert--from-lispy)

    (" " unfurl)    ;    -- https://countvajhula.com/2021/09/25/the-animated-guide-to-symex/
    (" " collapse)

    (" " reindent-functions-needed)
    (" " comment-uncomment-sexp)

    
    (" " toggle-code-highlighting)


    (" " polilith-specific)
    (" " integrant-specific)


    (" " outline-mode-collapse-sexp)

    (" " highlight-symbol-sexp-or-regexp)

    (" " elisp-debug-other-window)
    ;; from lispy:
    ;; i prettifies code (remove extra space, hanging parens ...)
    ;; xl turns current defun into a lambda
    ;; xd turns current lambda into a defun
    ;; etc
    ;; ide features from lispy:
    ;; Z breaks out of edebug, while storing current function's arguments

    ;; narrow/widen, etc
    )


   
   
   ))
(global-set-key (kbd "s-p") 'hydra-aaa/body)

;; -------------------------------------------------------------------


