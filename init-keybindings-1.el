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

;; I no longer use cua-mode
;; (cua-mode t)   ; CUA mode: C-x, C-c, C-v for copying, pasting, C-z for undo
;; (setq cua-keep-region-after-copy t)   ; Standard Windows behaviour

;; -------------------------------------------------------------------

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
(global-set-key (kbd "H-z") 'hydra-zoom/body)



(defhydra hydra-smartparens ()
  "smartparens"
  ("g" text-scale-increase "in")
  )


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

(pretty-hydra-define hydra-smartparens ()  ; :title "smartparens" :idle 0.5

  ;; TODO: toggle show-parens-mode/show-smartparens-mode
  ;; TODO: toggle smartparens-mode/smartparens-strict-mode
  ;; smartparens-global-strict-mode
  ;; turn-on-smartparens-strict-mode, turn-off-smartparens-strict-mode

  ;; sp-narrow-to-sexp
  

  ;; TODO: color things :pink and :blue, see https://github.com/abo-abo/hydra#foreign-keys

  ;; TODO: let us use hydra funcs by name
  ;; (ido-completing-read "test: " '("hello" "world"))
  
  ("Move" (
	   ;; these are cool
	   ;; the only thing I'd fix in them is they cycle through the list, e.g., (a| b c) -> (a b| c) -> (a b c|) and then back to the first one
	   ;; TODO: stop the sp-forward-parallel-sexp and sp-backward-parallel-sexp navigation from cycling in lists
	   ("l" sp-forward-parallel-sexp)
	   ("j" sp-backward-parallel-sexp)

	   ("i" sp-backward-up-sexp)
	   ("k" sp-up-sexp)

	   ("q" sp-show-enclosing-pair)       ; TODO: move faces configuration from customize.el

	   ("1" rainbow-mode "show hex colors" :toggle t )
	   ("2" rainbow-delimiters-mode :toggle t)
	   ("3" beacon-mode :toggle t)
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
	   )

   "Wrapping" (
	       ("r" sp-rewrap-sexp)
	       ("y" sp-unwrap-sexp)
	       ("u" sp-backward-unwrap-sexp)

	       ;; not used, because these three only work forward, and we can use selection anyway
	       ("(" sp-wrap-round)
	       ("{" sp-wrap-curly)
	       ("[" sp-wrap-square)
	       ;; TODO: maybe add my own bindings like sp-wrap-round-backwards later

	       ;; not used, because it's easier to rewrap than remembering this command
	       ;; ("" sp-swap-enclosing-sexp)   ; should ask for a number

	       )
   "Selecting" (
		("w" sp-select-next-thing)
		("W" sp-select-previous-thing)
		;; these are the same as above, but exchange the point and the mark
		;; ("" sp-select-next-thing-exchange)
		;; ("" sp-select-previous-thing-exchange)
		
		("e" er/expand-region)
		("E" er/contract-region)
		)

   "Slurp, barf, extract, absorb, emit" (
					 ("s" sp-forward-slurp-sexp)
					 ("S" sp-backward-slurp-sexp)
					 ("b" sp-forward-barf-sexp)
					 ("B" sp-backward-barf-sexp)

					 ;; these two are like slurp, when you add a thing to an s-exp, but it does more, which is too much to me, and it's easier to just use slurp
					 ;; ("" sp-add-to-next-sexp)
					 ;; ("" sp-add-to-previous-sexp)

					 ;; this is useful, but it indents too much
					 ;; and forgets to remove a whitespace
					 ;; and its understanding of at point is quirky, cursor must be exactly at a thing, not after
					 ;; TODO: indent after extracting using sp-extract-before-sexp
					 ("a" sp-extract-before-sexp)
					 ("A" sp-extract-after-sexp)

					 ("z" sp-absorb-sexp)   ; a (f b) -> (f a b)
					 ("Z" sp-emit-sexp)    ; (f a b) -> a (f b)
					 )
   "Sexp juggling" (
		    ("v" sp-split-sexp)
		    ("c" sp-join-sexp)
		    ("" sp-raise-sexp)   ; raise next expr, (a |b c) -> b

		    ("x" sp-convolute-sexp)   ; (outer (inner target)) -> (inner (outer target))     ; https://hungyi.net/posts/convolute-lisp-sexp-with-smartparens/

		    ;; ("m" sp-splice-sexp)  ; easier to use unwrap
		    ;; sp-splice-sexp-killing-forward, sp-splice-sexp-killing-backward, sp-splice-sexp-killing-around   ; easier to cut and paste

		    ("." sp-transpose-sexp)            ; = drag forward/backward
		    (","
		     (lambda () (interactive) (sp-transpose-sexp -1))
		     "sp-transpose-sexp backwards")
		    )

   "Destructive editing" (
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

			  ("d" sp-clone-sexp)
			  ("D" sp-kill-whole-line)
			  )



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
   ;; 	      ("H-a" . sp-splice-sexp)
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

   ))
(global-set-key (kbd "s-q") 'hydra-smartparens/body)



  ;; "Snippets"
  ;; (("y" yas-expand)
  ;;  ("y" yas-insert-snippet)
  ;;  ("y" yas-visit-snippet-file)
  ;;  ("y" yas-new-snippet)
  ;;  ("y" yas-reload-all)
  ;;  ("y" ym-list-all-yasnippets)
  ;;  ("y" ym-list-all-yasnippets-official "official snippets"))



(pretty-hydra-define hydra-window ()
  ("move"
   (("j" windmove-left)
    ("k" windmove-down)
    ("i" windmove-up)
    ("l" windmove-right)
    ("w" winner-undo)
    ("w" winner-redo)
    ("v" visual-line-mode :toggle t)
    ("f" (lambda () (interactive)
	   (follow-mode t))
     "follow-mode 2")

    )
  "window"
   (("1" delete-other-windows)
    ("2" split-window-below)
    ("3" split-window-right)
    )

   "buf"
   (("b" buf-move-up)
    ("b" buf-move-down)
    ("b" buf-move-left)
    ("b" buf-move-right)
    )
   ))
(global-set-key (kbd "H-w") 'hydra-window/body)

(pretty-hydra-define hydra-drill ()
  ("asdfasdf"
   (
    ;; ("d" org-drill)
    ;; ("t" org-drill-tree)
    ;; ("r" org-drill-resume)

    ;; ("z" (lambda () (interactive) (org-agenda nil "dn"))
    ;;  "show new")
    ;; ("x" (lambda () (interactive) (org-agenda nil "dt"))
    ;;  "show today")
    ;; ("c" (lambda () (interactive) (org-agenda nil "df"))
    ;;  "show future")
    ;; ("q" (lambda () (interactive) (org-toggle-tag "drill")) "toggle :drill:")
    ;; ("w" (lambda () (interactive) (org-toggle-tag "drilltodo")) "toggle :drilltodo:")
    ;; TODO: org-set-tags



   
   )))


(pretty-hydra-define hydra-search ()
  ("search and replace"
   (("f" anzu)   ; TODO: https://github.com/emacsorphanage/anzu
    )

   "Ripgrep"
   (("r" rg)
    ("r" rg-project)
    ("r" rg-menu)
    )
   ))

(pretty-hydra-define hydra-text ()
  ("text"
   (("m" mc/edit-lines)
    ("m" mc/mark-all-like-this)
    ("m" mc/mark-next-like-this)
    ("m" mc/mark-previous-like-this)


    ("d" duplicate)
    ("c" comment)
    ("c" copy-line)
    ("c" cut-line)
    ("c" remove-line)

    ("d" drag-stuff)    ; see also at the end of this: https://www.emacswiki.org/emacs/MoveLine
    )
   ))
;; (yas-expand-snippet (yas-lookup-snippet "let"))



(pretty-hydra-define hydra-agenda ( :exit t)
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

	  )

   ))
(global-set-key (kbd "<f1>") 'hydra-agenda/body)


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
