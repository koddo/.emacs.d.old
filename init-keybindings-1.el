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
(pretty-hydra-define hydra-smartparens ()  ; :title "smartparens"
  ("Move"
   (
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)

    ("p" sp-up-sexp)
    ;; sp-backward-up-sexp, sp-down-sexp, sp-backward-down-sexp

    ;; sp-next-sexp, sp-previous-sexp
    ;; sp-beginning-of-sexp, sp-end-of-sexp
    ;; sp-beginning-of-next-sexp, sp-beginning-of-previous-sexp, sp-end-of-next-sexp, sp-end-of-previous-sexp

    ;; these are internal functions: sp-forward-symbol, sp-backward-symbol
    )

  "Slurping & barfing"
  (("l" sp-forward-slurp-sexp)
   ("h" sp-backward-slurp-sexp)
   ("L" sp-forward-barf-sexp)
   ("H" sp-backward-barf-sexp)

   ("_" sp-add-to-next-sexp)
   ("_" sp-add-to-previous-sexp)

   ("_" sp-extract-before-sexp)
   ("_" sp-extract-after-sexp)
   )

  "Wrapping"
  (("R" sp-rewrap-sexp)
   ("u" sp-unwrap-sexp)
   ("U" sp-backward-unwrap-sexp)
   ("(" sp-wrap-round)
   ("{" sp-wrap-curly)
   ("[" sp-wrap-square)
   )

  "Sexp juggling"
  (("S" sp-split-sexp)

   ("r" sp-raise-sexp)
   ("j" sp-join-sexp)

   ("A" sp-absorb-sexp)
   ("E" sp-emit-sexp)
   ("o" sp-convolute-sexp)
   ("_" sp-swap-enclosing-sexp)   ; should ask for a number

   ("s" sp-splice-sexp)
   ;; sp-splice-sexp-killing-forward, sp-splice-sexp-killing-backward, sp-splice-sexp-killing-around   ; = sp-raise-sexp

   ("t" sp-transpose-sexp)    ; = drag forward/backward
   )

  "Destructive editing"
  (
   ;; sp-comment -- in strict mode it moves the closing parenthesis on a new line
   ;; ( ;
   ;;    )
   ;; instead of ( ; )

   ("c" sp-change-inner :exit t)
   ("C" sp-change-enclosing :exit t)

   ("k" sp-kill-sexp)   ; kill list forward, see docs
   ("K" sp-backward-kill-sexp)

   ("w" sp-copy-sexp)
   ;; sp-backward-copy-sexp

   )

  "Selecting"
  (("x" sp-select-next-thing)
   ("X" sp-select-previous-thing)
   ("z" er/expand-region)
   ("Z" er/contract-region)
   ;; sp-select-next-thing-exchange, sp-select-previous-thing-exchange
   )


  ;; see for examples: https://github.com/Fuco1/smartparens/wiki/Hybrid-S-expressions
  ;; sp-kill-hybrid-sexp
  ;; sp-transpose-hybrid-sexp
  ;; sp-push-hybrid-sexp
  ;; sp-slurp-hybrid-sexp
  ;; sp-indent-adjust-sexp
  ;; sp-dedent-adjust-sexp


  ;; sp-highlight-current-sexp



  ;; see https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks#use-the-type-prefixes
  ;; sp-prefix-tag-object
  ;; sp-prefix-pair-object
  ;; sp-prefix-symbol-object
  ;; sp-prefix-save-excursion

  ;; sp-restrict-to-pairs, sp-restrict-to-pairs-interactive
  ;; sp-restrict-to-object

  ;; sp-escape-wrapped-region, sp-escape-quotes-after-insert

  "Snippets"
  (("y" yas-expand)
   ("y" yas-insert-snippet)
   ("y" yas-visit-snippet-file)
   ("y" yas-new-snippet)
   ("y" yas-reload-all)
   ("y" ym-list-all-yasnippets)
   ("y" ym-list-all-yasnippets-official "official snippets"))


  ))
(global-set-key (kbd "H-q") 'hydra-smartparens/body)



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

    ("1" (lambda () (interactive)
	   (org-ql-search "~/werk/English.org" '(tags "drill") :sort 'date)
	   (delete-other-windows)
	   )
     "english")
    ("2" (lambda () (interactive)
	   (org-ql-search "~/werk/Spanish.org" '(tags "drill") :sort 'date)
	   (delete-other-windows)
	   )
     "spanish")
    ("5" (lambda () (interactive)
	   (org-ql-search (org-agenda-files) '(and (tags "clojure") (tags "drill" "drilltodo")) :sort 'date)
	   (delete-other-windows)
	   )
     "clojure")
    ("9" (lambda () (interactive)
	   (org-ql-search (org-agenda-files) '(and (tags "drill" "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
	   (delete-other-windows)
	   )
     "drill and drilltodo")
    ("8" (lambda () (interactive)
	   (org-ql-search (org-agenda-files) '(and (tags "drilltodo") (not (tags "english" "spanish" "humor"))) :sort '(date))
	   (delete-other-windows)
	   )
     "drilltodo only")

    )




   
   ))


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
  ("asdfasdf" (
	       ("1" (lambda () (interactive)
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
	       ("7" (lambda () (interactive)
		      (org-ql-search (org-agenda-files) '(and (tags "try") (not (tags "english" "spanish" "humor"))) :sort 'date)
		      (delete-other-windows)
		      )
		"try")

	       ;; ("a" (lambda () (interactive)
	       ;; 	      (org-ql-search (org-agenda-files) '(and (todo) (not (tags "english" "spanish" "humor")) (not (todo "HABIT"))) :sort 'date)
	       ;; 	      (delete-other-windows)
	       ;; 	      )
	       ;; 	"agenda")
	       
    )))


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
