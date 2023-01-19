;; -------------------------------------------------------------------

(defvar ym-keys-minor-mode-map (make-keymap) "ym-keys-minor-mode keymap")
(define-minor-mode ym-keys-minor-mode
  "My minor mode for global keybindings."
  :init-value t :lighter "" :keymap 'ym-keys-minor-mode-map)
(ym-keys-minor-mode 1)

;; -------------------------------------------------------------------

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'ym-keys-minor-mode))
      (let ((mykeys (assq 'ym-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'ym-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;; -------------------------------------------------------------------

(defun ym-define-key (key func)   ; not sure if i really need to define keys in both maps, but just in case
  (global-set-key key func)
  (define-key ym-keys-minor-mode-map key func))

(defun ym-undefined-key-message () (interactive) (message "undefined keybinding yet"))

;; -------------------------------------------------------------------

;; unbind all keybindings with
;; super
;; shift-super
;; meta
;; super-meta
;; shift-meta

(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "s-" (list x)))
                  nil))
 (concat
  "abcdefghijklmnopqrstuvwyxz"
  "ABCDEFGHIJKLMNOPQRSTUVWYXZ"
  "1234567890-=[];\\,./`"
  "!@#$%^&*()_+{}:|<>?~"
  "'\""
  ))

;; we leave M-x working
;; there was M-s prefix for sorting, but I've never used it, it's easier to remember the names of the functions
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-" (list x)))
                  nil))
 (concat
  "abcdefghijklmnopqrstuvwy"
  ; "x"     ; <- the M-x prefix stays
  "z"
  "1234567890-=[];\\,./`"
  "'"
  ))

;; mac option+keys
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-s-" (list x)))
                  nil))
 (concat
  "œ∑´®†¥¨ˆøπ“‘åß∂ƒ©˙∆˚¬…æ«Ω≈ç√∫˜µ≤≥÷"
  "¡™£¢∞§¶•ªº–≠"
  "`"
  ))

;; we have to bind shift-meta-something to any
;; otherwise these keybindings get translated to ones without shift
;; https://unix.stackexchange.com/questions/25649/is-it-possible-to-stop-emacs-from-down-translating-my-key-chords/25719#25719
(mapcar
 (lambda (x)
   (ym-define-key (kbd (concat "M-" (list x)))
                  #'ym-undefined-key-message))
 (concat
  "ABCDEFGHIJKLMNOPQRSTUVWYXZ"    ; M-S-a doesn't work for some reason, but M-A does
  "!@#$%^&*()_+{}:|<>?~"
  "\""
  ))

(ym-define-key (kbd "<s-return>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-return>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-return>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<M-return>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-S-return>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<S-return>")    #'ym-undefined-key-message)

(ym-define-key (kbd "<s-SPC>")    #'ym-undefined-key-message)
(ym-define-key (kbd "M-s- ")      #'ym-undefined-key-message)   ; "<M-s-SPC>" doesn't work
(ym-define-key (kbd "<S-s-SPC>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<M-SPC>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-S-SPC>")  #'ym-undefined-key-message)
;; (ym-define-key (kbd "<S-SPC>")    #'ym-undefined-key-message)

(ym-define-key (kbd "<s-backspace>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-backspace>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-backspace>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<M-backspace>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-S-backspace>")  #'ym-undefined-key-message)
(ym-define-key (kbd "<S-backspace>")    #'ym-undefined-key-message)

;; <s-tab> is cmd-tab, binding is useless
;; <M-s-tab>, <S-s-tab>, <M-S-tab> are not convenient, <S-tab> is probably used by org-mode
(ym-define-key (kbd "<M-tab>")    #'ym-undefined-key-message)

(ym-define-key (kbd "<s-up>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<s-down>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<s-left>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<s-right>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-up>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-down>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-left>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-right>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-up>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-down>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-left>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<M-s-right>")    #'ym-undefined-key-message)
;; (ym-define-key (kbd "<S-up>")    #'ym-undefined-key-message)     ; these four are used by org-mode
;; (ym-define-key (kbd "<S-down>")    #'ym-undefined-key-message)
;; (ym-define-key (kbd "<S-left>")    #'ym-undefined-key-message)
;; (ym-define-key (kbd "<S-right>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-up>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-down>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-left>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-s-right>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-M-up>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-M-down>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-M-left>")    #'ym-undefined-key-message)
(ym-define-key (kbd "<S-M-right>")    #'ym-undefined-key-message)

;; end of clearing all keybindings for my mode map

;; -------------------------------------------------------------------

;; we can now assign keys
(ym-define-key (kbd "M-_") (lambda () (interactive) (insert "—")))

;; -------------------------------------------------------------------

;; super + ijkl to move around, instead of arrow keys
;; considering shift for marking regions for coping/pasting

;; (defun ym-keys-ijkl-move (move-func shift-pressed)
;;   (interactive)
;;   (when (and
;;          (not mark-active)
;;          shift-pressed)
;;     (cua-set-mark))
;;   (setq mark-active shift-pressed)
;;   (funcall move-func))

;; (ym-define-key (kbd "s-i") (lambda () (interactive) (ym-keys-ijkl-move 'previous-line nil)))
;; (ym-define-key (kbd "s-I") (lambda () (interactive) (ym-keys-ijkl-move 'previous-line t)))
;; (ym-define-key (kbd "s-k") (lambda () (interactive) (ym-keys-ijkl-move 'next-line nil)))
;; (ym-define-key (kbd "s-K") (lambda () (interactive) (ym-keys-ijkl-move 'next-line t)))
;; (ym-define-key (kbd "s-j") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char nil)))
;; (ym-define-key (kbd "s-J") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char t)))
;; (ym-define-key (kbd "s-l") (lambda () (interactive) (ym-keys-ijkl-move 'forward-char nil)))
;; (ym-define-key (kbd "s-L") (lambda () (interactive) (ym-keys-ijkl-move 'forward-char t)))

(ym-define-key (kbd "s-i") (lambda () (interactive "^") (previous-line)))
(ym-define-key (kbd "s-k") (lambda () (interactive "^") (next-line)))
(ym-define-key (kbd "s-j") (lambda () (interactive "^") (backward-char)))
(ym-define-key (kbd "s-l") (lambda () (interactive "^") (forward-char)))

(ym-define-key (kbd "s-b") #'ido-switch-buffer)
(ym-define-key (kbd "s-B") #'ibuffer)
(global-set-key [remap list-buffers] 'ibuffer)

;; -------------------------------------------------------------------

;; mwim = move where I mean
;; https://github.com/alezost/mwim.el
(use-package mwim
  :config
  (ym-define-key (kbd "s-u") #'mwim-beginning-of-code-or-line)    ; I used to have custom functions for this, see git history
  (ym-define-key (kbd "s-o") #'mwim-end)
  )


;; -------------------------------------------------------------------

;; (ym-define-key (kbd "M-s-^") #'ym-undefined-key-message)   ; M-s-i
;; (ym-define-key (kbd "M-s-˚") #'ym-undefined-key-message)   ; M-s-k
;; (ym-define-key (kbd "M-s-∆") #'backward-word)   ; M-s-k
;; (ym-define-key (kbd "M-s-¬") #'forward-word)   ; M-s-l

;; -------------------------------------------------------------------

(ym-define-key (kbd "s-z") #'undo)
(ym-define-key (kbd "s-Z") #'undo-tree-visualize)
(ym-define-key (kbd "s-x")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-region beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the cut
(ym-define-key (kbd "s-c")
               (lambda (beg end)
                 (interactive "r")
                 (prog1
                     (kill-ring-save beg end)
                   (setq deactivate-mark nil))))   ; leave the region highlighted after the copy
(ym-define-key (kbd "s-v") #'yank)


;; this is my clean implementation
;; maybe rewrite other functions in this manner
(defun ym-delete-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			      (move-beginning-of-line 1)
			      (point)))
	 (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			      (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			      (move-beginning-of-line 2) (point)))
	 (orig-col(current-column))
	 )
    (delete-region beg end)
    (move-to-column orig-col)
    ))
(ym-define-key (kbd "s-S-<backspace>") #'ym-delete-current-line-or-region)

;; this is my clean implementation
;; maybe rewrite other functions in this manner
(defun ym-kill-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			      (move-beginning-of-line 1)
			      (point)))
	 (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			      (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			      (move-beginning-of-line 2) (point)))
	 (orig-col(current-column))
	 )
    (kill-region beg end)
    (move-to-column orig-col)
    ))
(ym-define-key (kbd "M-S-<backspace>") #'ym-kill-current-line-or-region)

;; this is my clean implementation
;; maybe rewrite other functions in this manner
(defun ym-copy-current-line-or-region ()
  (interactive)
  (let* ((beg (save-excursion (if (and mark-active (> (point) (mark))) (exchange-point-and-mark))
			      (move-beginning-of-line 1)
			      (point)))
	 (end (save-excursion (if (and mark-active (< (point) (mark))) (exchange-point-and-mark))
			      (if (and mark-active (= (point) (line-beginning-position))) (forward-line -1))
			      (move-beginning-of-line 2) (point)))
	 (orig-col(current-column))
	 )
    (kill-ring-save beg end)
    (move-to-column orig-col)
    ))
(ym-define-key (kbd "C-S-<backspace>") #'ym-copy-current-line-or-region)


(defun ym/comment-or-uncomment-line-or-region ()
  "Like comment-or-uncomment-region, but comment current line if there is no mark."
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (save-excursion (beginning-of-line) (point))
                                     (save-excursion (goto-char (mark))
                                                     (if (<= (point) (progn (back-to-indentation) (point))) (forward-line -1))
                                                     (line-end-position)))
      (comment-or-uncomment-region (save-excursion (goto-char (mark)) (line-beginning-position))
                                   (save-excursion (if (<= (point)
                                                           (progn (back-to-indentation) (point)))
                                                       (forward-line -1))
                                                   (end-of-line) (point))))))
(ym-define-key (kbd "s-/") #'ym/comment-or-uncomment-line-or-region)

(defun ym-duplicate-current-line-or-region (arg)   ; got it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")          ; also see if any problems: http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (if (and mark-active (= (point) (line-beginning-position)))
        (forward-line -1))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(ym-define-key (kbd "s-d") #'ym-duplicate-current-line-or-region)

(defun ym-duplicate-and-comment-current-line-or-region (arg)   ; got it from here: http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")          ; also see if any problems: http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (if (and mark-active (= (point) (line-beginning-position)))
        (forward-line -1))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (comment-or-uncomment-region beg end)
      (setq end (line-end-position))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      ;; (goto-char (+ origin (* (length region) arg) arg))
      ;; goto-char is commented out for the time being
      ;; maybe fix this: we don't actually know the length of the resulting commented region, so it jumps to an arbitrary position
      ;; but on the other hand, it's not really important
      )))
(ym-define-key (kbd "s-D") #'ym-duplicate-and-comment-current-line-or-region)


;; -------------------------------------------------------------------

;; use shift for selection
;; this is usual behaviour pretty much everywhere
(transient-mark-mode 1)   
(setq mark-even-if-inactive nil)     ; this is important, otherwise we can cut and paste something accidentally
(setq shift-select-mode 1)   ; shifted motion keys activate the mark momentarily
(delete-selection-mode 1)    ; typed text replaces the selection if the selection is active

;; I want unshifted movements to always deactivate region
;; without this fix it doesn't get deactivated even though the transient-mark-mode is on
(defun ym/advice-handle-shift-selection (orig-fun &optional args)
  (if (not this-command-keys-shift-translated)
      (deactivate-mark)
    (apply orig-fun args)))
(advice-add 'handle-shift-selection :around 'ym/advice-handle-shift-selection)
;; (advice-remove 'handle-shift-selection 'ym/advice-handle-shift-selection)

;; -------------------------------------------------------------------

(defun ym-backward-kill-word ()
  "Similar to backward-kill-word, but treats newlines and whitespace sequences as a words."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (delete-region (point)
                   (max (save-excursion (beginning-of-line) (point))
                        (save-excursion (backward-word) (point))
                        (save-excursion (let ((distance (skip-chars-backward "[:blank:]")))
                                          (if (or
                                               (= distance 0)
                                               (= distance -1)
                                               ;; (= distance -2)
                                               )
                                              0
                                            (point))
                                          ))   ; comment this block if removing whitespaces only is annoying
                        ))))
(ym-define-key (kbd "<M-backspace>") #'ym-backward-kill-word)
(ym-define-key (kbd "<s-backspace>") #'ym-backward-kill-word)

;; -------------------------------------------------------------------

(setq text-scale-mode-step 1.1)
(ym-define-key (kbd "s--") #'text-scale-decrease)
(ym-define-key (kbd "s-=") #'text-scale-increase)
(ym-define-key (kbd "s-0") (lambda () (interactive) (text-scale-adjust 0)))

;; -------------------------------------------------------------------

;; (require 'windmove)
;; (global-set-key (kbd "<left>") 'windmove-left)
;; (global-set-key (kbd "<right>") 'windmove-right)
;; (global-set-key (kbd "<up>") 'windmove-up)
;; (global-set-key (kbd "<down>") 'windmove-down)

;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

;; (use-package ace-window
;;   :config
;;   (global-set-key (kbd "M-o") 'ace-window)
;;   )

;; -------------------------------------------------------------------

;; TODO:
;; isearch
;; remove mark when move
;; switch marks

(ym-define-key (kbd "s-f") #'isearch-forward)

;; -------------------------------------------------------------------

(ym-define-key (kbd "s-p") 'projectile-command-map)

;; -------------------------------------------------------------------

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))
;; (global-set-key [s-left] (ignore-error-wrapper 'windmove-left))
;; (global-set-key [s-right] (ignore-error-wrapper 'windmove-right))
;; (global-set-key [s-up] (ignore-error-wrapper 'windmove-up))
;; (global-set-key [s-down] (ignore-error-wrapper 'windmove-down))

;; -------------------------------------------------------------------

;; we turn off our keybindings-only minor mode in minibuffer
;; but the very same keybindings still work in global keymap
;; so ido takes precedence in minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (ym-keys-minor-mode 0)))

(mapcar (lambda (map)
	  (define-key map (kbd "s-j") #'ido-prev-match)
	  (define-key map (kbd "s-l") #'ido-next-match)
	  )
        (list
	 ido-buffer-completion-map
	 ido-common-completion-map
	 ido-file-completion-map
	 ido-file-dir-completion-map
	 ))

;; -------------------------------------------------------------------

(define-key isearch-mode-map (kbd "s-;") 'avy-isearch)
(ym-define-key (kbd "s-;") #'avy-goto-word-1)
(ym-define-key (kbd "s-^") #'avy-goto-parens)   ; "S-s-;" -- this is not a usual ^, it's a unicode character

;; -------------------------------------------------------------------

;; spanish 
;; https://emacs.stackexchange.com/questions/66629/how-to-make-c-x-9-the-same-as-c-x-8

(defun ym-ctl-x-8-single-quote ()
  "Simulate typing: C-x 8 \'"
  (interactive)
  (dolist (event (nreverse (list ?\C-x ?8 ?\')))
    (push (cons t event) unread-command-events)))

;; -------------------------------------------------------------------

(setq isearch-repeat-on-direction-change nil)   ; I like it to go into search mode first before searching
(defun ym-search-selection-or-isearch (forward)
  (interactive)
  (if (not mark-active)
      (if forward (isearch-forward) (isearch-backward))
    (if forward
	(if (< (point) (mark)) (exchange-point-and-mark))
      (if (> (point) (mark)) (exchange-point-and-mark)))
    (let* ((beg (point))
	   (end (mark))
	   (selection (buffer-substring-no-properties beg end)))
      (deactivate-mark)
      (isearch-mode forward nil nil nil)
      (isearch-yank-string selection)
      )))
(defun ym-search-selection-or-isearch-forward ()   (interactive) (ym-search-selection-or-isearch t))
(defun ym-search-selection-or-isearch-backward ()  (interactive) (ym-search-selection-or-isearch nil))
(ym-define-key (kbd "s-s") 'ym-search-selection-or-isearch-forward)
(ym-define-key (kbd "s-r") 'ym-search-selection-or-isearch-backward)
(define-key isearch-mode-map (kbd "s-s") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "s-r") 'isearch-repeat-backward)


;; -------------------------------------------------------------------




;; -------------------------------------------------------------------








