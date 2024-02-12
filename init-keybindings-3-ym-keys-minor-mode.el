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

(ym-define-key (kbd "C-z")    #'ym-undefined-key-message)    ; I constantly hit this unintentionally

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

(ym-define-key (kbd "s-i") 'previous-line)
(ym-define-key (kbd "s-k") 'next-line)
(ym-define-key (kbd "s-j") 'backward-char)
(ym-define-key (kbd "s-l") 'forward-char)

(ym-define-key (kbd "s-s") (lambda () (interactive) (ignore-error 'user-error (windmove-left))))
(ym-define-key (kbd "s-d") (lambda () (interactive) (ignore-error 'user-error (windmove-down))))
(ym-define-key (kbd "s-f") (lambda () (interactive) (ignore-error 'user-error (windmove-right))))
(ym-define-key (kbd "s-e") (lambda () (interactive) (ignore-error 'user-error (windmove-up))))

(ym-define-key (kbd "s-S") (lambda () (interactive) (ignore-error 'error (buf-move-left))))
(ym-define-key (kbd "s-D") (lambda () (interactive) (ignore-error 'error (buf-move-down))))
(ym-define-key (kbd "s-F") (lambda () (interactive) (ignore-error 'error (buf-move-right))))
(ym-define-key (kbd "s-E") (lambda () (interactive) (ignore-error 'error (buf-move-up))))

(ym-define-key (kbd "s-w") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-prev-tab))))
(ym-define-key (kbd "s-r") (lambda () (interactive) (ignore-error 'user-error (tab-bar-switch-to-next-tab))))

(ym-define-key (kbd "s-W") (lambda () (interactive) (tab-bar-move-tab -1)))
(ym-define-key (kbd "s-R") (lambda () (interactive) (tab-bar-move-tab 1)))

(defvar ym/toggle-single-window-last-wc nil)
(defun ym/toggle-single-window ()
  (interactive)
  (if (not (one-window-p))            ; used to be (> (count-windows) 1)
      (progn
        (setq ym/toggle-single-window-last-wc (current-window-configuration))
        (delete-other-windows))
    (when (window-configuration-p ym/toggle-single-window-last-wc)
      (set-window-configuration ym/toggle-single-window-last-wc)
      (setq ym/toggle-single-window-last-wc nil))))

(ym-define-key (kbd "s-!") (lambda () (interactive) (ignore-error 'error (delete-window))))
(ym-define-key (kbd "s-1") 'ym/toggle-single-window)
(ym-define-key (kbd "s-@") (lambda () (interactive) (ignore-error 'error (split-window-below))))
(ym-define-key (kbd "s-#") (lambda () (interactive) (ignore-error 'error (split-window-right))))
(ym-define-key (kbd "s-2") 'tab-bar-history-back)
(ym-define-key (kbd "s-3") 'tab-bar-history-forward)

;;; here we avoid catching rethrowing errors: https://emacs.stackexchange.com/questions/13705/rethrowing-an-error-in-emacs-lisp
;;; for reference:
;; (let ((error t))
;;   (unwind-protect
;;       (prog1 (call-function)
;;         (setq error nil))
;;     (when error (cleanup))))

;; (defun bouncy-scroll-down ()        ; page up
;;   (interactive)
;;   (let ((error t))
;;     (unwind-protect
;;         (prog1 (progn
;;                  (if (and
;;                       (= (line-number-at-pos) (line-number-at-pos (point-max)))
;;                       (eq last-command #'bouncy-scroll-up)
;;                       )
;;                      (goto-char bouncy-scroll---last-pos))
;;                  (scroll-down))
;;           (setq error nil))
;;       (when error
;;         (if (= (line-number-at-pos) (line-number-at-pos (point-min)))
;;             (if (eq last-command this-command)         
;;                 (goto-char bouncy-scroll---last-pos)
;;               (setq bouncy-scroll---last-pos (point)))
;;           (setq bouncy-scroll---last-pos (point))
;;           (beginning-of-line (beginning-of-buffer)))))))
;; (defun bouncy-scroll-up ()        ; page down
;;   (interactive)
;;   (let ((error t))
;;     (unwind-protect
;;         (prog1
;;             (progn (when (and
;;                           (= (line-number-at-pos) (line-number-at-pos (point-min))))
;;                      (eq last-command #'bouncy-scroll-down)
;;                      (goto-char bouncy-scroll---last-pos))
;;                    (scroll-up))
;;           (setq error nil))
;;       (when error
;;         (if (= (line-number-at-pos) (line-number-at-pos (point-max)))
;;             (if (eq last-command this-command)         
;;                 (goto-char bouncy-scroll---last-pos)
;;               (setq bouncy-scroll---last-pos (point)))
;;           (setq bouncy-scroll---last-pos (point))
;;           (beginning-of-line (end-of-buffer)))))))



(defvar bouncy-scroll---last-pos nil)
(defvar bouncy-scroll---column-before-scrolling nil)
(make-variable-buffer-local 'bouncy-scroll---last-pos)
(make-variable-buffer-local 'bouncy-scroll---column-before-scrolling)

(defvar bouncy-scroll--jump-to-prev-pos-from-ends-when-going-in-the-opposite-direction t)
(setq next-screen-context-lines 0)


(comment

 (let ((existing-overlays (overlays-in (point-max) (point-max))))
     (dolist (next-overlay existing-overlays)
       (if (overlay-get next-overlay 'eob-overlay)
	   (delete-overlay next-overlay))))

 
 (defun my-mark-eob ()
   (let ((existing-overlays (overlays-in (point-max) (point-max)))
	     (eob-mark (make-overlay (point-max) (point-max) nil t t))
	     (eob-text "--- eof---"))
     ;; Delete any previous EOB markers.  Necessary so that they don't
     ;; accumulate on calls to revert-buffer.
     (dolist (next-overlay existing-overlays)
       (if (overlay-get next-overlay 'eob-overlay)
	       (delete-overlay next-overlay)))
     ;; Add a new EOB marker.
     (put-text-property 0 (length eob-text)
			            'face '(:foreground "grey50" :background "grey80" :extend t) eob-text)
     (overlay-put eob-mark 'eob-overlay t)
     (overlay-put eob-mark 'after-string eob-text))
   )
 ;; (add-hook 'find-file-hooks 'my-mark-eob)



 (let ((existing-overlays (overlays-in (point-min) (point-min))))
     (dolist (next-overlay existing-overlays)
       (if (overlay-get next-overlay 'bob-overlay)
	       (delete-overlay next-overlay))))


 (let ((existing-overlays (overlays-in (point-min) (point-min)))
	     (bob-mark (make-overlay (point-min) (point-min) nil t t))
	     (bob-text "--- bof---"))
     (dolist (next-overlay existing-overlays)
       (if (overlay-get next-overlay 'bob-overlay)
	       (delete-overlay next-overlay)))
     (put-text-property 0 (length bob-text)
			            'face '(:foreground "grey50" :background "grey95" :extend t) bob-text)
     (overlay-put bob-mark 'bob-overlay t)
     (overlay-put bob-mark 'before-string bob-text))
 
 )

(defun bouncy-scroll (direction)        ; page down
  (cl-multiple-value-bind (point-min---if-up
                           bouncy-scroll-down---if-up                ; binding all at once, instead of having a lot of (if (eq direction 'up)) scattered below
                           scroll-up---if-up            ; just pretend you're reading the blocks below about scrolling up
                           point-max---if-up            ; scrolling down is symmetrical, the logic is inversed
                           end-of-buffer---if-up)
      (if (eq direction 'up)
          (list (point-min) #'bouncy-scroll-down #'scroll-up (point-max) #'end-of-buffer)
        (list (point-max) #'bouncy-scroll-up #'scroll-down (point-min) #'beginning-of-buffer))
    (let ((col (current-column))
          (last-command-was-bouncy-scroll-up-or-down (or (eq last-command #'bouncy-scroll-up)
                                                         (eq last-command #'bouncy-scroll-down)))
          (point-before-scrolled (point))
          (error t))
      (unwind-protect
          (prog1               ; this block is for scrolling before encountering the end of buffer
              (progn
                ;; (let ((visible-lines (count-lines (window-start) (buffer-size))))
                ;;   (unless (< visible-lines (window-text-height))       ; (window-body-height)
                ;;     (funcall scroll-up---if-up)))
                (unless last-command-was-bouncy-scroll-up-or-down                   ; preserve column
                  (setq bouncy-scroll---column-before-scrolling (current-column)))
                (if (and
                     (= (line-number-at-pos) (line-number-at-pos point-min---if-up))    ; in case of the beginning of buffer jump to next page at the last position
                     (eq last-command bouncy-scroll-down---if-up)
                     bouncy-scroll---last-pos)
                    (goto-char bouncy-scroll---last-pos)
                  (when bouncy-scroll--jump-to-prev-pos-from-ends-when-going-in-the-opposite-direction
                    (funcall scroll-up---if-up)))
                (unless bouncy-scroll--jump-to-prev-pos-from-ends-when-going-in-the-opposite-direction
                  (funcall scroll-up---if-up))
                (unless (= (line-number-at-pos) (line-number-at-pos point-max---if-up))
                  (setq bouncy-scroll---last-pos (point)))
                (when last-command-was-bouncy-scroll-up-or-down
                  (move-to-column bouncy-scroll---column-before-scrolling))
                ;; (when (= (line-number-at-pos) (line-number-at-pos point-max---if-up))
                ;;   (let ((recenter-positions '(bottom)))                             ;; (set-window-start nil bouncy-scroll---last-pos)
                ;;     (recenter-top-bottom)
                ;;     (when bouncy-scroll---last-pos (goto-char bouncy-scroll---last-pos))))
                ;; (when (= point-before-scrolled (point))
                ;;   (funcall scroll-up---if-up))
                )
            (setq error nil))
        (when error             ; this block is about encountering the end of buffer, jumping back and forth from end of buffer to the last position
          (if (= (line-number-at-pos) (line-number-at-pos point-max---if-up))
              (if (eq last-command this-command)         
                  (when bouncy-scroll---last-pos (goto-char bouncy-scroll---last-pos))
                (setq bouncy-scroll---last-pos (point)))
            (setq bouncy-scroll---last-pos (point))
            (beginning-of-line (funcall end-of-buffer---if-up))))))))

(defun bouncy-scroll-up ()    (interactive) (bouncy-scroll 'up))
(defun bouncy-scroll-down ()  (interactive) (bouncy-scroll 'down))



(ym-define-key (kbd "s-,") (lambda () (interactive "^") (scroll-up-command   3)))
(ym-define-key (kbd "s-.") (lambda () (interactive "^") (scroll-down-command 3)))
(ym-define-key (kbd "s-m") #'bouncy-scroll-down)        ; page up
(ym-define-key (kbd "s-n") #'bouncy-scroll-up)          ; page down
;; (ym-define-key (kbd "s-m") #'scroll-down-command)        ; page up
;; (ym-define-key (kbd "s-n") #'scroll-up-command)          ; page down
;; (ym-define-key (kbd "s-m") (lambda () (interactive) (ignore-errors (scroll-down-command))))        ; page up
;; (ym-define-key (kbd "s-n") (lambda () (interactive)
                             
;;                                  (scroll-up-command)))


;; (ym-define-key (kbd "s-n") (lambda () (interactive "^") (scroll-up)))    ; ^ is for leaving selection intact
;; (ym-define-key (kbd "s-h") (lambda () (interactive "^") (scroll-down)))
;; (ym-define-key (kbd "s-,") (lambda () (interactive "^") (recenter
;;                                                          ;; (floor (* (window-height) 0.2z))
;;                                                          )))



;; (use-package vi-tilde-fringe
;;   :config
;;   (global-vi-tilde-fringe-mode -1)
;;   )

(use-package topspace
  :config
  (setq-default indicate-empty-lines t)
  (setq topspace-autocenter-buffers nil)
  (global-topspace-mode 1)
  )


(ym-define-key (kbd "H-s-*") (kbd "s-*"))
(ym-define-key (kbd "H-s-&") (kbd "s-&"))
(ym-define-key (kbd "H-s-(") (kbd "s-("))

(ym-define-key (kbd "H-s-i") 'scroll-down-command)
(ym-define-key (kbd "H-s-k") 'scroll-up-command)

;; (ym-define-key (kbd "s-*") (lambda () (interactive "^") (previous-line 24)))
;; (ym-define-key (kbd "s-,") (lambda () (interactive "^") (next-line 24)))
;; (ym-define-key (kbd "H-s-i") (lambda () (interactive "^") (previous-line 24)))
;; (ym-define-key (kbd "H-s-k") (lambda () (interactive "^") (next-line 24)))
;; (ym-define-key (kbd "H-s-j") (lambda () (interactive "^") (backward-char 4)))
;; (ym-define-key (kbd "H-s-l") (lambda () (interactive "^") (forward-char 4)))

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
;; (ym-define-key (kbd "s-d") #'ym-duplicate-current-line-or-region)

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
;; (ym-define-key (kbd "s-D") #'ym-duplicate-and-comment-current-line-or-region)


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
;; (ym-define-key (kbd "s--") #'text-scale-decrease)
;; (ym-define-key (kbd "s-=") #'text-scale-increase)
;; (ym-define-key (kbd "s-0") (lambda () (interactive) (text-scale-adjust 0)))

;; -------------------------------------------------------------------

;; (ym-define-key (kbd "s-p") 'projectile-command-map)

;; -------------------------------------------------------------------

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;; -------------------------------------------------------------------

;; we turn off our keybindings-only minor mode in minibuffer
;; but the very same keybindings still work in global keymap
;; so ido takes precedxence in minibuffer
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
;; (ym-define-key (kbd "s-;") #'avy-goto-word-2)
;; (ym-define-key (kbd "s-;") #'avy-goto-char-timer)
;; (ym-define-key (kbd "s-^") #'avy-goto-parens)   ; "S-s-;" -- this is not a usual ^, it's a unicode character

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
;; (ym-define-key (kbd "s-s") 'ym-search-selection-or-isearch-forward)
;; (ym-define-key (kbd "s-r") 'ym-search-selection-or-isearch-backward)
;; (define-key isearch-mode-map (kbd "s-s") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "s-r") 'isearch-repeat-backward)




;; -------------------------------------------------------------------




;; -------------------------------------------------------------------






