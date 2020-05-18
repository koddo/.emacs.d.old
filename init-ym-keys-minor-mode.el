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

;; we have to bind shift-meta-something to anything non-nil
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
(ym-define-key (kbd "<S-SPC>")    #'ym-undefined-key-message)

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

;; -------------------------------------------------------------------

;; (ym-define-key (kbd "M-s-^") #'ym-undefined-key-message)   ; M-s-i
;; (ym-define-key (kbd "M-s-˚") #'ym-undefined-key-message)   ; M-s-k
;; (ym-define-key (kbd "M-s-∆") #'ym-undefined-key-message)   ; M-s-k
;; (ym-define-key (kbd "M-s-¬") #'ym-undefined-key-message)   ; M-s-l

;; -------------------------------------------------------------------

(ym-define-key (kbd "s-z") #'undo)
(ym-define-key (kbd "s-x")  (lambda (beg end) (interactive "r") (prog1  (kill-region beg end)     (setq deactivate-mark nil))))
(ym-define-key (kbd "s-c")  (lambda (beg end) (interactive "r") (prog1  (kill-ring-save beg end)  (setq deactivate-mark nil))))
(ym-define-key (kbd "s-v") #'yank)



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

;; -------------------------------------------------------------------

(ym-define-key (kbd "s--") #'text-scale-decrease)
(ym-define-key (kbd "s-=") #'text-scale-increase)
(ym-define-key (kbd "s-0") (lambda () (interactive) (text-scale-adjust 0)))

;; -------------------------------------------------------------------











