;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; KEYBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dont-kill-emacs ()   ;; disable C-x C-c
  (interactive)
  (message (substitute-command-keys "To exit emacs: \\[save-buffers-kill-emacs]")))
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(setq cua-rectangle-mark-key [C-c\ C-q\ C-w\ C-e\ C-r\ C-t\ C-y])   ; was C-RET -- disabled now
(cua-mode t)   ; CUA mode: C-x, C-c, C-v for copying, pasting, C-z for undo
(transient-mark-mode 1)   ; No region when it is not highlighted
(setq cua-keep-region-after-copy t)   ; Standard Windows behaviour


;; -------------------------------------------------------------------
;; distinguish TAB and C-i, RET and C-m
;; internally Ctrl+i and Tab are both rerpresented by ascii 9
;;
;; distinguish C-i and TAB -- I don't know how it actualy works
;;    now tab-key is (kbd "TAB")
;;       and C-i is [tab]
(define-key function-key-map [tab] nil) ;; 1. Don't translate tab into C-i
(define-key key-translation-map [9] [tab]) ;; 2. Swap the meanings of tab and C-i 
(define-key key-translation-map [tab] [9])
(global-set-key [tab] (lambda () (interactive) (message "C-i is undefined")))
(global-set-key (kbd "S-TAB") (lambda () (interactive) (message "C-S-i is undefined")))
(global-set-key (kbd "TAB") 'indent-for-tab-command)

;; distinguish RET and C-m
;;    now return-key is (kbd "RET")
;;       and C-m is [return]
;; (define-key function-key-map [return] nil) ;; 1. Don't translate RET into C-m
;; (define-key key-translation-map [13] [return]) ;; 2. Swap the meanings of RET and C-m
;; (define-key key-translation-map [return] [13])
;; the code above worked up to emacs 23
(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\S-\C-m] [S-C-m])
(define-key input-decode-map [?\M-\S-\C-m] [M-S-C-m])
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-m] (lambda () (interactive)(message "C-m is undefined")))
(global-set-key [S-C-m] (lambda () (interactive)(message "S-C-m is undefined")))
(global-set-key [M-S-C-m] (lambda () (interactive)(message "M-S-C-m is undefined")))
;; [\C-return] (or C-RET) is now free, was cua-rectangle-mark, see init of CUA
;; -------------------------------------------------------------------



;; -------------------------------------------------------------------
;; russian keyboard
;; C-ф -> C-a
;; C-ы -> C-s
;; C-в -> C-d
;; C-а -> C-f
;; etc
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))
(defadvice read-passwd (around ym-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))
(reverse-input-method 'russian-computer)
;; -------------------------------------------------------------------



;; -------------------------------------------------------------------
(defvar ym-keys-minor-mode-map (make-keymap) "ym-keys-minor-mode keymap.")                           
(define-minor-mode ym-keys-minor-mode
  "My minor mode for global keybindings."
  :init-value t :lighter "" :keymap 'ym-keys-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (ym-keys-minor-mode 0)))
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
(mapcar   ; get rid of unused keybindings
 (lambda (x) (ym-define-key (kbd (concat "C-" (list x))) nil))
 "abdefjklnopqrstuwyz;$'\\,./?<>-")   ; C-i and C-m are handled above, z, x, c, v, g, h untouched
(mapcar
 (lambda (x) (ym-define-key (kbd (concat "M-" (list x))) nil))
 "abcdefghijklmnopqrstuvwyz;$'\\,./?<>-")   ; M-x untouched
(mapcar
 (lambda (x) (ym-define-key (kbd (concat "M-C-" (list x))) nil))
 "abcdefghijklmnopqrstuvwyxz;$'\\,./?<>-")   ; full alphabet
;; -------------------------------------------------------------------
;; mark   shift   activate   set
;; -      -       -          -
;; +      -       -          -
;; -      +       +          +
;; +      +       +          -
(defun ym-keys-ijkl-move (func shift-pressed) 
  (interactive) 
  (when (and 
         (not mark-active)
         shift-pressed)
    (cua-set-mark))
  (setq mark-active shift-pressed)
  (funcall func))
;; -------------------------------------------------------------------
(ym-define-key [tab] (lambda () (interactive) (ym-keys-ijkl-move 'previous-line nil)))   ; C-i
(ym-define-key (kbd "S-TAB") (lambda () (interactive) (ym-keys-ijkl-move 'previous-line t)))   ; C-i with shift key pressed, useful for marking a region
(ym-define-key (kbd "C-k") (lambda () (interactive) (ym-keys-ijkl-move 'next-line nil)))
(ym-define-key (kbd "S-C-k") (lambda () (interactive) (ym-keys-ijkl-move 'next-line t)))
(ym-define-key (kbd "C-j") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char nil)))
(ym-define-key (kbd "S-C-j") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char t)))
(define-key view-mode-map (kbd "C-j") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char nil)))
(define-key view-mode-map (kbd "S-C-j") (lambda () (interactive) (ym-keys-ijkl-move 'backward-char t)))
(ym-define-key (kbd "C-l") (lambda () (interactive) (ym-keys-ijkl-move 'forward-char nil)))
(ym-define-key (kbd "S-C-l") (lambda () (interactive) (ym-keys-ijkl-move 'forward-char t)))
(ym-define-key (kbd "C-u") (lambda () (interactive) (ym-keys-ijkl-move 'ym-go-to-beginning-of-code-or-line nil)))
(ym-define-key (kbd "S-C-u") (lambda () (interactive) (ym-keys-ijkl-move 'ym-go-to-beginning-of-code-or-line t)))
(ym-define-key (kbd "C-o") (lambda () (interactive) (ym-keys-ijkl-move 'ym-go-to-end-of-code-or-line nil)))
(ym-define-key (kbd "S-C-o") (lambda () (interactive) (ym-keys-ijkl-move 'ym-go-to-end-of-code-or-line t)))
(ym-define-key (kbd "M-C-i") (lambda () (interactive) (ym-keys-ijkl-move 'cua-scroll-down nil)))
(ym-define-key (kbd "S-M-C-i") (lambda () (interactive) (ym-keys-ijkl-move 'cua-scroll-down t)))
(ym-define-key (kbd "M-C-k") (lambda () (interactive) (ym-keys-ijkl-move 'cua-scroll-up nil)))
(ym-define-key (kbd "S-M-C-k") (lambda () (interactive) (ym-keys-ijkl-move 'cua-scroll-up t)))
(ym-define-key (kbd "M-C-k") (lambda () (interactive) (ym-keys-ijkl-move 'cua-scroll-up nil)))
(ym-define-key (kbd "M-C-j") (lambda () (interactive) (ym-keys-ijkl-move 'beginning-of-buffer nil)))
(ym-define-key (kbd "M-C-l") (lambda () (interactive) (ym-keys-ijkl-move 'end-of-buffer nil)))
(delete '(10 . exit-minibuffer) minibuffer-local-completion-map)   ; 10 = C-j
(define-key minibuffer-local-completion-map (kbd "C-j") (lambda () (interactive) (ym-keys-ijkl-move 'left-char nil)))
(define-key minibuffer-local-completion-map (kbd "C-l") (lambda () (interactive) (ym-keys-ijkl-move 'right-char nil)))
(define-key isearch-mode-map [tab] (lambda () (interactive) (isearch-exit) (ym-keys-ijkl-move 'previous-line nil)))
(define-key isearch-mode-map (kbd "S-TAB") (lambda () (interactive) (isearch-exit) (ym-keys-ijkl-move 'previous-line t)))
(ym-define-key (kbd "C-S-<backspace>") 'ym-delete-line)
(ym-define-key (kbd "M-S-<backspace>") 'kill-whole-line)
(ym-define-key (kbd "C-SPC") (lambda () (interactive) (other-window 1)))
(ym-define-key (kbd "C-1") 'delete-other-windows)
(ym-define-key (kbd "C-2") 'split-window-vertically)
(ym-define-key (kbd "C-3") 'split-window-horizontally)
(ym-define-key (kbd "C-0") 'delete-window)
(ym-define-key (kbd "C-f") 'ym-search-selection-or-isearch-forward)
(ym-define-key (kbd "C-S-f") 'ym-search-selection-or-isearch-backward)
(ym-define-key (kbd "C-M-f") 'isearch-forward-regexp)
(ym-define-key (kbd "C-M-S-f") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-M-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-M-S-f") 'isearch-repeat-backward)
(ym-define-key (kbd "C-s") 'save-buffer)   ; save with C-s
(ym-define-key (kbd "C-r") 'revert-buffer)       
(ym-define-key (kbd "C-w") (lambda () (interactive) (kill-buffer (current-buffer))))
(ym-define-key (kbd "C-b") 'ido-switch-buffer)
(ym-define-key (kbd "C-S-b") 'ibuffer)
(ym-define-key (kbd "C-d") 'ym-duplicate-current-line-or-region)
(ym-define-key (kbd "C-e") 'ym-ido-ffap)   ; open with C-e -- see ido C-e below
(ym-define-key (kbd "C-S-e") 'ym-dired-current-buffer-dir)
(ym-define-key (kbd "C-S-e") 'ym-dired-current-buffer-dir)
(ym-define-key (kbd "C-/") 'ym-comment-or-uncomment-region-or-line)
(ym-define-key (kbd "C-<backspace>") 'ym-backward-kill-word)
(ym-define-key (kbd "M-<backspace>") 'ym-backward-kill-word)
(ym-define-key (kbd "M-.") 'ym-find-tag-at-point)
;; (ym-define-key (kbd "M-/") ')
(ym-define-key (kbd "M-,") 'pop-tag-mark)
(ym-define-key (kbd "<C-f1>") 'ym-org-day-view)
(ym-define-key (kbd "<f1>") 'ym-org-working)
(ym-define-key (kbd "S-<f1>") 'ym-org-week-view)
(ym-define-key (kbd "M-<f1>") 'ym-org-todo-view)
(ym-define-key (kbd "S-M-<f1>") 'ym-org-todo-match-tags-view)
(ym-define-key (kbd "C-S-M-<f1>") 'ym-org-todo-match-tags-view-with-done-tasks)
(ym-define-key (kbd "<f2>") (lambda () (interactive) (org-capture nil "t")))
(ym-define-key (kbd "S-<f2>") (lambda () (interactive) (org-capture nil "T")))
(ym-define-key (kbd "<f3>") (lambda () (interactive) (ym-org-datetree-find-today org-default-notes-file)))
(ym-define-key (kbd "M-<f3>") 'ym-org-journal-view)
(ym-define-key (kbd "<f4>") 'ym-ledger-view)
(ym-define-key (kbd "M-<f4>") (lambda () (interactive)   (ym-ledger-view nil "--period-sort t --monthly --begin march -T '0'    reg Expences Liabilities")))
(ym-define-key (kbd "S-M-<f4>") (lambda () (interactive) (ym-ledger-view nil "--period-sort t --monthly --begin march -T '0' -n reg Expences Liabilities")))
(ym-define-key (kbd "S-<f4>") (lambda () (interactive) (ym-ledger-view t)))
(ym-define-key (kbd "<f5>") (lambda () (interactive) (ym-org-clock-summary)))
(ym-define-key (kbd "S-<f5>") (lambda () (interactive) (ym-org-clock-summary 7)))
(ym-define-key (kbd "M-<f5>") 'ym-org-problems-count-view)
(ym-define-key (kbd "<f6>") 'ym-org-contacts-view)
(ym-define-key (kbd "C-<left>")     'winner-undo)
(ym-define-key (kbd "C-<right>")    'winner-redo)
(ym-define-key (kbd "M-x") 'smex)
(ym-define-key (kbd "C-`") 'other-frame)
;; (ym-define-key (kbd "C-c '") 'narrow-or-widen-dwim)
;; (ym-define-key (kbd "<left>") (lambda () (interactive) (ignore-errors (windmove-left))))
;; (ym-define-key (kbd "<right>") (lambda () (interactive) (ignore-errors (windmove-right))))
;; (ym-define-key (kbd "<up>") (lambda () (interactive) (ignore-errors (windmove-up))))
;; (ym-define-key (kbd "<down>") (lambda () (interactive) (ignore-errors (windmove-down))))
;; -------------------------------------------------------------------
;; (define-key etags-select-mode-map (kbd "RET") 'etags-select-goto-tag)
(define-key dired-mode-map "\M-o" 'ym-toggle-dired-omit-details)
(define-key dired-mode-map (kbd "<backspace>") (lambda () (interactive) (dired (concat dired-directory ".."))))
(add-hook 'help-mode-hook (lambda () (define-key help-mode-map "b" 'help-go-back)))
(add-hook 'help-mode-hook (lambda () (define-key help-mode-map "f" 'help-go-forward)))
;; -------------------------------------------------------------------
(defadvice ido-init-completion-maps (after ido-init-completion-maps-with-ym-keybindings activate)
  "My keybindings in ido."
  (define-key ido-common-completion-map (kbd "C-j") 'ido-prev-match)
  (define-key ido-common-completion-map (kbd "C-l") 'ido-next-match)
  (define-key ido-file-completion-map (kbd "C-l") 'ido-next-match)
  (define-key ido-file-completion-map (kbd "C-j") 'ido-prev-match)
  (define-key ido-file-dir-completion-map (kbd "C-l") 'ido-next-match)
  (define-key ido-file-dir-completion-map (kbd "C-j") 'ido-prev-match)
  (define-key ido-buffer-completion-map (kbd "C-j") 'ido-prev-match)
  (define-key ido-buffer-completion-map (kbd "C-l") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-f") 'ido-fallback-command)
  (define-key ido-common-completion-map (kbd "C-r") 'ido-toggle-regexp)
  (define-key ido-file-dir-completion-map (kbd "C-e") 'ym-ido-merge-or-unmerge-work-directories)
  (define-key ido-file-completion-map [\C-return] 'ido-enter-dired)
  (define-key ido-file-dir-completion-map [\C-return] 'ido-enter-dired)
  )
(ido-init-completion-maps)
;; -------------------------------------------------------------------
(define-key org-agenda-mode-map (kbd "n") 'ym-org-agenda-view-goto-now-or-to-next-actions)
(define-key org-agenda-mode-map (kbd "т") 'ym-org-agenda-view-goto-now-or-to-next-actions)
(define-key org-agenda-mode-map (kbd "k") nil)
(define-key org-agenda-mode-map (kbd "л") nil)
(define-key org-agenda-mode-map (kbd "h") 'ym-org-agenda-hide-line-temporarily)
;; (define-key org-agenda-mode-map (kbd "H") 'ym-org-agenda-hide--lines-temporarily)
(define-key org-agenda-mode-map (kbd "р") 'ym-org-agenda-hide-line-temporarily)
(define-key org-mode-map (kbd "C-c C-x C-t") 'ym-org-insert-inline-task)
(define-key org-mode-map (kbd "RET")     (lambda () (interactive) (org-return) (indent-for-tab-command)))   ; org-return-indent behaves strangely
(define-key org-mode-map [(shift tab)]     (lambda () (interactive) (org-shifttab (- org-inlinetask-min-level 1))))
;; -------------------------------------------------------------------

(defun ym-org-agenda-later ()
  (interactive)
  (beginning-of-buffer)
  (next-line)
  (org-agenda-later 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; -------------------------------------------------------------------
(defun ym-org-insert-inline-task ()
  (interactive)
  (insert "
*************** TODO                                                 :inline:
                :LOGBOOK:
                - Added ")
  (org-insert-time-stamp nil t t)
  (insert "
                :END:
*************** END
")
  (forward-line -5)
  (beginning-of-line)
  (forward-char 21)
  )
;; -------------------------------------------------------------------
(defun ym-org-day-view ()
  (interactive)
  (org-agenda nil "1") ; see org-agenda-custom-commands
  (ym-org-day-view-aux-go-to-now-header)
  (delete-other-windows))
(defun ym-org-working ()
  (interactive)
  (org-agenda nil "0") ; see org-agenda-custom-commands
  (forward-line)
  (delete-other-windows))
(defun ym-org-week-view ()
  (interactive)
  (org-agenda nil "2") ; see org-agenda-custom-commands
  (end-of-buffer)
  (search-backward org-agenda-current-time-string nil t)
  (forward-line)
  (beginning-of-line)
  (delete-other-windows))
(defun ym-org-todo-view ()
  (interactive)
  (org-agenda nil "3") ; see org-agenda-custom-commands
  (search-forward-regexp (concat "^" ym-org-agenda-planning-header-non-scheduled "$"))
  (forward-line)
  (beginning-of-line)
  (delete-other-windows))
(defun ym-org-todo-match-tags-view ()
  (interactive)
  (org-agenda nil "M")
  (delete-other-windows))
(defun ym-org-todo-match-tags-view-with-done-tasks ()
  (interactive)
  (org-agenda nil "m")
  (delete-other-windows))
(defun ym-org-journal-view ()
  (interactive)
  (ym-org-datetree-find-today ym-org-journal-file)
  (message "  --- What good things happened to you today?\n  --- What is the most important thing for tomorrow?\n  --- What can i do for my family and friends?\n"))
(defun ym-org-contacts-view ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer ym-org-contacts-view-buffer-name))
(defun ym-org-problems-count-view ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer ym-org-problems-count-view-buffer-name))
(defun ym-org-datetree-find-today (file)
  (interactive)
  (let* ((non-blank-line-regex "^.*\\S-.*$")
         (date (decode-time))
         (month (nth 4 date))
         (day (nth 3 date))
         (year (nth 5 date)))
    (delete-other-windows)
    (find-file file)
    (end-of-buffer)
    (org-datetree-find-date-create (list month day year))
    (save-excursion
      (forward-line -1)
      (let ((point (point)))
        (when
            (string-match
             non-blank-line-regex
             (buffer-substring point (progn (end-of-line) (point)))
             )
          (newline-and-indent)
          )))
    (org-show-subtree)
    (if (not (org-goto-first-child))
        (org-end-of-subtree))
    (search-backward-regexp non-blank-line-regex nil t)
    (forward-line 1)
    (end-of-line)
    (if (eq (point) (point-max))
        (progn 
          (newline-and-indent)
          (forward-line -1))
      )
    (indent-for-tab-command)
    ))
(defun ym-ledger-view (&optional ask-for-command cmd)
  "Show balance using ledger utility."
  (interactive)
  (find-file ym-ledger-file)
  (delete-other-windows)
  (save-buffer)
  (let ((command (if cmd cmd "-s balance Assets Liabilities Savings")))   ; -s --- show sub-accounts
    (shell-command-on-region
     (point-min)
     (point-max)
     (concat "ledger -f - "
             (if (not ask-for-command)
                 command
               (read-string "ledger command: " command))))))
;; -------------------------------------------------------------------
(defun ym-org-clock-summary (&optional number-of-days)
  (interactive)
  (delete-other-windows)
  (switch-to-buffer ym-org-clock-buffer-name)
  (view-mode -1)
  (erase-buffer)
  (org-mode)
  (insert
   (concat "#+BEGIN: clocktable "
           (if (null number-of-days)
               ":block today "
             (concat ":tstart \""
                     (ym-org-clocktable--aux--today-string-plus-days (- number-of-days))
                     "\" :tend \""
                     (ym-org-clocktable--aux--today-string-plus-days 1)
                     "\" :step day"))
           " :scope agenda :properties (\"Effort\") :maxlevel 20 ::tcolumns 3 :link :indent\n#+END: clocktable\n"))
  (beginning-of-buffer)
  (org-clock-report)
  (flush-lines "\*0:00\*")
  (flush-lines "-|\n|[^ ]")
  (beginning-of-buffer)
  (let ((total-time-string "\*Total time\*"))
    (while (search-forward-regexp total-time-string nil t)
      (ym-add-overlay-to-line 'ym-org-clock-summary-total-time-line-highlight))
    (end-of-buffer)
    (search-backward-regexp "^Daily report:" nil t)
    (search-forward-regexp total-time-string nil t))
  (view-mode 1))
(defun ym-org-clocktable--aux--today-string-plus-days (days)
  (format-time-string "<%Y-%m-%d %a>" (time-add (current-time) (days-to-time days))))
(defun ym-clock-in-and-notify (&optional emacs-is-active)    ; for growl or notification center
  (interactive)
  (with-current-buffer (window-buffer (selected-window))   ; these functions are invoked by emacsclient
    (unless (and emacs-is-active (eq major-mode 'org-agenda-mode) (ignore-errors (org-agenda-clock-in)))
      (org-clock-in-last))
    org-clock-current-task))
(defun ym-clock-in-default-and-notify ()
  (interactive)
  (org-with-point-at (org-id-find "default" 'marker)
    (org-clock-in))
  org-clock-current-task)
(defun ym-clock-out-and-notify ()
  (interactive)
  (with-current-buffer (window-buffer (selected-window))
    (let ((old-current org-clock-current-task))
      (ignore-errors (org-agenda-clock-out))
      (if old-current
          old-current
        "")
      )))
;; (defun ym-org-clock-in-ask-effort ()
;;   "Ask for an effort estimate when clocking in."
;;   (unless (org-entry-get (point) "Effort")
;;     (org-set-effort))
;;   (when (string= (org-get-effort) "0:00")
;;     (org-delete-property "Effort")))
;; ;; (add-hook 'org-clock-in-prepare-hook 'ym-org-clock-in-ask-effort)
;; (define-key org-mode-map (kbd "M-I") 'ym-org-clock-in-ask-effort)
(defun ym-clock-show-current ()
  (interactive)
  (delete-other-windows)
  (org-clock-goto)
  (show-entry)
  (search-forward ":END:")
  (forward-line -1))
;; -------------------------------------------------------------------
(defun ym-org-agenda-hide-line-temporarily (&optional do-not-leave-blank)
  (interactive)
  (overlay-put
   (make-overlay
    (line-beginning-position)
    (+ (line-end-position)
       (if do-not-leave-blank 1 0)
       ))
   'invisible t)
  (forward-line)
  )
;; (defun ym-org-agenda-hide--lines-temporarily ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (while (search-forward-regexp "\\[\\#C\\]" nil t) ; :hide:
;;       (ym-org-agenda-hide-line-temporarily))
;;     )
;;   )
;; -------------------------------------------------------------------
(defun ym-org-agenda-view-goto-now-or-to-next-actions ()
  (interactive)
  (beginning-of-buffer)
  (search-forward-regexp "^=+\nTop:" nil t)
  (forward-line)
)
;;;;;;;;;;;;;
;; (defun ym-org-agenda-view-goto-now-or-to-next-actions ()
;;   (interactive)
;;   (if (ignore-errors (search-forward-regexp "^=+\n[[:alpha:][:space:]]+:"))
;;       (forward-line)
;;     (beginning-of-buffer)
;;     (ignore-errors (search-forward-regexp "^=+\n[[:alpha:][:space:]]+:"))
;;     (forward-line))
;;   )
;;;;;;;;;;;;;;;;;;;
;; (defun ym-org-agenda-view-goto-now-or-to-next-actions ()
;;   (interactive)
;;   (let ((point (point)))
;;     (beginning-of-buffer)
;;     (search-forward org-agenda-current-time-string nil t)
;;     (beginning-of-line)
;;     (when (= point (point))
;;       (ym-org-day-view-aux-go-to-now-header))))
(defun ym-org-day-view-aux-go-to-now-header ()
  (search-forward-regexp (concat "^" ym-org-agenda-planning-header-now "$") nil t)
  (next-line)
  (beginning-of-line))
;; -------------------------------------------------------------------




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; -------------------------------------------------------------------
(defun ym-backward-kill-word ()
  "Similar to backward-kill-word, but treats newline as a word."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (delete-region (point)
                   (max (save-excursion (beginning-of-line) (point))
                        (save-excursion (backward-word) (point))))))

(defun ym-comment-or-uncomment-region-or-line ()
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

(defun ym-ido-ffap ()
  (interactive)
  (if (eq 'dired-mode (with-current-buffer (current-buffer) major-mode))
        (set (make-local-variable 'ido-use-filename-at-point) nil))
  (ido-find-file))

(defun ym-ido-merge-or-unmerge-work-directories ()
  (interactive)
  (if (not (and (boundp 'ido-pre-merge-state) ido-pre-merge-state))
      (ido-merge-work-directories)
    (ido-undo-merge-work-directory)))

(defun ym-toggle-dired-omit-details ()
  "Toggle dired-omit and dired-details simultaneously."
  (interactive)
  (if (eq dired-details-state 'shown)
      (progn (dired-omit-mode 1)
             (dired-details-hide))
    (progn (dired-omit-mode -1)
           (dired-details-show))))

;; (defun ym-dired-at-point-or-current-buffer-dir ()
;;   (interactive)
;;   (if (eq (with-current-buffer (current-buffer) major-mode) 'dired-mode)
;;       (dired-at-point)
;;     (dired default-directory)))

(defun ym-dired-current-buffer-dir ()
  (interactive)
  (if (eq (with-current-buffer (current-buffer) major-mode) 'dired-mode)
      (ido-dired)
    (dired default-directory)))

(defun ym-go-to-beginning-of-code-or-line ()   ; http://emacswiki.org/emacs/BackToIndentationOrBeginning
  "Move to the beginning of code or to the beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun ym-go-to-end-of-code-or-line (&optional arg)
  "Move to the end of code or to the end of line."
  (interactive "P")
  (flet ((point-in-comment ()
                           "Determine if the point is inside a comment"
                           (interactive)
                           (let ((syn (syntax-ppss)))
                             (and (nth 8 syn)
                                  (not (nth 3 syn))))))
    (let ((eoc (save-excursion
                 (move-end-of-line arg)
                 (while (point-in-comment)
                   (backward-char))
                 (skip-chars-backward " \t")
                 (point))))
      (cond ((= (point) eoc)
             (move-end-of-line arg))
            (t
             (move-end-of-line arg)
             (while (point-in-comment)
               (backward-char))
             (skip-chars-backward " \t"))))))

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

(defun ym-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (delete-region
   (save-excursion (move-beginning-of-line 1) (point))
   (save-excursion (move-beginning-of-line 2) (point))))

(defun ym-search-selection-or-isearch (forward)
  (interactive)
  "search for selected text"
  (let* ((beg (point))
         (end (mark))
         (selection (buffer-substring-no-properties beg end)))
    (deactivate-mark)
    (isearch-mode forward nil nil nil)
    (isearch-yank-string selection)))  
(defun ym-search-selection-or-isearch-forward  () (interactive) (if mark-active (ym-search-selection-or-isearch t)   (isearch-forward)))
(defun ym-search-selection-or-isearch-backward () (interactive) (if mark-active (ym-search-selection-or-isearch nil) (isearch-backward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun narrow-or-widen-dwim (p)   ; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((org-in-src-block-p)
                (org-edit-src-code)
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))









