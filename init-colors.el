;; -*- lexical-binding: t; -*-







;; -------------------------------------------------------------------

;; TODO: move everything from custom.el

;; https://tech.toryanderson.com/2020/11/13/migrating-to-a-custom-file-less-setup/


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------


  (progn 
    ;; (print ym-base16-colors-darker)
    ;; (:base00 "white" :base01 "grey88" :base02 "#d8d8d8" :base03 "grey70" :base04 "#585858" :base05 "grey10" :base06 "#282828" :base07 "#181818" :base08 "#ab4642" :base09 "#b16724" :base0A "#a57a3a" :base0B "#718342" :base0C "#4f9b91" :base0D "#47839a" :base0E "#925684" :base0F "#a16946")
    
    ;; (defun asdf (cc)
    ;;   (apply 'color-rgb-to-hex `(,@(color-name-to-rgb (color-darken-name cc 18)) 2)))
    ;; (asdf "#925684")
    )





;; (setq-default mode-line-format
;;               (let* ((left-side '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   "))
;;                      (right-side '(mode-line-position
;;                                    (vc-mode vc-mode)
;;                                    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
;;                      (r-length (length (format-mode-line right-side)))
;;                      (padding (lambda () (propertize " " 'display `(space :align-to (- right ,r-length))))))
;;                 `(,left-side
;;                   (:eval (padding))
;;                   ,right-side
;;                   ))
;;               )























;; agenda coloring, configure this later
;; (setq ym-org-todo-keywords-working-regexp
;; 	(concat "\\("
;; 		(mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-working "\\|")
;; 		"\\)"))
;; (setq ym-org-todo-keywords-undone-regexp
;; 	(concat ym-org-todo-state-string-in-log "\\("
;; 		(mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-undone "\\|")
;; 		"\\))"))
;; (setq ym-org-todo-keywords-done-regexp
;; 	(concat ym-org-todo-state-string-in-log "\\("
;; 		(mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-done "\\|")
;; 		"\\))"))



(custom-set-faces
 '(org-habit-clear-face                 ((t (:background "white"))) t)
 '(org-habit-clear-future-face          ((t (:background "white"))) t)
 '(org-habit-ready-face                 ((t (:background "white"))) t)
 '(org-habit-ready-future-face          ((t (:background "white"))) t)
 '(org-habit-alert-face                 ((t (:background "white"))) t)
 '(org-habit-alert-future-face          ((t (:background "white"))) t)
 '(org-habit-overdue-face               ((t (:background "white"))) t)
 '(org-habit-overdue-future-face        ((t (:background "white"))) t))


;; (set-face-attribute 'org-ql-view-due-date  nil :foreground "grey80" :slant 'normal :weight 'normal)
(defface org-ql-view-due-date '((t :foreground "grey60")) "")


;; fixed in org 9.5 -- https://protesilaos.com/codelog/2020-09-24-org-headings-adapt/
;; (setq org-todo-keyword-faces '(
;; 			       ("HABIT" . (:height 3.0))
;; 			       ))


;; maybe rename later to hi-something
(defface ym/org-double-backslash-newline-highlight '((t :foreground "grey80")) "double backslash at eol")
;; (set-face-attribute 'org-double-slash-newline-highlight nil :foreground "grey80")

(dolist (x '(avy-lead-face
	         avy-lead-face-0
	         avy-lead-face-1
	         avy-lead-face-2))
  (set-face-attribute x nil :foreground "white" :background "#dc9656")
  ;; (set-face-attribute x nil :foreground "white" :background "#e5b180")
  ;; (set-face-attribute x nil :foreground "white" :background "#eecbaa")
  )
;; (dolist (buf (buffer-list))
;;    (when (get-buffer-window buf 'visible) (....<STUFF>...))

;; (defface ym/habits-face '((t :family "Monaco" :height 80)) "")






;; -------------------------------------------------------------------
;; highlith the active window
;; or the other way around, dim all inactive windows

;; when you have one buffer in multiple windows, use indirect buffers: clone-indirect-buffer-other-window?

;; https://emacs.stackexchange.com/questions/24630/is-there-a-way-to-change-color-of-active-windows-fringe
;; https://stackoverflow.com/questions/47456134/emacs-lisp-hooks-for-detecting-change-of-active-buffer
;; (defun highlight-selected-window ()
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (let ((hydra-window lv-wnd))        ; this is the hydra echo area, see https://github.com/abo-abo/hydra/blob/master/lv.el
    (walk-windows (lambda (w)
                    (cond 
                     ((eq (window-buffer w) (window-buffer (selected-window)))         ; if you want to dim same buffers, use (eq w (selected-window))
                      (buffer-face-set 'default))
                     ((eq (window-buffer w) (window-buffer hydra-window))         ; saved for history: (eq w hydra-window)
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set '(:background "grey"))))
                     (t
                      (with-current-buffer (window-buffer w)
                        (buffer-face-set '(:background "grey92"))))
                     )))
    ))
(add-hook 'buffer-list-update-hook #'highlight-selected-window)
(add-hook 'window-configuration-change-hook #'highlight-selected-window)


;; see an alternative:
;; a package that apparently does the same
;; https://github.com/mina86/auto-dim-other-buffers.el

;; -------------------------------------------------------------------



;; -------------------------------------------------------------------


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

;; electric-pair-mode is enough most of the time
;; I don't use strict mode and soft deletion from puni and smartparens
(electric-pair-mode t)

(use-package puni
  ;; No configuration here. I just directly use functions from puni, smartparens, lispy, etc
  )

(use-package smartparens
  ;; :demand t
  ;; :diminish smartparens-mode smartparens-global-mode show-smartparens-mode show-smartparens-global-mode
  :config
  (require 'smartparens-config)   ; default configuration
  (setq sp-navigate-reindent-after-up-in-string nil)
  (setq sp-navigate-reindent-after-up nil)

  ;; I now use electric-pair-mode
  ;; (smartparens-global-mode 1)     ; used to be (smartparens-global-strict-mode 1), but I don't need it to be that strict
  ;; (show-smartparens-global-mode 1)
  
  ;; customize sp-show-pair-match-content-face if you want to highlight not only parens but also the content of the s-exp
  ;; '(sp-show-pair-enclosing ((t (:inherit show-paren-match))))  
  )


;; highlight matching parenthesis
(require 'paren)   ; I prefer stock show-paren-mode over show-smartparen-mode because it's ultra-fast
;; (setq show-paren-delay 0)
;; (setq show-paren-delay 0.1)
(setq show-paren-delay 0.01)
(show-paren-mode 1)
;; (show-paren-mode -1)
(setq show-paren-style 'parenthesis)
(copy-face 'default 'show-paren-match)
(set-face-attribute 'show-paren-match nil
                    :weight 'bold
                    :foreground "black"
                    :background "grey"         ; was ym-hl-line-color-normal-mode-color
                    )    ; inherited by show-paren-match-expression


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





