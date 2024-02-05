;; -*- lexical-binding: t; -*-


;; (rainbow-mode)


;; taken from stackoverflow
(defun ym/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))







;; -------------------------------------------------------------------

;; Colorize color names in buffers, like #0000ff or blue
(use-package rainbow-mode)

;; -------------------------------------------------------------------

;; http://chriskempson.com/projects/base16/
;; https://github.com/belak/base16-emacs
;; https://github.com/belak/base16-emacs/blob/master/base16-theme.el
(use-package base16-theme
  :after org
  :config
  (deftheme ym-base16-theme)

  (setq ym-base16-colors (list
			              :base00 "grey99"     ; Default Background
			              :base01 "grey88"     ; Lighter Background (Used for status bars)
			              :base02 "#d8d8d8"    ; Selection Background
			              :base03 "grey70"     ; Comments, Invisibles, Line Highlighting
			              :base04 "#585858"    ; Dark Foreground (Used for status bars)
			              :base05 "grey20"     ; Default Foreground, Caret, Delimiters, Operators
			              :base06 "#282828"    ; Light Foreground (Not often used)
			              :base07 "#181818"    ; Light Background (Not often used)
			              :base08 "#ab4642"    ; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
			              :base09 "#dc9656"    ; Integers, Boolean, Constants, XML Attributes, Markup Link Url
			              :base0A "#cca770"    ; Classes, Markup Bold, Search Text Background
			              :base0B "#a1b56c"    ; Strings, Inherited Class, Markup Code, Diff Inserted
			              :base0C "#86c1b9"    ; Support, Regular Expressions, Escape Characters, Markup Quotes
			              :base0D "#7cafc2"    ; Functions, Methods, Attribute IDs, Headings
			              :base0E "#ba8baf"    ; Keywords, Storage, Selector, Markup Italic, Diff Changed
			              :base0F "#a16946"    ; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
			              ))
  (setq ym-base16-colors-darker    ; from :base09 to :base0E
        (let ((percent-darker 33))
          (-map-indexed (lambda (ii cc)
                          (if
                              (and (> ii 18) (<= ii 30)
                                   (cl-oddp ii))
                              (apply 'color-rgb-to-hex `(,@(color-name-to-rgb (color-darken-name cc percent-darker)) 2))
                            cc))
                        ym-base16-colors)))

  (base16-theme-define 'ym-base16-theme ym-base16-colors-darker)
  (enable-theme 'ym-base16-theme)

  (progn 
    ;; (print ym-base16-colors-darker)
    ;; (:base00 "white" :base01 "grey88" :base02 "#d8d8d8" :base03 "grey70" :base04 "#585858" :base05 "grey10" :base06 "#282828" :base07 "#181818" :base08 "#ab4642" :base09 "#b16724" :base0A "#a57a3a" :base0B "#718342" :base0C "#4f9b91" :base0D "#47839a" :base0E "#925684" :base0F "#a16946")
    
    ;; (defun asdf (cc)
    ;;   (apply 'color-rgb-to-hex `(,@(color-name-to-rgb (color-darken-name cc 18)) 2)))
    ;; (asdf "#925684")
    )



  
  ;; ;; https://terminal.sexy/
  ;; (use-package base16-theme
  ;;   :config
  ;;   (load-theme 'base16-default-light t)
  ;;   ;; (load-theme 'base16-grayscale-light t)
  ;;   )

  (defun ym/toggle-color-of-comments ()
    (interactive)
    (let* ((comments-colors-togglable '("grey90" "grey70" "grey30"))
           (current-comments-color (face-attribute 'font-lock-comment-face :foreground))
           (next-color-in-list (cadr (member current-comments-color comments-colors-togglable)))
           (new-comments-color (if next-color-in-list
                                   next-color-in-list
                                 (car comments-colors-togglable)))
           )
      (set-face-attribute 'font-lock-comment-face nil :foreground new-comments-color)
      ;; temporarily fixing this: https://github.com/belak/base16-emacs/issues/114
      ;; font-lock-comment-delimiter-face should be base03, not base02
      (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground new-comments-color)
      ))

  )



(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "black"
                    :distant-foreground "white"
                    ;; :background "grey"
                    )
(set-face-attribute 'mode-line nil
                    :foreground "grey60"
                    :box (list
                          :color "grey22"
                          :line-width '(0 . 9)
                          )
                    :background "grey22"
                    )
(set-face-attribute 'mode-line-inactive nil
                    :foreground "grey60"
                    :box (list
                          :color "grey22"
                          :line-width '(1 . 2)
                          )
                    :background "grey90"
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

(defun ym/align-mode-line (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default
 mode-line-format
 '((:eval
    (ym/align-mode-line
     ;; Left.
     '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification)

     ;; Right.
     '("   "
       "%l:%c"         ; instead of mode-line-position
       ;; (vc-mode vc-mode)
       "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
     ))))


;; tab-bar behaviour and appearance: https://github.com/daviwil/emacs-from-scratch/blob/82f03806d90eb356b815cf514d10b6d863a2cbdc/show-notes/Emacs-Tips-06.org
;; tab-bar menu, and other arbitrary info in tab-bar: https://karthinks.com/software/a-tab-bar-menu-in-emacs/
;; https://lambdaland.org/posts/2022-07-20_adding_a_clock_to_emacs/

(set-face-attribute 'tab-bar nil
                    :foreground "red"
                    :box nil
                    ;; :box (list
                    ;;       :color "grey22"
                    ;;       :line-width '(0 . 9)
                    ;;       )
                    :background "grey80"
                    )

(set-face-attribute 'tab-bar-tab nil
                    :foreground "white"
                    :box (list
                          :color "grey55"
                          :line-width '(10 . 8)
                          )
                    :background "grey55"                    
                    )

(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "grey36"
                    :box (list
                          :color "grey72"
                          :line-width '(10 . 0)
                          )
                    :background "grey72"                    
                    )
;; tab-bar-tab-group-current
;; tab-bar-tab-group-inactive

;; to customize further, first do M-x describe-text-properties, then the following
(set-face-attribute 'org-special-keyword nil :foreground (plist-get ym-base16-colors :base03))
(set-face-attribute 'org-drawer nil :foreground (plist-get ym-base16-colors :base03))
(set-face-attribute 'org-date   nil :foreground (plist-get ym-base16-colors :base03))

(let ((f "#5c69cc")) ; "#0018ca"
  (set-face-attribute 'org-level-1 nil :height 5.0 :foreground f)  ; "#ae1200"
  (set-face-attribute 'org-level-2 nil :height 3.0 :foreground f)
  (set-face-attribute 'org-level-3 nil :height 1.5 :foreground f)
  (set-face-attribute 'org-level-4 nil :height 1.0 :foreground "#5c69cc")
  (set-face-attribute 'org-level-5 nil :height 1.0 :foreground "#9096c5")
					; (set-face-attribute 'org-level-6 nil :height 1.0 :foreground "#9096c5")
					; (set-face-attribute 'org-level-7 nil :height 1.0 :foreground "#9096c5")
					; (set-face-attribute 'org-level-8 nil :height 1.0 :foreground "#9096c5")
  )





(use-package rainbow-delimiters
  :config
  ;; (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 6)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick3"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "dodger blue"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "green3"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "peru"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "grey50"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "black"))))
   )
  )









(set-face-attribute 'ido-virtual nil :foreground (plist-get ym-base16-colors :base03))



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
;; (set-face-attribute 'avy-background-face nil :foreground "grey90" :background "grey98")
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
                   ((eq w (selected-window))       ; could be also (eq (window-buffer w) (window-buffer (selected-window)))
                    (buffer-face-set 'default))
                   ((eq w hydra-window)
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

;; highlight minibuffer prompt, because large monitor

(set-face-attribute 'minibuffer-prompt nil :background "light green" :foreground "black")


;; the foloowing is an attempt to highlight the whole minibuffer, but it failed

;; TODO: this kind of works, but fix this: the face gets reset back to default after auto-save-visited-mode kicks in after 5 seconds
;; (defun my-minibuffer-setup-hook ()
;;   (buffer-face-set :background "yellow"))
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;; this doesn't work for me for some reason
;; (dolist (buf (buffer-list))
;;   (when (string-match-p " \*Minibuf-[0-9]+\*" (buffer-name buf))
;;     (message (buffer-name buf))
;;     (with-current-buffer (get-buffer buf)
;;       (set (make-local-variable 'face-remapping-alist)
;;          (copy-tree'((default :background "green")))))))
;; (dolist (buf (buffer-list))
;;   (when (string-match-p " \*Minibuf-[0-9]+\*" (buffer-name buf))
;;     (message (buffer-name buf))
;;     (with-current-buffer (get-buffer buf)
;;       (buffer-face-set :background "yellow")
;;       )))

;; -------------------------------------------------------------------

;; Douglas Crockford once suggested syntax highlighting based on scope, this is the closest thing so far
(use-package prism       ; there's also https://github.com/Fanael/rainbow-delimiters, but it doesn't work for python
  :config
  (setq prism-comments nil)
  (setq prism-parens t)
  (prism-set-colors :num 16
    :desaturations (cl-loop for i from 0 below 16 collect 20)
    :lightens (cl-loop for i from 0 below 16 collect 0)
    ;; :desaturations '(0) :lightens '(0)  ; don't lower, keep the contrast high
    :colors (list
             ;; "black"
             "medium blue"
             "dark violet"
             "firebrick"
             "forest green"
             ;; "red"
             ;; "forest green"
             ;; "blue"
             )

    ;; try color-saturate-name, color-saturate-hsl
    ;; :parens-fn (lambda (color)
    ;;              (prism-blend color "white" 1))
    
    :parens-fn (lambda (color)
                 (apply #'color-rgb-to-hex (apply #'color-hsl-to-rgb (apply #'color-saturate-hsl (append (apply #'color-rgb-to-hsl (color-name-to-rgb color)) '(3000))))))

    
    ;; (apply #'color-saturate-hsl (append (apply #'color-rgb-to-hsl (color-name-to-rgb "white")) '(30)))
    ;; (apply #'color-saturate-hsl (append (apply #'color-rgb-to-hsl (color-name-to-rgb "#102030")) '(30)))
    
    
    ;; :strings-fn (lambda (color)
    ;;               (prism-blend color "white" 0.5))
    )

   ;; there's a function/macro used to return the face for a point at a given depth. Change that function and the face will change. Then you'd probably need to refontify the whole visible portion of the buffer whenever the point moves to a position at a different logical depth.
  )

;; (use-package highlight-function-calls
;;   :config
;;   (setq highlight-function-calls-macro-calls nil)
;;   (setq highlight-function-calls-special-forms nil)
;;   ;; (add-hook 'emacs-lisp-mode-hook 'highlight-function-calls-mode)
;;   )



;; -------------------------------------------------------------------

(setq global-hl-line-sticky-flag nil)   ; only appear in one window
(global-hl-line-mode)
(blink-cursor-mode 0)
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows t)   ;;  displays a cursor related to the usual cursor type
;; cursor color is set by base16, it used to be (set-cursor-color "#000")

(setq ym-hl-line-color-normal-mode-color "#e6eef7")   ; was LightSteelBlue1, e3ecf7
(set-face-background 'hl-line ym-hl-line-color-normal-mode-color)

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





