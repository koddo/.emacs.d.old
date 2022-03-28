;; (rainbow-mode)

;; taken from stackoverflow
(defun ym/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))



(setq global-hl-line-sticky-flag nil)   ; only appear in one window
(global-hl-line-mode)
(blink-cursor-mode 0)
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows t)   ;;  displays a cursor related to the usual cursor type
;; cursor color is set by base16, it used to be (set-cursor-color "#000")

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

;; http://chriskempson.com/projects/base16/
;; https://github.com/belak/base16-emacs
;; https://github.com/belak/base16-emacs/blob/master/base16-theme.el
(use-package base16-theme
  :after org
  :config
  (deftheme ym-base16-theme)

  (progn
    (setq ym-base16-colors (list
			    :base00 "white"      ; Default Background
			    :base01 "grey90"     ; Lighter Background (Used for status bars)
			    :base02 "#d8d8d8"    ; Selection Background
			    :base03 "grey60"     ; Comments, Invisibles, Line Highlighting
			    :base04 "#585858"    ; Dark Foreground (Used for status bars)
			    :base05 "grey25"     ; Default Foreground, Caret, Delimiters, Operators
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
    (base16-theme-define 'ym-base16-theme ym-base16-colors)
    (enable-theme 'ym-base16-theme)
    )

  ;; temporarily fixing this: https://github.com/belak/base16-emacs/issues/114
  ;; font-lock-comment-delimiter-face should be base03, not base02
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground (plist-get ym-base16-colors :base03))
  )



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

(setq ym-hl-line-color-normal-mode-color "#e6eef7")   ; was LightSteelBlue1, e3ecf7
(set-face-background 'hl-line ym-hl-line-color-normal-mode-color)




(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
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




;; (defface ym/habits-face '((t :family "Monaco" :height 80)) "")

