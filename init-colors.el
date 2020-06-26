

(setq global-hl-line-sticky-flag nil)   ; only appear in one window
(global-hl-line-mode)
(blink-cursor-mode 0)




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
			    :base03 "grey75"     ; Comments, Invisibles, Line Highlighting
			    :base04 "#585858"    ; Dark Foreground (Used for status bars)
			    :base05 "grey20"     ; Default Foreground, Caret, Delimiters, Operators
			    :base06 "#282828"    ; Light Foreground (Not often used)
			    :base07 "#181818"    ; Light Background (Not often used)
			    :base08 "#ab4642"    ; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
			    :base09 "#dc9656"    ; Integers, Boolean, Constants, XML Attributes, Markup Link Url
			    :base0A "#f7ca88"    ; Classes, Markup Bold, Search Text Background
			    :base0B "#a1b56c"    ; Strings, Inherited Class, Markup Code, Diff Inserted
			    :base0C "#86c1b9"    ; Support, Regular Expressions, Escape Characters, Markup Quotes
			    :base0D "#7cafc2"    ; Functions, Methods, Attribute IDs, Headings
			    :base0E "#ba8baf"    ; Keywords, Storage, Selector, Markup Italic, Diff Changed
			    :base0F "#a16946"    ; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
			    ))
    (base16-theme-define 'ym-base16-theme ym-base16-colors))

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

  )

