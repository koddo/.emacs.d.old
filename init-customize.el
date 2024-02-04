(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-omit-extensions 'nil nil nil "Customized by me")
 '(dired-omit-files (rx (or (seq bol "." eol) (seq bol "." (not (any "."))))) nil nil "Customized by me")
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(header-line ((t (:inherit nil :background "grey70" :foreground "grey20"))))
 '(org-habit-alert-face ((t (:background "white"))))
 '(org-habit-alert-future-face ((t (:background "white"))))
 '(org-habit-clear-face ((t (:background "white"))))
 '(org-habit-clear-future-face ((t (:background "white"))))
 '(org-habit-overdue-face ((t (:background "white"))))
 '(org-habit-overdue-future-face ((t (:background "white"))))
 '(org-habit-ready-face ((t (:background "white"))))
 '(org-habit-ready-future-face ((t (:background "white"))))
 '(org-hide ((t (:foreground "light gray"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "firebrick3"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green3"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "peru"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "grey50"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "black"))))
 '(sp-show-pair-enclosing ((t (:inherit show-paren-match))))
 '(sp-show-pair-match-content-face ((t nil)) t))


