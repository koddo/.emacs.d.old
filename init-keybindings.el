;; https://emacs.stackexchange.com/questions/14755/how-to-remove-bindings-to-the-esc-prefix-key/14759#14759
;; let the escape key do its thing
;; yeah, I feel the judgemental stare
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))   ; = quit

;; -------------------------------------------------------------------

;; I no longer use cua-mode
;; (cua-mode t)   ; CUA mode: C-x, C-c, C-v for copying, pasting, C-z for undo
;; (setq cua-keep-region-after-copy t)   ; Standard Windows behaviour

;; -------------------------------------------------------------------

