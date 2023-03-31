
;; TODO yasnippet

;; TODO (auto-insert-mode 1)
;; (define-auto-insert 'sh-mode 'ym-auto-insert-action)  ;; probably a good idea to use yasnippet

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)   ;; chmod u+x current file when there's shebang

;; -------------------------------------------------------------------

;; TODO yank-indent

;; -------------------------------------------------------------------


;; TODO macrostep for writing elisp macros
;; (require 'macrostep)
;; (defun m/elisp-macrostep-expand ()
;;   (interactive)
;;   (backward-sexp)
;;   (macrostep-expand))



;; -------------------------------------------------------------------


;; flycheck

;; -------------------------------------------------------------------

;; yaml-mode

;; -------------------------------------------------------------------

;; TODO: indirect buffers
;; https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; https://emacs.stackexchange.com/questions/12180/why-use-indirect-buffers/12185#12185
;; (defun narrow-to-region-indirect (start end)
;;   "Restrict editing in this buffer to the current region, indirectly."
;;   (interactive "r")
;;   (deactivate-mark)
;;   (let ((buf (clone-indirect-buffer nil nil)))
;;     (with-current-buffer buf
;;       (narrow-to-region start end)
;;       (python-mode))
;;     (switch-to-buffer buf)
;;     (font-lock-fontify-buffer)   ; without this the colors get lost
;;     ))


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------


;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

;; -------------------------------------------------------------------

