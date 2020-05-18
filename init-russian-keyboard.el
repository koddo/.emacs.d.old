;; taken from https://stackoverflow.com/questions/10639429/emacs-linux-and-international-keyboard-layouts/54647483#54647483
;; I added (super) and (super meta) to this list in the code
;; (modifiers '(nil (control) (meta) (control meta)))


;; I honesly have no idea how it works, but it does the job

;; it allows to use usual keybindings without switching to english layout
;; translates
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
        (modifiers '(nil (control) (meta) (control meta) (super) (super meta))))
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

