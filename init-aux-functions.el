(defun goto-random-line ()
  (interactive)
  (end-of-buffer)
  (let ((eof-line-number (line-number-at-pos)))
    (beginning-of-buffer)
    (forward-line (random eof-line-number))
    )
  )

(defun ym-org-agenda-drill-today-only ()
  (interactive)
  (org-agenda nil "dt")
  )



(defun ym/emacs-training ()
  (interactive)
  (let ((buf (get-buffer-create "ym/emacs-training-tmp-buf")))
    (set-buffer-major-mode buf)
    (with-current-buffer buf
      (emacs-lisp-mode)
      (insert-file-contents "~/werk/emacs-training.el")
      )
    (switch-to-buffer buf)
    (delete-other-windows)
    ))
