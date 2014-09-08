
;; -------------------------------------------------------------------
(defun m/dot-emacs-reload-init-file ()
  (interactive)
  (ym-load-path)
  (load-file "~/.emacs.d/init.el"))
;; -------------------------------------------------------------------
(setq ym-backup-log-file "~/.ym-backup.log")
(defun m/backup-check-log ()
  (interactive)
  (delete-other-windows)
  
  (let ((buf (find-buffer-visiting ym-backup-log-file)))
    (if buf (switch-to-buffer buf)
      (find-file ym-backup-log-file)))
  (revert-buffer :noconfirm t)
  (view-mode t)
  (goto-char (point-max)))
;; -------------------------------------------------------------------
(defun ym-aux-make-unique-image-name (&optional relative-path tag ext)
  (make-temp-file
   (concat
    (file-name-directory (buffer-file-name))
    relative-path
    (format-time-string "%Y%m%d-%H%M-" nil t)
    )
   nil
   (concat (when tag ".") tag "." ext)))
(defun m/org-img-screenshot (&optional relative-path dont-insert-at-point-and-display)
  "Take a screenshot into a unique-named file. When relative-path is nil, .images/ is used."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Can insert a screenshot only in org-mode buffers.")
    (let* ((rel-path (if (null relative-path) ".images/" relative-path))
           (file-name (ym-aux-make-unique-image-name rel-path "screenshot" "png"))
           (relative-file-name (file-relative-name file-name (file-name-directory (buffer-file-name))))
           )
      (call-process "screencapture" nil nil nil "-i" file-name)
      (if (zerop (nth 7 (file-attributes file-name)))
          (delete-file file-name)
        (when (not dont-insert-at-point-and-display)
          (insert (concat "[[" relative-file-name "]]"))
          (org-display-inline-images)
          )
        relative-file-name
        ))))
(defun m/org-img-insert()
  "Prompt for image filename, move it to images folder and insert [[newfilename]] to buffer"
  (interactive)
  (let* ((old-file-name (read-file-name "insert image to buffer: " nil nil t))
                                        ; (file-name (file-name-nondirectory old-file-name))
                                        ; (new-file-name (expand-file-name file-name "~/tmp"))
         (new-file-name (ym-aux-make-unique-image-name ".images/" "image" (file-name-extension old-file-name)))
         (relative-file-name (file-relative-name new-file-name (file-name-directory (buffer-file-name))))
         )
    (rename-file old-file-name new-file-name t)
    (insert (concat "[[" relative-file-name "]]"))
    (org-display-inline-images)
    ))
(defun m/org-img-remove-file-at-point ()
  (interactive)
  (let ((thing (ignore-errors (substring-no-properties (thing-at-point 'filename))))
        (bounds-of-thing (bounds-of-thing-at-point 'filename)))
    (if (not (stringp thing))
        (message "No file at point.")
      (let ((filename (concat
                       (file-name-directory (buffer-file-name))
                       thing)))
        (if (not (file-exists-p filename))
            (message (concat "This file does not exist: " filename))
          (when (yes-or-no-p (concat "Do you really want to remove " filename "? "))
            (delete-file filename t)
            (delete-region (car bounds-of-thing) (cdr bounds-of-thing))))))))
(defun m/org-edit-file-at-point ()
  (interactive)
  (let ((thing (ignore-errors (substring-no-properties (thing-at-point 'filename)))))
    (open-file-with-os thing)
    ))
;; -------------------------------------------------------------------
;; $asdf$ <-> [$]asdf[/$]
(defun m/ankify-region (start end)
  (interactive "*r")
  (replace-regexp "\\($+\\)\\([^$]+\\)\\($+\\)" "[\\1]\\2[/\\3]" nil start end))
(defun m/deankify-region (start end)
  (interactive "*r")
  (replace-regexp "\\[/?\\(\\$\\$\\|\\$\\)\\]" "\\1" nil start end))
;; -------------------------------------------------------------------
(defun m/org-list-done ()
  (interactive)
  (org-tags-view nil (concat "/" (mapconcat (lambda (str) (car (split-string str "("))) ym-org-todo-keywords-done "|")))
  (delete-other-windows)
  )
;; -------------------------------------------------------------------
(defun m/toggle-habits ()
  (interactive)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (ym-org-day-view)
  )







;; (setq ym-proj-name nil)
;; (setq ym-proj-tags-file nil)
;; (setq ym-proj-basedir nil)
;; (setq ym-proj-src-patterns nil)

;; (defun m/proj-load ()
;;   (interactive)
;;   (visit)
;;   )
