(setq-default truncate-lines t)
(setq x-select-enable-clipboard t)
(setq require-final-newline t)
(setq next-line-add-newlines t)
(setq vc-handled-backends nil)          ;; disable version control (vc) enabled by default, it slows down emacs, and i don't use it
(put 'downcase-region 'disabled nil)
(put   'upcase-region 'disabled nil)
(setq      comint-buffer-maximum-size 5000) ; truncate the shell buffer
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ; to a certain maximum number of lines
(when (fboundp 'winner-mode)
  (winner-mode 1))   ; see init_keybindings.el
(autoload 'ibuffer "ibuffer" "List buffers." t) ; see init_keybindings.el -- replacement for BufferMenu (C-x C-b)
(defadvice query-replace-read-args   ; Don’t bother entering search and replace args if the buffer is read-only
  (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))
(add-hook 'ledger-mode-hook (lambda ()
                              (set (make-local-variable 'comment-start) "; ")
                              (set (make-local-variable 'comment-style) 'plain)))
;; -------------------------------------------------------------------
(require       'saveplace)   ; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(setq-default   save-place t)
(setq save-place-file "~/.emacs.d/.emacs-places")
;; -------------------------------------------------------------------
(require   'uniquify)  ; buffer names are uniquified with parts of directory name, for ex.: name|folder
(setq       uniquify-buffer-name-style 'reverse)
;; -------------------------------------------------------------------
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
;; (setq auto-save-visited-file-name t) -- I found this glitchy, sometimes it saves to #auto-save-files#, but not to originals, probably because of auto-save-list-file-name set to nil
(defun save-buffer-if-visiting-file (&optional args)   ; https://shreevatsa.wordpress.com/2008/01/22/emacs-auto-save/, http://bryan-murdock.blogspot.ru/2008/03/beat-save-habit.html
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)   ; forces saving the buffer to file, still creates #files#, but they are gitignored
;; -------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark)) ; show tabs
(setq global-whitespace-mode 1)
;; -------------------------------------------------------------------
(set-scroll-bar-mode 'right)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1))
;; -------------------------------------------------------------------
;; show full filename path in mode line
(setq ym-mode-line-filename-max-directory-length 30)
(let ((index (position 'mode-line-buffer-identification mode-line-format)))
  (if index
      (let ((new-buffer-id
             '(:eval
               (let* ((in-dired-mode (eq 'dired-mode (with-current-buffer (current-buffer) major-mode)))
                      (dir (ym-shorten-directory
                            (cond
                             (in-dired-mode default-directory)
                             ((not buffer-file-name) "")
                             (t (file-name-directory buffer-file-name)))
                            ym-mode-line-filename-max-directory-length))
                      (bufname (buffer-name)))
                 (propertize (concat
                              (propertize dir 'font-lock-face ym-mode-line-directory-font)
                              (when (not in-dired-mode) (propertize bufname 'font-lock-face ym-mode-line-file-name-font)))
                             'help-echo (buffer-file-name))))))
        (setcar (nthcdr index mode-line-format)   ; replace mode-line-buffer-identification with '(:eval ...) above
                new-buffer-id))
    (message "Notice: somebody already had changed mode line")))
;; reset mode-line: (setcar (nthcdr 7 mode-line-format) 'mode-line-buffer-identification)
(defun ym-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path
                ;; (< (+ (length output)) (- max-length 4))   ; 4 chars in ".../"
                (< (+ 1 (length output) (length (car path))) (- max-length 4))
                )
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when (and path (equal "" (car path)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path))
      )
    (when path
      (setq output (concat ".../" output)))
    output))
