(server-start)
;(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t)

;; -------------------------------------------------------------------
(setq user-full-name "Alex Scherbanov")
(setq user-mail-address "alex@egotv.ru")
;; -------------------------------------------------------------------

(require 'view)   ; for view-mode-map
(tool-bar-mode -1)   ; menu-bar-mode moved to preinit
(setq inhibit-startup-message t)
(setq frame-title-format "emacs")
(setq debug-on-error t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq-default truncate-lines t)
(setq x-select-enable-clipboard t)
(setq require-final-newline t)
(setq calendar-week-start-day 1)     ;; week starts from Monday
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)   ;; chmod u+x current file when saving if not set
(setq next-line-add-newlines t)
(setq vc-handled-backends nil)          ;; disable version control (vc) enabled by default, it slows down emacs, and i don't use it
(setq split-width-threshold nil) ; for calendar. otherwise emacs splits windows horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 160)
(put 'downcase-region 'disabled nil)
(put   'upcase-region 'disabled nil)
(  line-number-mode t)
(column-number-mode t)
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
;; -------------------------------------------------------------------
(require   'uniquify)  ; buffer names are uniquified with parts of directory name, for ex.: name|folder
(setq       uniquify-buffer-name-style 'reverse)
;; -------------------------------------------------------------------
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-visited-file-name t)    ; just autosave files, not their backups
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)
(setq auto-save-include-big-deletions t)   ; do not track auto-save backups
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
(add-hook 'dired-mode-hook 'auto-revert-mode)    ;; don't know if it works
(require 'dired+)
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(defvar ym-dired-omit-files-system-specific "")  ; see preinit_win.el -- to hide RECYCLER folder
(setq dired-omit-files (concat "^\\.[^.].*$\\|^\\.$\\|^#.*$" ym-dired-omit-files-system-specific))        ;; omit ".files" and ".", but show ".."
(setq dired-omit-extensions '())
(defun ym-add-to-list-dired-omit-extensions (extensions-list)
  (mapc (lambda (ext) (add-to-list 'dired-omit-extensions ext))
        extensions-list))
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")
(setq dired-details-hide-link-targets nil)
(require 'ls-lisp) ;; ignore case when listing directory
(setq ls-lisp-ignore-case t)
(setq ls-lisp-use-insert-directory-program nil)
(setq delete-by-moving-to-trash t)
(set-face-attribute 'diredp-symlink nil :foreground "Blue")   ; trash-directory is set in preinit.this_machine.el
;; -------------------------------------------------------------------
(require 'epa-file)   ; gnupg, gpg, pgp
(setq epa-file-inhibit-auto-save nil)
;; -------------------------------------------------------------------
(require 'ido)   ; InteractivelyDoThings -- input completion without having to hit TAB
(setq ido-ignore-buffers '("\\` " "\\.muse\\'" "\\*Ibuffer\\*" "\\*Ido Completions\\*"))
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)   ;; may be nil, guess and t(literal filename)
(setq ido-use-url-at-point t)
(setq ido-enter-matching-directory nil)
(setq ido-show-dot-for-dired t)
(setq ido-use-virtual-buffers t)
(setq ido-enable-tramp-completion nil)
(ido-everywhere 1)
(setq ido-case-fold t)
(setq ido-max-prospects 7)
(defun ido-sort-mtime ()   ; sort ido filelist by mtime instead of alphabetically
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(add-to-list 'ido-ignore-buffers "\\*Messages\\*")
(add-to-list 'ido-ignore-buffers "\\*Completions\\*")
(add-to-list 'ido-ignore-buffers "\\*Scratch\\*")
(add-to-list 'ido-ignore-buffers "\\*Help\\*")
(ido-mode t)
;; -------------------------------------------------------------------
(require 'smex)   ; M-x with ido
(smex-initialize)
(defun ym-smex-update-after-load-file (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'ym-smex-update-after-load-file)   ; see init_keybindings.el
;; -------------------------------------------------------------------
(require 'diminish)   ; hide some minor modes from mode line
(eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
;; -------------------------------------------------------------------
(recentf-mode 1)
(setq recentf-max-saved-items 200)
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
;; -------------------------------------------------------------------
(require 'tree-mode)
(require 'dirtree)   ; alternative to dired
(require 'windata)
;; -------------------------------------------------------------------
