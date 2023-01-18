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

