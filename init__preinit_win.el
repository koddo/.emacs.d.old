;;;; Microsoft Windows part of .emacs

(let* ((cygwin-root "c:/")
           (cygwin-bin (concat cygwin-root "/bin")))
      (setenv "HOME" "d:/")
      (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
      (setq exec-path (cons cygwin-bin exec-path)))
    (setq shell-file-name "bash")
    (setq explicit-shell-file-name "bash")

(require 'cygwin-mount)
(cygwin-mount-activate)

;; in dired skip two system folders: RECYCLER and System Volume Information
;; it's used in main .emacs when configuring dired+
(setq ym-dired-omit-files-system-specific "\\|^RECYCLER$\\|^System Volume Information$")

;; old stuff above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;; On Windows operating systems it can be unreliable to cache directory listings: the directory may not appear to be modified even though files have been added or removed. Ido caches directory listings by default, which may cause confusion on Windows. You can disable caching:
(when (equal system-type 'windows-nt)
  (setq ido-max-dir-file-cache 0)) ; caching unreliable
