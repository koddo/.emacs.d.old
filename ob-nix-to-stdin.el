;;; ob-nix-to-stdin.el

;; from https://www.linux.org.ru/forum/desktop/13139938
(defun org-babel-execute:nix-to-stdin (body params)
  "Execute command with body passed to stdin"
  (org-babel-eval "cat" body))

(provide 'ob-nix-to-stdin)
