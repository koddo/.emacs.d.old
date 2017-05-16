;;; Compiled snippets and support files for `markdown-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
                     '(("ymjekyllposttemplate" "---\nlayout: post\ntitle:  \"$0\"\ndate:   `(format-time-string \"%Y-%m-%d %H:%M:%S %z\")`\ntags:   []\ncomments: true\ndisqus_identifier: `(first (process-lines \"uuidgen\"))`\n\n---\n\n" "ymjekyllposttemplate" nil nil nil "/Users/alex/.emacs.d/yasnippet/markdown-mode/ymjekyllposttemplate" nil nil)))


;;; Do not edit! File generated at Sun May 14 16:52:11 2017
