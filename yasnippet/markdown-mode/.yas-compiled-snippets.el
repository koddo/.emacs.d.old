;;; Compiled snippets and support files for `markdown-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
                     '(("ymjekyllposttemplate" "---\nlayout: post\ntitle:  \"$0\"\ndate:   `(format-time-string \"%Y-%m-%d %H:%M:%S %z\")`\ntags:   []\ncomments: true\ndisqus_identifier: `(shell-command \"uuidgen\" t)`\n---\n\n" "ymjekyllposttemplate" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Tue May 24 19:03:29 2016
