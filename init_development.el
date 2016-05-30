
;; -------------------------------------------------------------------
(ym-add-to-list-dired-omit-extensions '(".o" ".a"))
;; -------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . t)
   (lisp . t)
   (scheme . t)
   (python . t)
   (gnuplot . t)
   (sh . t)
   (sql . t)
   ))
;; -------------------------------------------------------------------
;; automatically indent pasted text if in programming-modes
(defvar yank-indent-modes '(emacs-lisp-mode
                            c-mode c++-mode
                            tcl-mode
                            sql-mode
                            perl-mode cperl-mode
                            java-mode jde-mode
                            lisp-interaction-mode
                            LaTeX-mode TeX-mode
                            erlang-mode
                            python-mode
                            ruby-mode
                            haskell-mode
                            )
 "Modes in which to indent regions that are yanked (or yank-popped)")
(defvar yank-advised-indent-threshold 1000000
  "Threshold (# chars) over which indentation does not automatically occur.")
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))
(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))
(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))
;; -------------------------------------------------------------------
;; for writing elisp macros
(require 'macrostep)
(defun m/elisp-macrostep-expand ()
  (interactive)
  (backward-sexp)
  (macrostep-expand))
;; -------------------------------------------------------------------
;; show matching paren
(require 'paren)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)   ; highlight parens if both visible, highlight the whole expression when one paren is invisible
;; (modify-syntax-entry ?\{ "(}")
;; (modify-syntax-entry ?\} "){")
(show-paren-mode t)
(progn (require 'autopair)   ; insert paired parenthesis
       (autopair-global-mode)
       (setq autopair-blink nil)
       (setq autopair-skip-whitespace 'chomp))   ; ) ) => )) when closing
;; -------------------------------------------------------------------
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/yasnippet"))   ; only use my snippets, skip default snippets dir
(setq yas-new-snippet-default "# -*- mode: snippet -*-\n# name: $1\n# key: ${2:${1:$(yas--key-from-desc yas-text)}}\n# --\n$0")
(yas/global-mode 1)
(add-hook 'snippet-mode-hook
          (lambda ()
            (setq require-final-newline nil)
            (add-hook 'after-save-hook 'yas-recompile-all 'append 'make-it-local)
            (add-hook 'after-save-hook 'yas-reload-all 'append 'make-it-local)))
(auto-insert-mode 1)
(setq auto-insert-query nil)
(defun ym-auto-insert-action () (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid major-mode "ymyasnippetautoinsertmodetemplate")) (point-min) (point-max)))
(define-auto-insert 'sh-mode 'ym-auto-insert-action)   ; when adding new modes, put file 'ymyasnippetautoinsertmodetemplate' to yasnippet/mode/ dir
;; (setq yas/indent-line 'fixed)
;; (setq yas/wrap-around-region nil)
;; clojure (ns ...) auto fill: http://inclojurewetrust.blogspot.ru/2011/04/fed-up-of-typing-ns-declarations.html
;; https://github.com/swannodette/clojure-snippets
;;; ??? hippie-expand
;; -------------------------------------------------------------------
;; (require 'rvm)
;; (rvm-use-default)
;; -------------------------------------------------------------------
;; (load "~/.elisp/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)   ; hslint on the command line only likes this indentation mode
;; (add-to-list 'completion-ignored-extensions ".hi")
;; -------------------------------------------------------------------
;; for coursera programming languages
;; (autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;; (autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
;; (add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))
;; (add-hook 'sml-mode-hook
;;           (lambda ()
;;             (define-key sml-mode-map (kbd "C-c C-v") 'm/sml-restart-repl-and-load-current-file)))
;; (defun m/sml-restart-repl-and-load-current-file ()
;;   (interactive)
;;   (ignore-errors (with-current-buffer "*sml*"
;;                    (comint-interrupt-subjob)
;;                    (comint-send-eof)
;;                    (let ((some-time 0.1))
;;                      (while (process-status (get-process "sml"))
;;                        (sleep-for some-time)))))      
;;   (flet ((sml--read-run-cmd ()
;;                             '("sml" "" nil)))   ; (command args host)  
;;     (sml-prog-proc-send-buffer t)))
;; -------------------------------------------------------------------
;; see options here https://github.com/mattkeller/mk-project
;; (require 'mk-project)
;; (defmacro m/def-project (vars)
;;   (let ((varslst (mapcar (lambda (lst) (car lst)) vars)))
;;     `(let* ,vars
;;        (project-def proj-name
;;                     (mapcar (lambda (elt) (list elt (symbol-value elt)))
;;                             ',varslst))
;;        )))
;; -------------------------------------------------------------------
;; ## make sure there is exuberant etags installed
;; $ sudo ports install ctags
;; $ cd /opt/local/bin/
;; $ sudo ln -s ctags etags
;; (require 'etags-select)
;; (require 'etags-update)
;; (etags-update-mode 1)
;; (setq tags-revert-without-query t)
;; (diminish 'etags-update-mode)
;; (setq etu/append-file-action
;;       (lambda (filename-to-save)
;;         (when
;;             (and (projectile-project-p) (file-exists-p (projectile-expand-root ".TAGS")))
;;             ;; (and ym-project-name
;;             ;;      ym-proj-tags-file
;;             ;;      (string-prefix-p ym-project-basedir filename-to-save)
;;             ;;      (some (lambda (elt) (not (null elt)))    ; has known extenstion
;;             ;;            (mapcar
;;             ;;             (lambda (w) (string-match (wildcard-to-regexp w) filename-to-save))
;;             ;;             ym-proj-src-patterns))
;;             ;;      )
;;           'add)))

;; ;; (if (and (boundp 'projectile-mode) projectile-mode)
;; ;;     (message "projectile-mode is on")
;; ;;   (message "projectile-mode is off"))

;; (defun ym-ido-find-tag ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (tags-completion-table)
;;   (let (tag-names)
;;     (mapatoms (lambda (x)
;;                 (push (prin1-to-string x t) tag-names))
;;               tags-completion-table)
;;     (etags-select-find (ido-completing-read "Tag: " tag-names))))
;; (defun ym-find-tag-at-point ()
;;   (interactive)
;;   (if (find-tag-default)
;;       (etags-select-find-tag-at-point)
;;     (ym-ido-find-tag)))
;; -------------------------------------------------------------------
(setq erlang-root-dir "~/.kerl_erlang/17.5")   ; maybe smth like this? (setq erlang-root-dir (shell-command-to-string))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(add-to-list 'load-path
             (car (file-expand-wildcards (concat erlang-root-dir "/lib/tools-*/emacs"))))
(require 'erlang-start)
(add-to-list 'load-path "~/.emacs.d/ym-packages/distel/elisp")
(require 'distel)
(distel-setup)
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-name" "emacs@127.0.0.1"))   ; default node name
            (setq erlang-compile-extra-opts '(debug_info))
            )) ; (setq erlang-mode-hook nil)
;; (require 'erlang-flymake)
;; (erlang-flymake-only-on-save)
;; ;; maybe http://blog.erlware.org/2012/05/15/getting-flymake-and-rebar-to-play-nice/
(defun m/erl-recompile-and-reload ()
  (interactive)
  (let
      ((current-prefix-arg '(4)))
    (call-interactively 'erlang-compile))
  ;; (erl-reload-module (erl-target-node) (erlang-get-module))
  ;; (message "===== %S" (list (erl-target-node) (erlang-get-module)))
  )
(defun m/erl-shell-on-node ()
  (interactive)
  (erl-choose-nodename)
  (erl-ping (erl-target-node))
  (sleep-for 2)
  (progn
    (erlang-shell-display)
    (end-of-buffer)
    (insert (kbd "C-g"))
    (erlang-RET-command)
    (insert (kbd "h"))
    (erlang-RET-command)
    (insert (kbd "j"))
    (erlang-RET-command)
    (let* ((node (symbol-name (erl-target-node)))
           (found
            (save-excursion
              (search-backward-regexp "^ --> j\n")
              (numberp (ignore-errors (search-forward node))))
            ))
      (flet ((get-job-number ()
                             (save-excursion
                               (search-backward node)
                               (let ((line (substring-no-properties (buffer-substring (point-at-bol) (point-at-eol)))))
                                 (string-match "^[[:space:]]*\\([[:digit:]]+\\)+" line)
                                 (match-string-no-properties 1 line))))
             (connect-to-job ()
                             (let ((nn (get-job-number)))
                               (insert (concat "c " nn))
                               (erlang-RET-command)
                               (erlang-RET-command)
                               )))
        (if (not found)
            (progn (insert (concat "r '" node "'"))
                   (erlang-RET-command)
                   (insert "j")
                   (erlang-RET-command)
                   (connect-to-job))
          (connect-to-job)
          )))))
(defun m/erl-connect-to-node ()
  (interactive)
  (erl-choose-nodename)
  (erl-ping (erl-target-node)))
;; -------------------------------------------------------------------
;; (setq save-abbrevs nil)   ; stop asking whether to save newly added abbrev when quitting emacs
;; (setq-default abbrev-mode t)   ; turn on abbrev mode globally
;; (define-abbrev-table 'sql-mode-abbrev-table
;;   (mapcar #'(lambda (v) (list v (upcase v) nil 1))
;;           '("absolute" "action" "add" "after" "all" "allocate" "alter" "and" "any" "are" "array" "as" "asc" "asensitive" "assertion" "asymmetric" "at" "atomic" "authorization" "avg" "before" "begin" "between" "bigint" "binary" "bit" "bitlength" "blob" "boolean" "both" "breadth" "by" "call" "called" "cascade" "cascaded" "case" "cast" "catalog" "char" "char_length" "character" "character_length" "check" "clob" "close" "coalesce" "collate" "collation" "column" "commit" "condition" "connect" "connection" "constraint" "constraints" "constructor" "contains" "continue" "convert" "corresponding" "count" "create" "cross" "cube" "current" "current_date" "current_default_transform_group" "current_path" "current_role" "current_time" "current_timestamp" "current_transform_group_for_type" "current_user" "cursor" "cycle" "data" "date" "day" "deallocate" "dec" "decimal" "declare" "default" "deferrable" "deferred" "delete" "depth" "deref" "desc" "describe" "descriptor" "deterministic" "diagnostics" "disconnect" "distinct" "do" "domain" "double" "drop" "dynamic" "each" "element" "else" "elseif" "end" "equals" "escape" "except" "exception" "exec" "execute" "exists" "exit" "external" "extract" "false" "fetch" "filter" "first" "float" "for" "foreign" "found" "free" "from" "full" "function" "general" "get" "global" "go" "goto" "grant" "group" "grouping" "handler" "having" "hold" "hour" "identity" "if" "immediate" "in" "indicator" "initially" "inner" "inout" "input" "insensitive" "insert" "int" "integer" "intersect" "interval" "into" "is" "isolation" "iterate" "join" "key" "language" "large" "last" "lateral" "leading" "leave" "left" "level" "like" "local" "localtime" "localtimestamp" "locator" "loop" "lower" "map" "match" "map" "member" "merge" "method" "min" "minute" "modifies" "module" "month" "multiset" "names" "national" "natural" "nchar" "nclob" "new" "next" "no" "none" "not" "null" "nullif" "numeric" "object" "octet_length" "of" "old" "on" "only" "open" "option" "or" "order" "ordinality" "out" "outer" "output" "over" "overlaps" "pad" "parameter" "partial" "partition" "path" "position" "precision" "prepare" "preserve" "primary" "prior" "privileges" "procedure" "public" "range" "read" "reads" "real" "recursive" "ref" "references" "referencing" "relative" "release" "repeat" "resignal" "restrict" "result" "return" "returns" "revoke" "right" "role" "rollback" "rollup" "routine" "row" "rows" "savepoint" "schema" "scope" "scroll" "search" "second" "section" "select" "sensitive" "session" "session_user" "set" "sets" "signal" "similar" "size" "smallint" "some" "space" "specific" "specifictype" "sql" "sqlcode" "sqlerror" "sqlexception" "sqlstate" "sqlwarning" "start" "state" "static" "submultiset" "substring" "sum" "symmetric" "system" "system_user" "table" "tablesample" "temporary" "then" "time" "timestamp" "timezone_hour" "timezone_minute" "to" "trailing" "transaction" "translate" "translation" "treat" "trigger" "trim" "true" "under" "undo" "union" "unique" "unknown" "unnest" "until" "update" "upper" "usage" "user" "using" "value" "values" "varchar" "varying" "view" "when" "whenever" "where" "while" "window" "with" "within" "without" "work" "write" "year" "zone")
;;           ))
(load-library "sql-indent")
(setq sql-indent-first-column-regexp    ; added "alter"
      (concat "\\(^\\s-*" (regexp-opt '(
                                        "select" "update" "insert" "delete"
                                        "union" "intersect"
                                        "from" "where" "into" "group" "having" "order"
                                        "set"
                                        "create" "drop" "truncate"
                                        ;; <------------------------ my
                                        "alter"
                                        "reset"
                                        "begin" "end" "commit" "return" "declare"
                                        "if"
                                        "$$"
                                        "grant"
                                        "--") t) "\\(\\b\\|\\s-\\)\\)\\|\\(^```$\\)"))
(defun m/align-sql (start end)
 (interactive "*r")
 (align-regexp start end "\\s-*[^[:space:]]*\\(\\s-*\\)" 1 3 nil)
 )
(add-to-list 'auto-mode-alist '("\\.pgtap$" . sql-mode))
;; -------------------------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
;; -------------------------------------------------------------------
(require 'projectile)
(require 'ack-and-a-half)
(require 'dash)
(require 's)
(require 'epl)
(projectile-global-mode)
                                        ; TODO: what about flx-ido -- highly recommended by projectile?
;; (add-hook 'js-mode-hook 'projectile-on)
;; (add-hook 'css-mode-hook 'projectile-on)
;; (add-hook 'web-mode-hook 'projectile-on)
;; (add-hook 'emacs-lisp-mode-hook 'projectile-on)
;; (add-hook 'python-mode-hook 'projectile-on)
;; (add-hook 'prog-mode-hook 'projectile-on) --- ???
;; -------------------------------------------------------------------
(require 'flycheck)
;; -------------------------------------------------------------------
(require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)     ; TODO: keybinding
;; -------------------------------------------------------------------
(require 'quickrun)
;; (quickrun-add-command "shellscript"
;;                       '((:command . (lambda () sh-shell))
;;                         (:description . "Run Shellscript file")))
;; -------------------------------------------------------------------
(require 'restclient)
;; -------------------------------------------------------------------
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode))
(require 'queue)
(require 'cider)
;; -------------------------------------------------------------------
(require 'yaml-mode)
;; -------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
;; -------------------------------------------------------------------
(defun ym-jekyll-post-template ()
  (interactive)
  (yas-expand-snippet (yas--template-content (yas--get-template-by-uuid major-mode "ymjekyllposttemplate"))))
(defun ym-rename-current-buffer-file (&optional suggestion)   ; http://emacs.stackexchange.com/questions/2849/save-current-file-with-a-slightly-different-name/2850#2850
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " nil nil 'confirm
                                      (if suggestion
                                          suggestion
                                        filename)   ; a bug here -- when using ido-everywhere and falling back to a regular read-file-name with C-f, it inserts a name twice, but this doesn't bother me, because I never use this fallback
                                      )))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'."
                   name (file-name-nondirectory new-name)))))))
(defun ym-jekyll-regenerate-date-and-filename ()
  (interactive)
  (beginning-of-buffer)
  (search-forward-regexp "^--- *$" nil t 2)
  (let ((end-of-front-matter (point)))
    (beginning-of-buffer)
    (search-forward-regexp "^date:.*$" end-of-front-matter t)
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert "date:   " (format-time-string "%Y-%m-%d %H:%M:%S %z"))
    (beginning-of-buffer)
    (search-forward-regexp "^title:.*$" end-of-front-matter t)
    (let ((title-line (buffer-substring (line-beginning-position)
                                  (line-end-position))))
      (save-match-data
        (string-match "title:  \"\\(.*\\)\"" title-line)
        (let ((title
               (s-replace-all '((" " . "-") ("'" . "") ("\"" . "") ("!" . "") ("?" . "") ("." . "") ("," . ""))
                              (downcase (match-string 1 title-line)))))
          (ym-rename-current-buffer-file (concat (format-time-string "%Y-%m-%d") "-" title ".markdown"))
          )
        )
      )
    )
  )








