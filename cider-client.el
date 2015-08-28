;;; cider-client.el --- A layer of abstraction above the actual client code. -*- lexical-binding: t -*-

;; Copyright © 2013-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A layer of abstraction above the actual client code.

;;; Code:

(require 'nrepl-client)
(require 'cider-util)

;;; Connection Buffer Management

(defvar cider-connections nil
  "A list of connections.")

(defun cider-default-connection (&optional no-error)
  "The default (fallback) connection to use for nREPL interaction.
When NO-ERROR is non-nil, don't throw an error when no connection has been
found."
  (or nrepl-connection-buffer
      (car (cider-connections))
      (unless no-error
        (error "No nREPL connection buffer"))))

(define-obsolete-function-alias 'nrepl-current-connection-buffer 'cider-default-connection "0.10")

(defun cider-connections ()
  "Return the list of connection buffers."
  (setq cider-connections
        (-remove (lambda (buffer)
                   (not (buffer-live-p (get-buffer buffer))))
                 cider-connections)))

(defun cider-repl-buffers ()
  "Return the list of REPL buffers.
Purge the dead buffers from the `cider-connections' beforehand."
  (-filter
   (lambda (buffer)
     (with-current-buffer buffer (derived-mode-p 'cider-repl-mode)))
   (buffer-list)))

(defun cider-make-connection-default (connection-buffer)
  "Make the nREPL CONNECTION-BUFFER the default connection.
Moves CONNECTION-BUFFER to the front of `cider-connections'."
  (interactive (list nrepl-connection-buffer))
  (if connection-buffer
      ;; maintain the connection list in most recently used order
      (let ((buf-name (buffer-name (get-buffer connection-buffer))))
        (setq cider-connections
              (cons buf-name (delq buf-name cider-connections)))
        (cider--connections-refresh))
    (user-error "Not in a REPL buffer")))

(declare-function cider--close-buffer "cider-interaction")
(defun cider--close-connection-buffer (conn-buffer)
  "Close CONN-BUFFER, removing it from `cider-connections'.
Also close associated REPL and server buffers."
  (let ((buffer (get-buffer conn-buffer)))
    (setq cider-connections
          (delq (buffer-name buffer) cider-connections))
    (when (buffer-live-p buffer)
      (dolist (buf `(,@(or (nrepl--get-sibling-buffers buffer)
                           (list buffer))
                     ,(buffer-local-value 'nrepl-tunnel-buffer buffer)
                     ,(buffer-local-value 'nrepl-server-buffer buffer)))
        (when buf
          (cider--close-buffer buf))))))


;;; Connection Browser
(defvar cider-connections-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'cider-connections-make-default)
    (define-key map "g" #'cider-connection-browser)
    (define-key map (kbd "C-k") #'cider-connections-close-connection)
    (define-key map (kbd "RET") #'cider-connections-goto-connection)
    map))

(declare-function cider-popup-buffer-mode "cider-interaction")
(define-derived-mode cider-connections-buffer-mode cider-popup-buffer-mode
                     "CIDER Connections"
  "CIDER Connections Buffer Mode.
\\{cider-connections-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

(defvar cider--connection-ewoc)
(defconst cider--connection-browser-buffer-name "*cider-connections*")

(defun cider-connection-browser ()
  "Open a browser buffer for nREPL connections."
  (interactive)
  (let ((buffer (get-buffer cider--connection-browser-buffer-name)))
    (if buffer
        (progn
          (cider--connections-refresh-buffer buffer)
          (unless (get-buffer-window buffer)
            (select-window (display-buffer buffer))))
      (cider--setup-connection-browser))))

(define-obsolete-function-alias 'nrepl-connection-browser 'cider-connection-browser "0.10")

(defun cider--connections-refresh ()
  "Refresh the connections buffer, if the buffer exists.
The connections buffer is determined by
`cider--connection-browser-buffer-name'"
  (let ((buffer (get-buffer cider--connection-browser-buffer-name)))
    (when buffer
      (cider--connections-refresh-buffer buffer))))

(defun cider--connections-refresh-buffer (buffer)
  "Refresh the connections BUFFER."
  (cider--update-connections-display
   (buffer-local-value 'cider--connection-ewoc buffer)
   cider-connections))

(defun cider--setup-connection-browser ()
  "Create a browser buffer for nREPL connections."
  (with-current-buffer (get-buffer-create cider--connection-browser-buffer-name)
    (let ((ewoc (ewoc-create
                 'cider--connection-pp
                 "  Host              Port   Project\n")))
      (setq-local cider--connection-ewoc ewoc)
      (cider--update-connections-display ewoc cider-connections)
      (setq buffer-read-only t)
      (cider-connections-buffer-mode)
      (display-buffer (current-buffer)))))

(defvar-local cider-repl-type nil
  "The type of this REPL buffer, usually either \"clj\" or \"cljs\".")

(defun cider--connection-pp (connection)
  "Print an nREPL CONNECTION to the current buffer."
  (let* ((buffer-read-only nil)
         (buffer (get-buffer connection))
         (endpoint (buffer-local-value 'nrepl-endpoint buffer)))
    (insert
     (format "%s %-16s %5s   %s%s"
             (if (equal connection (car cider-connections)) "*" " ")
             (car endpoint)
             (prin1-to-string (cadr endpoint))
             (or (cider--project-name
                  (buffer-local-value 'nrepl-project-dir buffer))
                 "")
             (with-current-buffer buffer
               (if nrepl-sibling-buffer-alist
                   (concat " " cider-repl-type)
                 ""))))))

(defun cider--update-connections-display (ewoc connections)
  "Update the connections EWOC to show CONNECTIONS."
  (ewoc-filter ewoc (lambda (n) (member n connections)))
  (let ((existing))
    (ewoc-map (lambda (n) (setq existing (cons n existing))) ewoc)
    (let ((added (-difference connections existing)))
      (mapc (apply-partially 'ewoc-enter-last ewoc) added)
      (save-excursion (ewoc-refresh ewoc)))))

(defun cider--ewoc-apply-at-point (f)
  "Apply function F to the ewoc node at point.
F is a function of two arguments, the ewoc and the data at point."
  (let* ((ewoc cider--connection-ewoc)
         (node (and ewoc (ewoc-locate ewoc))))
    (when node
      (funcall f ewoc (ewoc-data node)))))

(defun cider-connections-make-default ()
  "Make default the connection at point in the connection browser."
  (interactive)
  (save-excursion
    (cider--ewoc-apply-at-point #'cider--connections-make-default)))

(defun cider--connections-make-default (ewoc data)
  "Make the connection in EWOC specified by DATA default.
Refreshes EWOC."
  (interactive)
  (cider-make-connection-default data)
  (ewoc-refresh ewoc))

(defun cider-connections-close-connection ()
  "Close connection at point in the connection browser."
  (interactive)
  (cider--ewoc-apply-at-point #'cider--connections-close-connection))

(defun cider--connections-close-connection (ewoc data)
  "Close the connection in EWOC specified by DATA."
  (nrepl-close (get-buffer data))
  (cider--update-connections-display ewoc cider-connections))

(defun cider-connections-goto-connection ()
  "Goto connection at point in the connection browser."
  (interactive)
  (cider--ewoc-apply-at-point #'cider--connections-goto-connection))

(defun cider--connections-goto-connection (_ewoc data)
  "Goto the REPL for the connection in _EWOC specified by DATA."
  (let ((buffer (buffer-local-value 'nrepl-repl-buffer (get-buffer data))))
    (when buffer
      (select-window (display-buffer buffer)))))


(defun cider-display-connected-message ()
  "Message displayed on successful connection."
  (message "Connected.  %s" (cider-random-words-of-inspiration)))

;; TODO: Replace direct usage of such hooks with CIDER hooks,
;; that are connection type independent
(add-hook 'nrepl-connected-hook 'cider-display-connected-message)

;;; Evaluation helpers
(defun cider-ns-form-p (form)
  "Check if FORM is an ns form."
  (string-match-p "^[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" form))

(define-obsolete-function-alias 'cider-eval 'nrepl-request:eval "0.9")

(defun cider-nrepl-op-supported-p (op)
  "Check whether the current connection supports the nREPL middleware OP."
  (nrepl-op-supported-p op (cider-current-repl-buffer)))

(defun cider-nrepl-send-request (request callback)
  "Send REQUEST and register response handler CALLBACK.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
\"par1\" ... )."
  (nrepl-send-request request callback (cider-current-repl-buffer)))

(defun cider-nrepl-send-sync-request (request &optional abort-on-input)
  "Send REQUEST to the nREPL server synchronously.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along.
If ABORT-ON-INPUT is non-nil, the function will return nil at the first
sign of user input, so as not to hang the interface."
  (nrepl-send-sync-request request (cider-current-repl-buffer) abort-on-input))

(defun cider-tooling-eval (input callback &optional ns)
  "Send the request INPUT and register the CALLBACK as the response handler.
NS specifies the namespace in which to evaluate the request."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-request:eval input callback ns (cider-current-tooling-session)))

(declare-function cider-find-relevant-connection "cider-interaction")
(defun cider-current-repl-buffer ()
  "The current REPL buffer.
Return the REPL buffer given by using `cider-find-relevant-connection' and
falling back to `cider-default-connection'.
If current buffer is a file buffer, and if the REPL has siblings, instead
return the sibling that corresponds to the current file extension.  This
allows for evaluation to be properly directed to clj or cljs REPLs depending
on where they come from."
  (-when-let (repl-buf (or (cider-find-relevant-connection)
                           (cider-default-connection 'no-error)))
    ;; Take the extension of current file, or nil if there is none.
    (let ((ext (file-name-extension (or (buffer-file-name) ""))))
      ;; Go to the "globally" active REPL buffer.
      (with-current-buffer repl-buf
        ;; If it has siblings, check which of them is associated with this file
        ;; extension.
        (or (cdr-safe (assoc ext nrepl-sibling-buffer-alist))
            ;; If it has no siblings, or if this extension is not specified,
            ;; fallback on the old behavior to just return the currently active
            ;; REPL buffer (which is probably just `repl-buf').
            nrepl-repl-buffer)))))

(declare-function cider-interrupt-handler "cider-interaction")
(defun cider-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (let ((pending-request-ids (cider-util--hash-keys nrepl-pending-requests)))
      (dolist (request-id pending-request-ids)
        (nrepl-request:interrupt
         request-id
         (cider-interrupt-handler (current-buffer))
         (cider-current-repl-buffer)
         (cider-current-session))))))

(defun cider-current-session ()
  "The REPL session to use for this buffer."
  (with-current-buffer (cider-current-repl-buffer)
    nrepl-session))

(define-obsolete-function-alias 'nrepl-current-session 'cider-current-session "0.10")

(defun cider-current-tooling-session ()
  "Return the current tooling session."
  (with-current-buffer (cider-current-repl-buffer)
    nrepl-tooling-session))

(define-obsolete-function-alias 'nrepl-current-tooling-session 'cider-current-tooling-session "0.10")

(defun cider--var-choice (var-info)
  "Prompt to choose from among multiple VAR-INFO candidates, if required.
This is needed only when the symbol queried is an unqualified host platform
method, and multiple classes have a so-named member.  If VAR-INFO does not
contain a `candidates' key, it is returned as is."
  (let ((candidates (nrepl-dict-get var-info "candidates")))
    (if candidates
        (let* ((classes (nrepl-dict-keys candidates))
               (choice (completing-read "Member in class: " classes nil t))
               (info (nrepl-dict-get candidates choice)))
          info)
      var-info)))

(defun cider-var-info (var &optional all)
  "Return VAR's info as an alist with list cdrs.
When multiple matching vars are returned you'll be prompted to select one,
unless ALL is truthy."
  (when (and var (not (string= var "")))
    (let ((var-info (cider-sync-request:info var)))
      (if all var-info (cider--var-choice var-info)))))

(defun cider-member-info (class member)
  "Return the CLASS MEMBER's info as an alist with list cdrs."
  (when (and class member)
    (cider-sync-request:info nil class member)))


;;; Requests

(declare-function cider-load-file-handler "cider-interaction")
(defun cider-request:load-file (file-contents file-path file-name &optional callback)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded. If CALLBACK is nil, use `cider-load-file-handler'."
  (cider-nrepl-send-request (list "op" "load-file"
                                  "session" (cider-current-session)
                                  "file" file-contents
                                  "file-path" file-path
                                  "file-name" file-name)
                            (or callback
                                (cider-load-file-handler (current-buffer)))))


;;; Sync Requests
(declare-function cider-current-ns "cider-interaction")
(defun cider-sync-request:apropos (query &optional search-ns docs-p privates-p case-sensitive-p)
  "Send \"apropos\" op with args SEARCH-NS, DOCS-P, PRIVATES-P, CASE-SENSITIVE-P."
  (-> `("op" "apropos"
        "ns" ,(cider-current-ns)
        "query" ,query
        ,@(when search-ns `("search-ns" ,search-ns))
        ,@(when docs-p '("docs?" "t"))
        ,@(when privates-p '("privates?" "t"))
        ,@(when case-sensitive-p '("case-sensitive?" "t")))
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "apropos-matches")))

(declare-function cider-ensure-op-supported "cider-interaction")
(defun cider-sync-request:classpath ()
  "Return a list of classpath entries."
  (cider-ensure-op-supported "classpath")
  (-> (list "op" "classpath"
            "session" (cider-current-session))
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "classpath")))

(defun cider-sync-request:complete (str context)
  "Return a list of completions for STR using nREPL's \"complete\" op."
  (-when-let (dict (-> (list "op" "complete"
                             "session" (cider-current-session)
                             "ns" (cider-current-ns)
                             "symbol" str
                             "context" context)
                       (cider-nrepl-send-sync-request 'abort-on-input)))
    (nrepl-dict-get dict "completions")))

(defun cider-sync-request:info (symbol &optional class member)
  "Send \"info\" op with parameters SYMBOL or CLASS and MEMBER."
  (let ((var-info (-> `("op" "info"
                        "session" ,(cider-current-session)
                        "ns" ,(cider-current-ns)
                        ,@(when symbol (list "symbol" symbol))
                        ,@(when class (list "class" class))
                        ,@(when member (list "member" member)))
                      (cider-nrepl-send-sync-request))))
    (if (member "no-info" (nrepl-dict-get var-info "status"))
        nil
      var-info)))

(defun cider-sync-request:eldoc (symbol &optional class member)
  "Send \"eldoc\" op with parameters SYMBOL or CLASS and MEMBER."
  (-when-let (eldoc (-> `("op" "eldoc"
                          "session" ,(cider-current-session)
                          "ns" ,(cider-current-ns)
                          ,@(when symbol (list "symbol" symbol))
                          ,@(when class (list "class" class))
                          ,@(when member (list "member" member)))
                        (cider-nrepl-send-sync-request 'abort-on-input)))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:ns-list ()
  "Get a list of the available namespaces."
  (-> (list "op" "ns-list"
            "session" (cider-current-session))
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "ns-list")))

(defun cider-sync-request:ns-vars (ns)
  "Get a list of the vars in NS."
  (-> (list "op" "ns-vars"
            "session" (cider-current-session)
            "ns" ns)
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "ns-vars")))

(defun cider-sync-request:resource (name)
  "Perform nREPL \"resource\" op with resource name NAME."
  (-> (list "op" "resource"
            "name" name)
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "resource-path")))

(defun cider-sync-request:resources-list ()
  "Perform nREPL \"resource\" op with resource name NAME."
  (-> (list "op" "resources-list")
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "resources-list")))

(defun cider-sync-request:format-code (code)
  "Perform nREPL \"format-code\" op with CODE."
  (-> (list "op" "format-code"
            "code" code)
      (cider-nrepl-send-sync-request)
      (nrepl-dict-get "formatted-code")))

(defun cider-sync-request:format-edn (edn &optional right-margin)
  "Perform \"format-edn\" op with EDN and RIGHT-MARGIN."
  (let* ((response (-> (list "op" "format-edn"
                             "edn" edn)
                       (append (and right-margin (list "right-margin" right-margin)))
                       (cider-nrepl-send-sync-request)))
         (err (nrepl-dict-get response "err")))
    (when err
      ;; err will be a stacktrace with a first line that looks like:
      ;; "clojure.lang.ExceptionInfo: Unmatched delimiter ]"
      (error (car (split-string err "\n"))))
    (nrepl-dict-get response "formatted-edn")))

(provide 'cider-client)

;;; cider-client.el ends here
