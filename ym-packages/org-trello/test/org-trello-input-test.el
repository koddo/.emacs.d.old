(require 'org-trello-input)

(ert-deftest test-orgtrello-input-read-string-completion ()
  (should (eq :res-with-ido
              (let ((org-trello-input-completion-mechanism 'default))
                (with-mock
                  (mock (ido-completing-read :prompt :choices nil 'do-match) => :res-with-ido)
                  (orgtrello-input-read-string-completion :prompt :choices)))))
  ;; cask declares helm as test dependency
  (should (eq :res-with-helm
              (let ((org-trello-input-completion-mechanism 'other))
                (with-mock
                  (mock (helm-comp-read :prompt :choices) => :res-with-helm)
                  (orgtrello-input-read-string-completion :prompt :choices))))))

(ert-deftest test-orgtrello-input-read-not-empty ()
  (should (equal "something"
                 (with-mock
                   (mock (read-string "prompt: ") => " something ")
                   (orgtrello-input-read-not-empty "prompt: ")))))

(ert-deftest test-orgtrello-input-read-string ()
  (should (equal :something
                 (with-mock
                   (mock (read-string "prompt: ") => :something)
                   (orgtrello-input-read-string "prompt: ")))))
