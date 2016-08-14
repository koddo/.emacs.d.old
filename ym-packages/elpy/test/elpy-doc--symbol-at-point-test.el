(ert-deftest elpy-doc--symbol-at-point-should-return-simple-name ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert "foo")
    (should (equal (elpy-doc--symbol-at-point)
                   "foo"))))

(ert-deftest elpy-doc--symbol-at-point-should-return-dotted-name ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert "foo.bar.baz")
    (should (equal (elpy-doc--symbol-at-point)
                   "foo.bar.baz"))))

(ert-deftest elpy-doc--symbol-at-point-should-return-nil-for-none ()
  (elpy-testcase ((:project project-root "test.py"))
    (find-file (f-join project-root "test.py"))
    (insert "foo.bar.baz\n")
    (should (equal (elpy-doc--symbol-at-point)
                   nil))))