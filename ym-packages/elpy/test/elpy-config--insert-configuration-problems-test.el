;; This is deliberately short. The function is primarily concerned
;; with output. We will test its constituent functions later, and just
;; make sure it doesn't throw an error here.

(ert-deftest elpy-config--insert-configuration-problems-should-not-fail ()
  (elpy-testcase ()
    (elpy-config--insert-configuration-problems)))
