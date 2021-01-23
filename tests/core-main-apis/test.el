;; TODO: test the APIs more thoroughly.

(ert-deftest test-main-apis ()
  "Test main APIs."
  (let ((tags (citre-core-get-tags
               (expand-test-file)
               "z" 'exact nil :require '(name input kind line))))
    (should (equal
             (mapcar (lambda (tag) (citre-core-get-field 'name tag))
                     tags)
             '("z" "z")))
    (should (equal
             (mapcar (lambda (tag) (citre-core-get-field 'input tag))
                     tags)
             '("src/input.h" "src/input.h")))
    (should (equal
             (mapcar (lambda (tag) (citre-core-get-field 'kind tag))
                     tags)
             '("member" "member")))
    (should (equal
             (mapcar (lambda (tag) (citre-core-get-field 'line tag))
                     tags)
             '(18 9)))))
