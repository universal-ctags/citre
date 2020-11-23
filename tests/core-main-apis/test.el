;; TODO: test the APIs more thoroughly.

(ert-deftest test-main-apis ()
  "Test main APIs."
  (let ((records (citre-core-get-records
                  (expand-test-file)
                  "z" 'exact nil :require '(name input kind line))))
    (should (equal
             (mapcar (lambda (record) (citre-core-get-field 'name record))
                     records)
             '("z" "z")))
    (should (equal
             (mapcar (lambda (record) (citre-core-get-field 'input record))
                     records)
             '("src/input.h" "src/input.h")))
    (should (equal
             (mapcar (lambda (record) (citre-core-get-field 'kind record))
                     records)
             '("member" "member")))
    (should (equal
             (mapcar (lambda (record) (citre-core-get-field 'line record))
                     records)
             '(18 9)))))
