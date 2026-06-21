;;; tests.el --- The setags test suite  -*- lexical-binding: t -*-

(require 'ert)
(require 'setags)

(ert-deftest setags-finds-tag-test ()
  (with-temp-buffer
    (insert "foo\tbar\t1")
    (let ((setags-table-list (list (current-buffer))))
      (should (equal (setags--find-tags "foo")
                     (list (setags--make-tag "foo" (expand-file-name "bar") "1" ())))))))

(ert-deftest setags-nomagic-test ()
  (should (string= (setags--transform-nomagic "\\\\. *")
                   "\\\\\\. \\*")))
