;;; setags.el --- Ctags facility  -*- lexical-binding: t -*-

;; Copyright (C) Axel Forsman

;; Author: Axel Forsman <axel@axelf.se>
;; Maintainer: Axel Forsman <axel@axelf.se>
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools
;; URL: https://github.com/axelf4/setags

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parser and xref backend for extended vi tags files.

;; A tag is an identifier, appearing in a tags file, that can be
;; jumped to. Lines in a ctags tags file are of the form (see
;; <https://ctags.sourceforge.net/FORMAT>):

;;     {tagname}<Tab>{tagfile}<Tab>{tagaddress}[;"<Tab>{tagfield}..]

;; where "tagname" is an identifier; "tagfile" names the file where
;; the tag is defined; "tagaddress" is Ex search commands and/or line
;; numbers for locating the tag within its file; and each "tagfield"
;; is an optional colon-separated name/value pair. With lines sorted
;; by ASCII byte values, binary search for desired tags is viable.

;; To use, add `setags-xref-backend' to `xref-backend-functions', and
;; execute M-x setags-visit-tags-table RET for each tags file.

;;; Code:

(eval-when-compile (require 'cl-lib))
(declare-function xref-make "xref" (summary location))

(defconst setags--ctags-line-regexp
  (eval-when-compile
    (let ((cmd "/\\(?:[^/\\]\\|\\\\.\\)*/\\|?\\(?:[^?\\]\\|\\\\.\\)*\\?\\|[0-9]+"))
      (concat "^\\([^\t]+\\)\t\\([^\t]+\\)\t\\(" cmd
              "\\(?:;\\(?:" cmd "\\)\\)*\\)\\(?:;\"\t\\(.*\\)\\)?$"))))

(defvar setags-table-list () "Tags file buffers to search.")

(cl-defstruct (setags-tag (:constructor nil) (:copier nil)
                          (:constructor setags--make-tag (tagname file address fields)))
  "Ctags line." tagname file address fields)

(defun setags--find-in-file (tagname tables &optional cur-ffname ignore-case regexpp)
  "Search for tags matching TAGNAME in current tags file buffer.
If non-nil REGEXPP, TAGNAME is a regular expression, otherwise it is
taken literally, as a full tag name. Push matches to lists in 4 long
vector TABLES, ordered by precedence. Non-nil CUR-FFNAME prioritizes
tags in that file."
  (goto-char (point-min))
  (let ((head (unless regexpp tagname))
        (case-fold-search ignore-case)
        (tag-file-sorted t) lo (hi (point-max)) binaryp)
    ;; Parse tag file information pseudo-tag lines
    (while (looking-at "!_TAG_" t)
      (when (looking-at "!_TAG_FILE_SORTED\t\\([0-2]\\)\t")
        (let ((val (char-after (match-beginning 1))))
          (setq tag-file-sorted (cond ((= val ?0) nil)
                                      ((= val ?1) t)
                                      ((= val ?2) 'case-fold)))))
      (forward-line))
    (setq lo (point)
          binaryp (and head (eq tag-file-sorted (if ignore-case 'case-fold t))))
    (when binaryp ; Binary search for the 1st matching tag
      (while (< lo hi)
        (goto-char (+ lo (/ (- hi lo) 2)))
        (forward-line 0)
        (let* ((off (point))
               (s (buffer-substring-no-properties
                   off (progn (skip-chars-forward "^\t") (point))))
               (cmp (compare-strings s nil nil head nil nil ignore-case)))
          (if (or (eq cmp t) (> cmp 0))
              (setq hi off)
            (setq lo (pos-bol 2)))))
      (goto-char lo))
    ;; Search unstructuredly and filter out false positives. After
    ;; binary search: Step forward linewise via EOL bound.
    (while (or (funcall (if regexpp #'search-forward-regexp #'search-forward)
                        tagname (when binaryp (pos-eol)) t)
               (and binaryp regexpp (looking-at-p (regexp-quote head))))
      (when (if regexpp (progn (skip-chars-backward "^\t\n") (bolp))
              (and (= (char-after) ?\t) (goto-char (match-beginning 0)) (bolp)))
        (unless (looking-at setags--ctags-line-regexp) (error "Corrupted tag line"))
        (let* ((name (match-string-no-properties 1))
               (fname (match-string-no-properties 2))
               (address (match-string-no-properties 3))
               (extra (match-string-no-properties 4))
               (fields (and extra (split-string extra "\t")))
               (currentp (and cur-ffname (file-equal-p fname cur-ffname)))
               (staticp (member "file:" fields)))
          (push (setags--make-tag name (expand-file-name fname) address fields)
                (aref tables (if currentp (if staticp 0 1) (if staticp 3 2))))))
      (forward-line))))

(defun setags--find-tags (tagname)
  "Search for tags matching TAGNAME in tags files."
  (let ((fname buffer-file-name) (tables (make-vector 4 ())))
    (dolist (buf setags-table-list)
      (with-current-buffer buf (setags--find-in-file tagname tables fname)))
    (mapcan #'identity tables)))

(defun setags--transform-nomagic (regexp)
  "Transform Ex REGEXP into Emacs regular expression syntax.
Result behaves like `magic' was not set."
  (replace-regexp-in-string "\\(\\`\\|[^\\]\\)\\(\\\\\\{2\\}*[.*[+?]\\)"
                            "\\1\\\\\\2" regexp))

(defun setags--jumpto-tag (tagname address &optional ignore-case)
  (goto-char (point-min))
  (condition-case nil
      (let ((i 0) ch)
        (while (< i (length address))
          (if (<= ?0 (setq ch (aref address i)) ?9)
              (progn (string-match "[0-9]+" address i)
                     (goto-char (point-min))
                     (forward-line (1- (string-to-number (match-string 0 address)))))
            ;; Search for unescaped delimiter
            (string-match (format "[^\\]\\\\\\{2\\}*\\%c" ch) address (1+ i))
            (let ((regexp (setags--transform-nomagic
                           (substring address (1+ i) (1- (match-end 0)))))
                  (case-fold-search ignore-case))
              (re-search-forward regexp nil nil (if (= ch ?/) 1 -1))))
          (setq i (1+ (match-end 0)))))
    (search-failed
     (if (not ignore-case)
         (setags--jumpto-tag tagname address t) ; Try again, ignoring case
       (goto-char (point-min))
       (or ; Failed to find pattern, take a guess: "^func  ("
        (re-search-forward (concat "^" tagname "\\s-*(") nil t)
        ;; Guess again: "^char * \_<func  ("
        (re-search-forward (concat "^[#a-zA-Z_].*\\_<" tagname "\\s-*(") nil t)
        (error "Can't find tag pattern"))
       (message "Couldn't find tag, just guessing!")))))

;;;###autoload
(defun setags-visit-tags-table (file)
  "Add FILE to the list of tags files to search."
  (interactive
   (let ((dir (or (locate-dominating-file default-directory "tags")
                  default-directory)))
     (list (read-file-name (format-prompt "Visit tags table" "tags")
                           dir (file-name-concat dir "tags") t))))
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (goto-char (point-min))
      (unless (looking-at-p setags--ctags-line-regexp)
        (user-error "File %s is not a valid tags table" file)))
    (push buf setags-table-list)))

;;;###autoload
(defun setags-xref-backend () "Setags xref backend." 'setags)

(cl-defmethod xref-location-group ((l setags-tag)) (setags-tag-file l))

(cl-defmethod xref-location-marker ((l setags-tag))
  (with-current-buffer (find-file-noselect (setags-tag-file l))
    (save-excursion
      (save-restriction
        (widen)
        (setags--jumpto-tag (setags-tag-tagname l) (setags-tag-address l))
        (point-marker)))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'setags)))
  (find-tag-default))

(cl-defmethod xref-backend-definitions ((_backend (eql 'setags)) identifier)
  (mapcar (lambda (tag) (xref-make (setags-tag-tagname tag) tag))
          (setags--find-tags identifier)))

(provide 'setags)

;;; setags.el ends here
