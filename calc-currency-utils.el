;;; calc-currency-utils.el --- Utility functions

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>

;;; Code:

(require 'cl-lib)

(defun calc-currency-utils-fetch-file (url file-infix file-suffix)
  "Fetch a file from the Web and download it to a file.

This function will HTTP GET a file from URL, and download it to a
temporary file.  The temporary file will have a name that includes
the string FILE-INFIX and will end in FILE-SUFFIX, which should be
a file extension like \"json\"."
  (let ((file (concat "/tmp/exchange." file-infix "." (format-time-string "%Y%m%d") "." file-suffix)))
    (url-copy-file url file t)
    file))

(defun assqv (key alist)
  "Find KEY in ALIST and return its `cdr`."
  (cdr (assq key alist)))

(defun calc-currency-utils-time-modified (file)
  "Return the last modification time of FILE.

The return value will be an Emacs time data structure, like `current-time`."
  (nth 5 (file-attributes file)))

(defun calc-currency-utils-file-age (file)
  "Return the number of days since FILE was last modified."
  (/ (float-time (time-subtract
                  (current-time)
                  (calc-currency-utils-time-modified file)))
     (* 60 60 24)))

(defun calc-currency-utils-build-list (rate-table currency-table base-currency)
  "Build a list of currencies formatted like 'math-additional-units'.

RATE-TABLE and CURRENCY-TABLE should both be alists, and the alist keys in
both should correspond to currency units.  BASE-CURRENCY is the unit to be
used as the base."
  (let* ((base-rate (assqv base-currency rate-table))
         (base-desc (assqv base-currency currency-table))
         (rate-table-mod (assq-delete-all base-currency rate-table)))
    (cons (list base-currency nil base-desc)
          (cl-loop for rate in rate-table
                   collect (list
                            (car rate)
                            (format "%S / %f" base-currency (/ (cdr rate) base-rate))
                            (assqv (car rate) currency-table))))))


(provide 'calc-currency-utils)

;;; calc-currency-utils.el ends here
