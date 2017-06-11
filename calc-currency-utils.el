;;; calc-currency-utils.el --- Utility functions

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>

;;; Code:

(require 'cl-lib)
(require 'f)

(defun calc-currency-utils-fetch-file (url)
  "GET a file from the Web and return its contents as a string.

This function will HTTP GET a file from URL, and download it to a
temporary file; by default in /tmp."
  ; FIXME Windows support will require putting temp files elsewhere
  (let ((file (format "/tmp/exchange.%s.%06d.tmp"
                      (format-time-string "%Y%m%d.%H%M%S")
                      (random 999999))))
    (url-copy-file url file t)
    (f-read file)))

(defun assqv (key alist)
  "Find KEY in ALIST and return its `cdr`."
  (cdr (assq key alist)))

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
