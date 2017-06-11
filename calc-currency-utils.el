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

(defun 9time-to-4time (9time)
  "Convert a 9TIME to a 4TIME.

The 'parse-time-string' function in parse-time.el, and a few other
functions, create 9-tuples (e.g. lists with nine things in them).
This structure is documented here:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Conversion.html
It's actually a pretty simple format.

Other functions (notably 'float-time') expect a list that can have up to
4 items in it (but in my experience only has two).  This structure is
documented here, as the return value of 'current-time':
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-of-Day.html

This function attempts to convert the 9-tuple format to the 2/3/4-tuple
format, so that it can then be used with 'float-time' et al."
  (let ((sec (nth 0 9time))
        (min (nth 1 9time))
        (hr  (nth 2 9time))
        (day (nth 3 9time))
        (mon (nth 4 9time))
        (yr  (nth 5 9time))
        (tz  (nth 8 9time)))
    (encode-time sec min hr day mon yr tz)))

(defun epoch-time-from-string (time-string)
  "Take TIME-STRING, return Unix epoch time.

TIME-STRING should be anything accepted by 'parse-time-string', including
ISO time formats like:

2017-06-09 16:00:00 +0100"
  (float-time (9time-to-4time (parse-time-string time-string))))

(defun difference-in-days (timestamp1 &optional timestamp2)
  "Return difference in days between TIMESTAMP1 and TIMESTAMP2.

The timestamps are Unix epoch times; TIMESTAMP1 should always represent
an earlier time (assuming computer clocks aren't set incorrectly).

If TIMESTAMP2 is not specified, it will be the value of 'float-time'."
  (let ((timestamp (if (equal timestamp2 nil) (float-time) timestamp2)))
    (/ (- timestamp timestamp1)
       (* 60 60 24))))

(defun assqv (key alist)
  "Find KEY in ALIST and return its `cdr`."
  (cdr (assq key alist)))

(defun calc-undefine-unit-if-exists (unit)
  "Delete a unit UNIT from 'math-additional-units', if it exists."
  (condition-case nil
      (calc-undefine-unit unit)
    (error nil)))

(provide 'calc-currency-utils)

;;; calc-currency-utils.el ends here
