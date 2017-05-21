;;; calc-currency-utils.el --- Utility functions

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Time-stamp: <2017-05-21 01:53:36 jws>

;;; Code:

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

(provide 'calc-currency-utils)

;;; calc-currency-utils.el ends here
