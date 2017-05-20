;;; calc-currency-utils.el --- Utility functions

;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Time-stamp: <2017-05-20 15:26:04 jws>

(defun calc-currency-utils-fetch-file (url file-infix)
  "Fetch a file from `url`, download it into the tmp directory, and return its filename."
  (let ((file (concat "/tmp/exchange." file-infix "." (format-time-string "%Y%m%d") ".xml")))
    (url-copy-file url file t)
    file))

(defun assqv (key alist)
  "Finds `key` in `alist` and returns its `cdr`"
  (cdr (assq key alist)))

(defun calc-currency-utils-time-modified (file)
  "Return the time `file` was last modified, as a time value (e.g. like `current-time`)"
  (nth 5 (file-attributes file)))

(defun calc-currency-utils-file-age (file)
  "Returns the number of days since `file` was last modified"
  (/ (float-time (time-subtract
                  (current-time)
                  (calc-currency-utils-time-modified file)))
     (* 60 60 24)))

(provide 'calc-currency-utils)
