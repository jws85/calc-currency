;;; calc-currency-init.el --- Initializes tests

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Keywords: calc, currency, exchange
;; Time-stamp: <2017-05-29 18:09:10 jws>

;;; Code:

(require 'f)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar library-path
  (f-parent test-path))

(add-to-list 'load-path library-path)

;;; calc-currency-init.el ends here
