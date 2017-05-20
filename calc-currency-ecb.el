;;; calc-currency-ecb.el --- Fetches currency rates from the European Central Bank

;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Time-stamp: <2017-05-20 15:32:12 jws>

;; Notes:
;; This only updates daily -- so anyone looking for more latency is out of luck.

(require 'xml)  ;; to read XML files
(require 'cl)   ;; for the loop macro

(require 'calc-currency-utils)

;; The XML file containing the exchange rates
;; This one is provided by the European Union.
(defvar *calc-currency-ecb-exchange-rates-url*
  "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")

(defun calc-currency-ecb-download-rates ()
  "Download the latest exchange rates, return the file they were downloaded to"
  (calc-currency-utils-fetch-file *exchange-rates-url* "ecb"))

(defun calc-currency-ecb-process-currency (node)
  "Used by `process-currency-rates` to turn an XML currency node to a single cons cell relation."
  (let* ((attrs (xml-node-attributes node))
         (code (read (assqv 'currency attrs)))
         (rate (string-to-number (assqv 'rate attrs))))
    (cons code rate)))

(defun calc-currency-ecb-process-rates ()
  "Reads the exchange rate XML and transforms it into an alist that relates ISO codes to exchange rates."
  (let* ((xml (xml-parse-file (calc-currency-ecb-download-rates)))
         (grandpappy-cube (xml-get-children (car xml) 'Cube))
         (pappy-cube (xml-get-children (car grandpappy-cube) 'Cube))
         (date (assq 'time (xml-node-attributes (car pappy-cube))))
         (baby-cubes (xml-get-children (car pappy-cube) 'Cube)))
    (cons (cons 'EUR 1)
          (loop for cube in baby-cubes
                collect (calc-currency-ecb-process-currency cube)))))

(provide 'calc-currency-ecb)
