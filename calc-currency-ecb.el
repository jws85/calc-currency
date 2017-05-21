;;; calc-currency-ecb.el --- Fetches currency rates from the European Central Bank

;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Time-stamp: <2017-05-20 23:32:34 jws>

;; Notes:
;; This only updates daily -- so anyone looking for more latency is out of luck.
;; But it is free, free, free... just be considerate and don't update too often!

(require 'xml)  ;; to read XML files
(require 'cl)   ;; for the loop macro

(require 'calc-currency-utils)

;; The XML file containing the exchange rates
;; This one is provided by the European Union.
(defvar *calc-currency-ecb-exchange-rates-url*
  "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")

(defun calc-currency-ecb-currency-table ()
  "Returns the table of all currencies supported by the ECB endpoint."
  '((USD . "United States dollar")
    (EUR . "European Union euro")
    (JPY . "Japanese yen")
    (BGN . "Bulgarian lev")
    (CZK . "Czech koruna")
    (DKK . "Danish kroner")
    (GBP . "British pound")
    (HUF . "Hungarian forint")
    (PLN . "Polish zloty")
    (RON . "Romanian new leu")
    (SEK . "Swedish krona")
    (CHF . "Swiss franc")
    (NOK . "Norwegian kroner")
    (HRK . "Croatian kuna")
    (RUB . "Russian ruble")
    (TRY . "Turkish new lira")
    (AUD . "Australian dollar")
    (BRL . "Brazilian real")
    (CAD . "Canadian dollar")
    (CNY . "Chinese yuan renminbi")
    (HKD . "Hong Kong dollar")
    (IDR . "Indonesian rupiah")
    (ILS . "Israeli new shekel")
    (INR . "Indian rupee")
    (KRW . "South Korean won")
    (MXN . "Mexican peso")
    (MYR . "Malaysian ringgit")
    (NZD . "New Zealand dollar")
    (PHP . "Philippine peso")
    (SGD . "Singaporean dollar")
    (THB . "Thai baht")
    (ZAR . "South African rand")))

(defun calc-currency-ecb-download-rates ()
  "Download the latest exchange rates, return the file they were downloaded to"
  (calc-currency-utils-fetch-file *exchange-rates-url* "ecb" "xml"))

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

(defun calc-currency-ecb-module ()
  "A function providing a consistent interface to the ECB backend functions."
  '((currency-table . calc-currency-ecb-currency-table)
    (download-rates . calc-currency-ecb-download-rates)
    (process-rates . calc-currency-ecb-process-rates)))

(provide 'calc-currency-ecb)
