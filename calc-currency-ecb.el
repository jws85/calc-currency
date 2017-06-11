;;; calc-currency-ecb.el --- Fetches currency rates from the European Central Bank

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>

;; Notes:
;; This only updates daily -- so anyone looking for more latency is out of luck.
;; But it is free, free, free... just be considerate and don't update too often!

;;; Code:

(require 'xml)  ;; to read XML files
(eval-when-compile (require 'cl))   ;; for the loop macro

(require 'calc-currency-utils)

;; The XML file containing the exchange rates
;; This one is provided by the European Union.
(defvar *calc-currency-ecb-exchange-rates-url*
  "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")

;; The ECB updates at 16:00 CET:
;; https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html
(defvar *calc-currency-ecb-update-time*
  "16:00:00 +0100")

(defun calc-currency-ecb-currency-table ()
  "Return a table of all currencies supported by the ECB endpoint."
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
  "Download the latest exchange rates from the ECB.

This function returns the filename of the downloaded XML file."
  (calc-currency-utils-fetch-file *calc-currency-ecb-exchange-rates-url*))

(defun my-xml-parse-string (string)
  "Parse the XML in STRING."
  (with-temp-buffer
    (insert string)
    (xml-parse-region)))

(defun calc-currency-ecb-process-currency (node)
  "Helper function for `calc-currency-ecb-process-rates`.

This takes a NODE, which should correspond to one of the currency
<Cube> entries in the XML file, and returns a cons cell with that
same information."
  (let* ((attrs (xml-node-attributes node))
         (code (read (assqv 'currency attrs)))
         (rate (string-to-number (assqv 'rate attrs))))
    (cons code rate)))

(defun calc-currency-ecb-process-rates (xml-string)
  "Return an alist representing the exchange rates from the ECB in XML-STRING."
  (let* ((xml (my-xml-parse-string xml-string))
         (grandpappy-cube (xml-get-children (car xml) 'Cube))
         (pappy-cube (xml-get-children (car grandpappy-cube) 'Cube))
         (baby-cubes (xml-get-children (car pappy-cube) 'Cube)))
    (cons (cons 'EUR 1.0)
          (loop for cube in baby-cubes
                collect (calc-currency-ecb-process-currency cube)))))

(defun calc-currency-ecb-get-timestamp (xml-string)
  "Return the timestamp of the ECB rates in XML-STRING."
  (let* ((xml (my-xml-parse-string xml-string))
         (grandpappy-cube (xml-get-children (car xml) 'Cube))
         (pappy-cube (xml-get-children (car grandpappy-cube) 'Cube))
         (date (assqv 'time (xml-node-attributes (car pappy-cube)))))
    (message date)
    (epoch-time-from-string (concat date " " *calc-currency-ecb-update-time*))))

(defun calc-currency-ecb-list (base-currency)
  "Build a list of rates from the ECB using BASE-CURRENCY."
  (let* ((xml-string (calc-currency-ecb-download-rates))
         (rate-table (calc-currency-ecb-process-rates xml-string))
         (currency-table (calc-currency-ecb-currency-table))
         (rate-list (calc-currency-utils-build-list rate-table
                                                    currency-table
                                                    base-currency))
         (timestamp (calc-currency-ecb-get-timestamp xml-string)))
    (list 'ecb timestamp rate-list)))

(provide 'calc-currency-ecb)

;;; calc-currency-ecb.el ends here
