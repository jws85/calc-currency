;;; calc-currency.el --- Fetches currency exchange rates for Calc

;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Keywords: calc, currency, exchange
;; Time-stamp: <2017-05-18 21:33:45 jws>

;;; Notes:

(require 'xml)

;; The XML file containing the exchange rates
;; This one is provided by the European Union.
(defvar *exchange-rates-url* "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")

;; Where to copy the exchange rates to
(defvar *exchange-rates-file* "/tmp/emacs-exchange.xml")

;; How often to check for exchange rates
(defvar *exchange-rates-update-interval* 5)

;; The currency to use as the base for the final table
(defvar *base-currency* 'USD)

;; Names of currencies
(defvar *currency-names*
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

(defun time-last-modified (file)
  "Return the time `file` was last modified, as a time value (e.g. like `current-time`)"
  (nth 5 (file-attributes file)))

(defun file-age (file)
  "Returns the number of days since `file` was last modified"
  (/ (float-time (time-subtract
                  (current-time)
                  (time-last-modified file)))
     (* 60 60 24)))

(defun download-exchange-rates ()
  "Download the latest exchange rates"
  (url-copy-file *exchange-rates-url* *exchange-rates-file* t))

(defun check-exchange-rates ()
  "Check to see how old the exchange rates on disk are; if they are too old, download new data."
  (if (> (file-age *exchange-rates-file*) *exchange-rates-update-interval*)
      (download-exchange-rates)))

(defun assqv (key alist)
  "Finds `key` in `alist` and returns its `cdr`"
  (cdr (assq key alist)))

(defun process-currency (node)
  (let* ((attrs (xml-node-attributes node))
         (code (read (assqv 'currency attrs)))
         (rate (string-to-number (assqv 'rate attrs))))
    (cons code rate)))

(defun process-currency-rates ()
  (let* ((xml (xml-parse-file *exchange-rates-file*))
         (grandpappy-cube (xml-get-children (car xml) 'Cube))
         (pappy-cube (xml-get-children (car grandpappy-cube) 'Cube))
         (date (assq 'time (xml-node-attributes (car pappy-cube))))
         (baby-cubes (xml-get-children (car pappy-cube) 'Cube)))
    (cons (cons 'EUR 1)
          (loop for cube in baby-cubes
                collect (process-currency cube)))))

(defun build-currency-unit-table ()
  (let* ((rate-table (process-currency-rates))
         (base-rate (assqv *base-currency* rate-table))
         (base-desc (assqv *base-currency* *currency-names*))
         (rate-table-mod (assq-delete-all *base-currency* rate-table)))
    (cons (list *base-currency* nil base-desc)
          (loop for rate in rate-table
                collect (list
                         (car rate)
                         (format "%f * %S" (/ (cdr rate) base-rate) *base-currency*)
                         (assqv (car rate) *currency-names*))))))

;; necessary for write-currency-unit-table to work properly
(setq eval-expression-print-length nil)
(defun write-currency-unit-table ()
  (write-region
   (pp (build-currency-unit-table))
   nil
   "/tmp/currency.el"))

(defun calc-currency-load ()
  (progn
    (check-exchange-rates)
