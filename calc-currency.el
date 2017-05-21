;;; calc-currency.el --- Fetches currency exchange rates for Calc

;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Keywords: calc, currency, exchange
;; Time-stamp: <2017-05-21 00:39:23 jws>

;;; Notes:

(require 'cl)   ;; for the loop macro

(require 'calc-currency-utils)
(require 'calc-currency-ecb)

(defgroup calc-currency nil
  "Automatically load and update exchange rates as units in Emacs Calc."
  :group 'calc)

;; Where to save the exchange rates to
(defcustom calc-currency-exchange-rates-file
  (expand-file-name "calc-currency-rates.el" user-emacs-directory)
  "Where calc-currency saves the latest exchange rates to."
  :group 'calc-currency
  :type 'string)

;; How often to check for exchange rates
(defcustom calc-currency-update-interval 5
  "How often (integer number of days) to wait between updates of the exchange rate table."
  :group 'calc-currency
  :type 'integer)

;; The currency to use as the base for the final table
(defcustom calc-currency-base-currency 'USD
  "Which currency to use as the base currency of the final table."
  :group 'calc-currency
  :type 'symbol)

(defcustom calc-currency-backend #'calc-currency-ecb-module
  "What backend function to use; the name should end in -module"
  :group 'calc-currency
  :type 'function)

(defun build-currency-unit-table ()
  "Take the alist from `process-currency-rates` and transform it into a list structured like `math-additional-units`."
  (let* ((module (funcall calc-currency-backend))
         (rate-table (funcall (assqv 'process-rates module)))
         (currency-table (funcall (assqv 'currency-table module)))
         (base-currency calc-currency-base-currency)
         (base-rate (assqv base-currency rate-table))
         (base-desc (assqv base-currency currency-table))
         (rate-table-mod (assq-delete-all base-currency rate-table)))
    (cons (list base-currency nil base-desc)
          (loop for rate in rate-table
                collect (list
                         (car rate)
                         (format "%S / %f" base-currency (/ (cdr rate) base-rate))
                         (assqv (car rate) currency-table))))))

;; necessary for write-currency-unit-table to work properly
(setq-local eval-expression-print-length nil)
(defun write-currency-unit-table ()
  "Writes the exchange rate table to a file."
  (write-region
   (pp (build-currency-unit-table))
   nil
   calc-currency-exchange-rates-file))

(defun check-currency-unit-table ()
  "Check to see if the exchange rates table exists, or if it is up to date.
If it is not, fetch new data and write a new exchange rate table."
  (if (or (not (file-readable-p calc-currency-exchange-rates-file))
          (> (calc-currency-utils-file-age calc-currency-exchange-rates-file) calc-currency-update-interval))
      (progn
        (write-currency-unit-table)
        (message "Fetched new exchange rates!"))))

(defun read-currency-unit-table ()
  "Reads in the exchange rates table."
  (with-temp-buffer
    (insert-file-contents calc-currency-exchange-rates-file)
    (read (buffer-string))))

;; FIXME I'll go back and try the following code:
;;  - if unit exists in math-additional-units, update that entry
;;  - otherwise, add unit

;; FIXME This probably isn't the best way to handle this!
(defun calc-undefine-unit-if-exists (unit)
  "Deletes a unit from `math-additional-units` if it exists."
  (condition-case nil
      (calc-undefine-unit unit)
    (error nil)))

;; FIXME And this probably isn't the best way to handle this!
(defun calc-currency-load ()
  (progn
    (check-currency-unit-table)
    (let ((currency-unit-table (read-currency-unit-table)))
      ;; For each unit of currency, undefine it in math-additional-units
      (loop for unit in currency-unit-table
            do (calc-undefine-unit-if-exists (car unit)))

      ;; Then, add math-standard-units to the units table
      (setq math-additional-units (append math-additional-units (read-currency-unit-table))
            math-units-table nil))))

(provide 'calc-currency)
