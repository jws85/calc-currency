;;; calc-currency.el --- Fetches currency exchange rates for Calc -*- lexical-binding: t; -*-

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Keywords: calc, currency, exchange

;;; Code:

(eval-when-compile (require 'cl))   ;; for the loop macro

(require 'calc-units)

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
  "How many days to wait between updates of the exchange rate table."
  :group 'calc-currency
  :type 'integer)

;; The currency to use as the base for the final table
(defcustom calc-currency-base-currency 'USD
  "Which currency to use as the base currency of the final table."
  :group 'calc-currency
  :type 'symbol)

(defcustom calc-currency-backend-function #'calc-currency-ecb-list
  "What backend function to use; the name should end in -list."
  :group 'calc-currency
  :type 'function)

;; necessary for write-currency-unit-table to work properly
(setq-local eval-expression-print-length nil)
(defun calc-currency-update-file ()
  "Writes the latest exchange rate table to a file."
  (condition-case err
      (let ((rate-list (funcall calc-currency-backend-function calc-currency-base-currency)))
        (write-region
         (pp rate-list)
         nil
         calc-currency-exchange-rates-file)
        (message "Fetched new exchange rates!")
        t)
    (error
     (message (format "Error updating; using existing rates instead: [%s]" err)))))

(defun calc-currency-read-file ()
  "Read in the exchange rates table."
  (with-temp-buffer
    (insert-file-contents calc-currency-exchange-rates-file)
    (read (buffer-string))))

(defun calc-currency-undefine-units (units)
  "Undefine the units defined in the unit table UNITS."
  (loop for unit in units
        do (calc-undefine-unit-if-exists (car unit))))

(defun calc-currency-load ()
  "Load exchange rates into Calc's units table.

This function will load exchange rates into Emacs Calc.  It does this
by downloading exchange rate info from one of several services.  This
function automatically downloads new exchange rates after a
user-specified number of days."
  (progn
    (if (file-readable-p calc-currency-exchange-rates-file)
        (let* ((cached-file (calc-currency-read-file))
               (cached-units (nth 2 cached-file))
               (timestamp (nth 1 cached-file)))
          (calc-currency-undefine-units cached-units)
          ;; if file is old, download a new file
          (if (> (difference-in-days timestamp) calc-currency-update-interval)
              (calc-currency-update-file)))
      (calc-currency-update-file))
    (setq math-additional-units (append math-additional-units (nth 2 (calc-currency-read-file)))
          math-units-table nil)))

(provide 'calc-currency)

;;; calc-currency.el ends here
