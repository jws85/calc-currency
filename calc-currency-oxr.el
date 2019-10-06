;;; calc-currency-oxr.el --- Fetches currency rates from openexchangerates.org -*- lexical-binding: t; -*-
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Keywords: calc, currency, exchange
;; Homepage: https://github.com/jws85/calc-currency
;; Package-Requires: (calc-currency)

;;; Commentary:

;; Notes:
;; You will need to sign up and provide an App ID.
;; Open Exchange Rates can be used for free so long as you only submit 1000
;; requests a month or less.  Beyond that, and you'll need to pay.

;;; Code:

(require 'json)  ;; to read JSON
(eval-when-compile (require 'cl))   ;; for the loop macro

(require 'calc-currency-utils)

(defgroup calc-currency-oxr nil
  "Use Open Exchange Rates as backend for calc-currency.

You will need to sign up for an account at openexchangerates.org and
provide an App ID.  Open Exchange Rates is (as of 2017-05-20) free
so long as you do less than 1000 requests a month."
  :group 'calc-currency)

;; Where to save the exchange rates to
(defcustom calc-currency-oxr-app-id "APP_ID_HERE"
  "Your App ID for Open Exchange Rates.

You must enter a value in order to use this backend."
  :group 'calc-currency-oxr
  :type 'string)

(defcustom calc-currency-oxr-show-alternative nil
  "Whether to show alternative currencies or not.

OXR defines 'alternative currencies' as the following:
 - Cryptocurrencies like Bitcoin, Dogecoin, etc
 - Alternate (black market) valuations for currencies.  As of
   2017-05-20, this consists only of various valuations for
   the Venezuelan Bolívar.

Many people may wish to enable this, especially if you need
to know the value of Bitcoin in order to pay cryptolocker
ransoms...

On the other hand, the Venezuelan president Maduro has attempted
to censor sites hosting alternative valuations for his country's
currency[1], so... use discretion if you're in Venezuela?

[1] e.g. https://en.wikipedia.org/wiki/DolarToday"
  :group 'calc-currency-oxr
  :type 'boolean)

(defvar *calc-currency-oxr-api-url* "https://openexchangerates.org/api/")

(defun calc-currency-oxr-url (endpoint)
  "Build the OXR endpoint URL.

ENDPOINT is a string representing the name of the endpoint e.g.
\"latest\", \"currencies\"."
  (if (equal calc-currency-oxr-app-id "APP_ID_HERE")
      (error "Please sign up for an OpenExchangeRates App ID!")
    (concat *calc-currency-oxr-api-url*
            endpoint
            ".json"
            "?app_id=" calc-currency-oxr-app-id
            (if calc-currency-oxr-show-alternative "&show_alternative=1" ""))))

(defun calc-currency-oxr-download-currency-table ()
  "Download the OXR currency names, and slurp the JSON into a list.

Note that GETting currencies.json does NOT count against your usage limit!"
  (json-read-from-string
   (calc-currency-utils-fetch-file (calc-currency-oxr-url "currencies"))))

(defun calc-currency-oxr-download-rates ()
  "Download the latest exchange rates from OXR, and slurp the JSON into a list.

This function returns the filename of the downloaded JSON file."
  (json-read-from-string
   (calc-currency-utils-fetch-file (calc-currency-oxr-url "latest"))))

(defun calc-currency-oxr-process-rates (rate-data)
  "Return an alist representing the exchange rates from OXR in RATE-DATA."
  (let ((raw-rates (assqv 'rates rate-data)))
    (loop for rate in raw-rates
          collect (cons
                   (read (replace-regexp-in-string "[^A-Za-z]" ""
                                                   (prin1-to-string (car rate))))
                   (cdr rate)))))

(defun calc-currency-oxr-get-timestamp (rate-data)
  "Return the Unix timestamp of the exchange rate update from RATE-DATA."
  (assqv 'timestamp rate-data))

(defun calc-currency-oxr-list (base-currency)
  "Build a list of rates from OpenExchangeRates using BASE-CURRENCY."
  (let* ((rate-data-raw (calc-currency-oxr-download-rates))
         (rate-data (calc-currency-oxr-process-rates rate-data-raw))
         (currency-data (calc-currency-oxr-download-currency-table))
         (rate-list (calc-currency-utils-build-list rate-data
                                                    currency-data
                                                    base-currency))
         (timestamp (calc-currency-oxr-get-timestamp rate-data-raw)))
    (list 'oxr timestamp rate-list)))

(provide 'calc-currency-oxr)

;;; calc-currency-oxr.el ends here
