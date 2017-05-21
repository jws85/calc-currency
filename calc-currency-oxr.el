;;; calc-currency-oxr.el --- Fetches currency rates from openexchangerates.org

;;; Commentary:
;; Author: J. W. Smith <jwsmith2spam at gmail dot com>
;; Time-stamp: <2017-05-21 02:05:15 jws>

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
  (concat *calc-currency-oxr-api-url*
          endpoint
          ".json"
          "?app_id=" calc-currency-oxr-app-id
          (if calc-currency-oxr-show-alternative "&show_alternative=1" "")))

(defun calc-currency-oxr-download-currency-table ()
  "Download the OXR currency names, and return the file they were downloaded to.
Note that GETting currencies.json does NOT count against your usage limit!"
  (calc-currency-utils-fetch-file (calc-currency-oxr-url "currencies")
                                  "oxr.currencies" "json"))

(defun calc-currency-oxr-currency-table ()
  "Return a table of all currencies supported by the OXR endpoint."
  (let ((file (calc-currency-oxr-download-currency-table)))
    (with-temp-buffer
      (insert-file-contents file)
      (json-read-from-string (buffer-string)))))

(defun calc-currency-oxr-download-rates ()
  "Download the latest exchange rates from OXR.

This function returns the filename of the downloaded JSON file."
  (calc-currency-utils-fetch-file (calc-currency-oxr-url "latest")
                                  "oxr.rates" "json"))

(defun calc-currency-oxr-process-rates ()
  "Return an alist representing the exchange rates from OXR."
  (let* ((file (calc-currency-oxr-download-rates))
         (json (with-temp-buffer
                 (insert-file-contents file)
                 (json-read-from-string (buffer-string))))
         (raw-rates (assqv 'rates json)))
    (loop for rate in raw-rates
          collect (cons
                   (read (replace-regexp-in-string "[^A-Za-z]" ""
                                                   (prin1-to-string (car rate))))
                   (cdr rate)))))

(defun calc-currency-oxr-module ()
  "Provide a consistent interface to the OXR backend functions."
  '((currency-table . calc-currency-oxr-currency-table)
    (download-rates . calc-currency-oxr-download-rates)
    (process-rates . calc-currency-oxr-process-rates)))

(provide 'calc-currency-oxr)

;;; calc-currency-oxr.el ends here
