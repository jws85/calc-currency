;;; calc-currency-utils-test.el --- Test of utility functions

;;; Commentary:

;;; Code:

(require 'calc-currency-utils)

;;; calc-currency-utils

;; calc-currency-utils-build-list
(ert-deftest calc-currency-utils-build-list-test-1 ()
  (let ((rate-table '((USD . 1.0) (CAD . 1.2) (GBP . 0.8) (JPY . 100.0) (KRW . 1000.0)))
        (currency-table '((USD . "US dollar") (CAD . "Canadian dollar") (GBP . "UK pound")
                          (JPY . "Japanese yen") (KRW . "South Korean won"))))
    (should (equal (calc-currency-utils-build-list rate-table currency-table 'USD)
                   '((USD nil "US dollar")
                     (CAD "USD / 1.200000" "Canadian dollar")
                     (GBP "USD / 0.800000" "UK pound")
                     (JPY "USD / 100.000000" "Japanese yen")
                     (KRW "USD / 1000.000000" "South Korean won"))))))

(ert-deftest calc-currency-utils-build-list-test-2 ()
  (let ((rate-table '((USD . 1.0) (CAD . 1.2) (GBP . 0.8) (JPY . 100.0) (KRW . 1000.0)))
        (currency-table '((USD . "US dollar") (CAD . "Canadian dollar") (GBP . "UK pound")
                          (JPY . "Japanese yen") (KRW . "South Korean won"))))
    (should (equal (calc-currency-utils-build-list rate-table currency-table 'JPY)
                   '((JPY nil "Japanese yen")
                     (USD "JPY / 0.010000" "US dollar")
                     (CAD "JPY / 0.012000" "Canadian dollar")
                     (GBP "JPY / 0.008000" "UK pound")
                     (KRW "JPY / 10.000000" "South Korean won"))))))

;; 9time-to-4time
(ert-deftest 9time-to-4time-test-1 ()
  (should (equal (9time-to-4time '(0 0 0 1 1 1970 nil nil 0))
                 '(0 0))))

(ert-deftest 9time-to-4time-test-2 ()
  (should (equal (9time-to-4time '(45 30 12 5 6 2017 nil nil -14400))
                 (seconds-to-time 1496680245))))

;; epoch-time-from-string
(ert-deftest epoch-time-from-string-test-1 ()
  (should (equal (epoch-time-from-string "1970-01-01 00:00:00 +0000")
                 0.0)))

(ert-deftest epoch-time-from-string-test-2 ()
  (should (equal (epoch-time-from-string "2017-06-05 12:30:45 -0400")
                 1496680245.0)))

;; difference-in-days
(ert-deftest difference-in-days-test-1 ()
  (should (equal (difference-in-days 1496593845 1496680245) 1)))

(ert-deftest difference-in-days-test-2 ()
  (should (equal (difference-in-days 1465144245 1496680245) 365)))

(ert-deftest difference-in-days-test-3 ()
  (should (equal (difference-in-days 946684800 978307200) 366)))

(ert-deftest difference-in-days-test-4 ()
  (should (equal (difference-in-days 946684800 946728000) 0)))

;; assqv
(ert-deftest assqv-test-1 ()
  (should (equal (assqv 'baz '((foo . 42) (bar. 69) (baz . 1337) (quux . 9001)))
                 1337)))

(ert-deftest assqv-test-2 ()
  (should (equal (assqv 'foo '((foo . 42)))
                 42)))

(ert-deftest assqv-test-3 ()
  (should (equal (assqv 'foo '())
                 nil)))

(provide 'calc-currency-utils-test)

;;; calc-currency-utils-test.el ends here
