;;; calc-currency-test.el --- Test of stuff

;;; Commentary:

;;; Code:

(require 'calc-currency)
(require 'calc-currency-utils)

(require 'cl-lib)

(defvar *conversion-file*
  "((USD nil \"United States dollar\")
 (EUR \"USD / 0.893815\" \"European Union euro\")
 (JPY \"USD / 111.342510\" \"Japanese yen\")
 (BGN \"USD / 1.748123\" \"Bulgarian lev\")
 (CZK \"USD / 23.627994\" \"Czech koruna\")
 (DKK \"USD / 6.650429\" \"Danish kroner\")
 (GBP \"USD / 0.778450\" \"British pound\")
 (HUF \"USD / 275.151949\" \"Hungarian forint\")
 (PLN \"USD / 3.735878\" \"Polish zloty\")
 (RON \"USD / 4.079192\" \"Romanian new leu\")
 (SEK \"USD / 8.692170\" \"Swedish krona\")
 (CHF \"USD / 0.974258\" \"Swiss franc\")
 (NOK \"USD / 8.407669\" \"Norwegian kroner\")
 (HRK \"USD / 6.626296\" \"Croatian kuna\")
 (RUB \"USD / 56.617537\" \"Russian ruble\")
 (TRY \"USD / 3.577226\" \"Turkish new lira\")
 (AUD \"USD / 1.344744\" \"Australian dollar\")
 (BRL \"USD / 3.265821\" \"Brazilian real\")
 (CAD \"USD / 1.345459\" \"Canadian dollar\")
 (CNY \"USD / 6.856811\" \"Chinese yuan renminbi\")
 (HKD \"USD / 7.794244\" \"Hong Kong dollar\")
 (IDR \"USD / 13314.998212\" \"Indonesian rupiah\")
 (ILS \"USD / 3.572399\" \"Israeli new shekel\")
 (INR \"USD / 64.537898\" \"Indian rupee\")
 (KRW \"USD / 1121.058277\" \"South Korean won\")
 (MXN \"USD / 18.509385\" \"Mexican peso\")
 (MYR \"USD / 4.270200\" \"Malaysian ringgit\")
 (NZD \"USD / 1.411512\" \"New Zealand dollar\")
 (PHP \"USD / 49.797104\" \"Philippine peso\")
 (SGD \"USD / 1.384430\" \"Singaporean dollar\")
 (THB \"USD / 34.115123\" \"Thai baht\")
 (ZAR \"USD / 12.947533\" \"South African rand\"))")

(defvar *conversion-table*
  '((USD nil "United States dollar")
    (EUR "USD / 0.893815" "European Union euro")
    (JPY "USD / 111.342510" "Japanese yen")
    (BGN "USD / 1.748123" "Bulgarian lev")
    (CZK "USD / 23.627994" "Czech koruna")
    (DKK "USD / 6.650429" "Danish kroner")
    (GBP "USD / 0.778450" "British pound")
    (HUF "USD / 275.151949" "Hungarian forint")
    (PLN "USD / 3.735878" "Polish zloty")
    (RON "USD / 4.079192" "Romanian new leu")
    (SEK "USD / 8.692170" "Swedish krona")
    (CHF "USD / 0.974258" "Swiss franc")
    (NOK "USD / 8.407669" "Norwegian kroner")
    (HRK "USD / 6.626296" "Croatian kuna")
    (RUB "USD / 56.617537" "Russian ruble")
    (TRY "USD / 3.577226" "Turkish new lira")
    (AUD "USD / 1.344744" "Australian dollar")
    (BRL "USD / 3.265821" "Brazilian real")
    (CAD "USD / 1.345459" "Canadian dollar")
    (CNY "USD / 6.856811" "Chinese yuan renminbi")
    (HKD "USD / 7.794244" "Hong Kong dollar")
    (IDR "USD / 13314.998212" "Indonesian rupiah")
    (ILS "USD / 3.572399" "Israeli new shekel")
    (INR "USD / 64.537898" "Indian rupee")
    (KRW "USD / 1121.058277" "South Korean won")
    (MXN "USD / 18.509385" "Mexican peso")
    (MYR "USD / 4.270200" "Malaysian ringgit")
    (NZD "USD / 1.411512" "New Zealand dollar")
    (PHP "USD / 49.797104" "Philippine peso")
    (SGD "USD / 1.384430" "Singaporean dollar")
    (THB "USD / 34.115123" "Thai baht")
    (ZAR "USD / 12.947533" "South African rand")))

;;; calc-currency-utils

;; calc-currency-utils-build-list
(ert-deftest calc-currency-utils-build-list-test ()
  (let ((rate-table '((USD . 1) (CAD . 1.2) (GBP . 0.8) (JPY . 100) (KRW . 1000)))
        (currency-table '((USD . "US dollar") (CAD . "Canadian dollar") (GBP . "UK pound")
                          (JPY . "Japanese yen") (KRW . "South Korean won"))))
    (should (equal (calc-currency-utils-build-list rate-table currency-table 'USD)
                   '((USD nil "US dollar")
                     (CAD "USD / 1.200000" "Canadian dollar")
                     (GBP "USD / 0.800000" "UK pound")
                     (JPY "USD / 100.000000" "Japanese yen")
                     (KRW "USD / 1000.000000" "South Korean won"))))))

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

;;; calc-currency-test.el ends here
