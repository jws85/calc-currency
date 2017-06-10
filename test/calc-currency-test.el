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

(ert-deftest calc-currency-read-file-test-1 ()
  (cl-letf (((symbol-function 'file-readable-p) (lambda (file) t))
            ((symbol-function 'insert-file-contents) (lambda (file) *conversion-file*)))
    (should (equal (calc-currency-read-file) *conversion-table*))))

  (cl-letf (((symbol-function 'file-readable-p) (lambda (file) t))
            ((symbol-function 'insert-file-contents) (lambda (file) *conversion-file*)))
    (calc-currency-read-file))

;;; calc-currency-utils

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

;;; calc-currency-utils-file-age
;(ert-deftest calc-currency-utils-file-age-1 ()
;  (cl-flet ((calc-currency-utils-time-modified (file)
;             (time-subtract (current-time) (days-to-time 5))))
;    (should (= (calc-currency-utils-file-age "doesntmatter") 5))))

;;; calc-currency-test.el ends here
