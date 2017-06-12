;;; calc-currency-oxr-test.el --- Test of OpenExchangeRates backend functionality

;;; Commentary:

;;; Code:

(require 'calc-currency-oxr)

(ert-deftest calc-currency-oxr-process-rates-test-1 ()
  (let* ((json-string "{
  \"disclaimer\": \"Usage subject to terms: https://openexchangerates.org/terms\",
  \"license\": \"https://openexchangerates.org/license\",
  \"timestamp\": 1497211202,
  \"base\": \"USD\",
  \"rates\": {
    \"AUD\": 1.328371,
    \"BTC\": 0.00033395295,
    \"CAD\": 1.346348,
    \"DOGE\": 288.00997931,
    \"EUR\": 0.89236,
    \"GBP\": 0.785776,
    \"JPY\": 110.29355357,
    \"KRW\": 1124.65,
    \"USD\": 1,
    \"VEF\": 9.995002,
    \"VEF_DIPRO\": 10
  }
}")
         (json-data (json-read-from-string json-string)))
    (should (equal (calc-currency-oxr-process-rates json-data)
                   '((AUD . 1.328371) (BTC . 0.00033395295) (CAD . 1.346348)
                     (DOGE . 288.00997931) (EUR . 0.89236) (GBP . 0.785776)
                     (JPY . 110.29355357) (KRW . 1124.65) (USD . 1)
                     (VEF . 9.995002) (VEFDIPRO . 10))))))

(ert-deftest calc-currency-oxr-get-timestamp-test-1 ()
  (let* ((json-string "{
  \"disclaimer\": \"Usage subject to terms: https://openexchangerates.org/terms\",
  \"license\": \"https://openexchangerates.org/license\",
  \"timestamp\": 1497211202,
  \"base\": \"USD\",
  \"rates\": {
    \"AUD\": 1.328371,
    \"BTC\": 0.00033395295,
    \"CAD\": 1.346348,
    \"DOGE\": 288.00997931,
    \"EUR\": 0.89236,
    \"GBP\": 0.785776,
    \"JPY\": 110.29355357,
    \"KRW\": 1124.65,
    \"USD\": 1,
    \"VEF\": 9.995002,
    \"VEF_DIPRO\": 10
  }
}")
         (json-data (json-read-from-string json-string)))
    (should (equal (calc-currency-oxr-get-timestamp json-data)
                   1497211202))))

(provide 'calc-currency-oxr-test)

;;; calc-currency-oxr-test.el ends here
