;;; calc-currency-ecb-test.el --- Test of European Common Bank backend functionality

;;; Commentary:

;;; Code:

(require 'calc-currency-ecb)

(ert-deftest calc-currency-ecb-process-rates-test-1 ()
  (let ((xml-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gesmes:Envelope xmlns:gesmes=\"http://www.gesmes.org/xml/2002-08-01\" xmlns=\"http://www.ecb.int/vocabulary/2002-08-01/eurofxref\">
	<gesmes:subject>Reference rates</gesmes:subject>
	<gesmes:Sender>
		<gesmes:name>European Central Bank</gesmes:name>
	</gesmes:Sender>
	<Cube>
		<Cube time='2017-05-29'>
			<Cube currency='USD' rate='1.1188'/>
			<Cube currency='JPY' rate='124.57'/>
			<Cube currency='GBP' rate='0.87093'/>
			<Cube currency='CAD' rate='1.5053'/>
			<Cube currency='KRW' rate='1254.24'/>
		</Cube>
	</Cube>
</gesmes:Envelope>"))
    (should (equal (calc-currency-ecb-process-rates xml-string)
                   '((EUR . 1.0) (USD . 1.1188) (JPY . 124.57)
                     (GBP . 0.87093) (CAD . 1.5053) (KRW . 1254.24))))))

(ert-deftest calc-currency-ecb-get-timestamp-test-1 ()
  (let ((xml-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<gesmes:Envelope xmlns:gesmes=\"http://www.gesmes.org/xml/2002-08-01\" xmlns=\"http://www.ecb.int/vocabulary/2002-08-01/eurofxref\">
	<gesmes:subject>Reference rates</gesmes:subject>
	<gesmes:Sender>
		<gesmes:name>European Central Bank</gesmes:name>
	</gesmes:Sender>
	<Cube>
		<Cube time='2017-05-29'>
			<Cube currency='USD' rate='1.1188'/>
			<Cube currency='JPY' rate='124.57'/>
			<Cube currency='GBP' rate='0.87093'/>
			<Cube currency='CAD' rate='1.5053'/>
			<Cube currency='KRW' rate='1254.24'/>
		</Cube>
	</Cube>
</gesmes:Envelope>"))
    (should (equal (calc-currency-ecb-get-timestamp xml-string)
                   1496070000.0))))

(provide 'calc-currency-ecb-test)

;;; calc-currency-ecb-test.el ends here
