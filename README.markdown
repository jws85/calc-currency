# Introduction

This library will add currency units to
[the built-in Emacs Calc](https://www.gnu.org/software/emacs/manual/html_mono/calc.html).

# Setup

I'm planning on putting this on MELPA once it's sufficiently "solid."

At the moment, something like this will have to suffice for now:

```elisp
(add-to-list 'load-path "/path/to/calc-currency")
(require 'calc-currency)
(setq calc-start-hook #'calc-currency-load)
```

This will load `calc-currency` at startup, and then run the
`calc-currency-load` function every time Calc is started up.

# Usage

Usage should be like converting any other unit in Emacs Calc.

This isn't the place for a full Calc tutorial, but you can play
around as follows:

 - To start Calc up, type <kbd>C-x</kbd> <kbd>*</kbd> <kbd>c</kbd>.

 - <kbd>'</kbd> 25000 JPY <kbd>Enter</kbd> (the apostrophe key,
   then the text "25000 JPY", then the Enter key) will put 25000
   Japanese yen on the Calc stack.

 - <kbd>u</kbd> <kbd>c</kbd> USD <kbd>Enter</kbd> will convert
   the value on the stack to US dollars.

 - <kbd>u</kbd> <kbd>V</kbd> will display all units, including all
   currency units.

# Disclaimer

This code only supports about 30 currencies.  The exchange rates are
fetched from an [XML file provided by the European Common Bank relating
common units to the Euro](https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml).
I'd like to support more currencies, but I'd like whatever source to not
require signup, and would prefer to not have API limits, etc.

Right now I only allow for
[ISO currency codes](https://en.wikipedia.org/wiki/ISO_4217);
on down the road, I'd like to support currency signs like $, €, ¥.

This code is alpha quality.  There be dragons here!

I have used Emacs for awhile, and my Elisp is OK enough to configure
it, but actual straight-up programming in it is pretty new to me.  As
such, this code is probably completely non-idiomatic in a number of
ways.  Please let me know if there's anything that can be improved.
