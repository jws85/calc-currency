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

## Basic Configuration

All configuration can be done with Customize if you prefer: <kbd>M-x</kbd>
customize-group <kbd>Return</kbd> calc-currency <kbd>Return</kbd>.

`calc-currency-exchange-rates-file` allows you to change the
default location for the exchange rate table file.  By default,
this file is placed at `~/.emacs.d/calc-currency-rates.el`.

`calc-currency-update-interval` is the number of days before
`calc-currency` will fetch new rates.  This is 5 by default.

## Using Open Exchange Rates

Out of the box, calc-currency will download exchange rates from an
[XML file provided by the European Common Bank relating about 30
common currencies to the Euro](https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml).
You may take a look at that XML file and see if those are
sufficient for your needs.  If they aren't, you can use [Open
Exchange Rates](https://openexchangerates.org/) as a backend
instead.  This will get you about 180 currencies, and you can
optionally fetch rates for several cryptocurrencies as well.

In order to do this, you'll need to sign up and get an app ID.
This will allow you 1000 updates a month for free; if you need
it more often, you'll need to pay (and, honestly, you probably
CAN pay if you need that level of granularity ^_~).

Once you do that, add the following to your elisp above:

```elisp
(require 'calc-currency-oxr)
(setq calc-currency-backend-function #'calc-currency-oxr-list
      calc-currency-oxr-app-id "Put your App ID here")
```

Remember to not commit your app ID to a public repository.

You may additionally fetch alternative rates and cryptocurrency
rates by setting `calc-currency-show-alternative` to `t`.

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

Right now I only allow for
[ISO currency codes](https://en.wikipedia.org/wiki/ISO_4217);
on down the road, I'd like to support currency signs like $, €, ¥.

In at least one place in this code, I use `/tmp` as a temporary
scratch place, and thus has a reliance on a *nix based environment.
That should be fixed at some point too.

# Licensing

This software is licensed under the GNU GPL v3.
