Scrapetition
============

Scrapetition is a type safe yet flexible web scraper written in
Haskell. In gains flexibility by using type classes and can easily be
extented by scalpel scrapers.

Right now it has scrapers for

- Comments on the articles on (http://www.zeit.de)[http://www.zeit.de]


## Installation

Scrapetition is written in the Haskell programming language. In order
to install it, at least [Stack](https://docs.haskellstack.org/) is
needed, Haskell's build tool.

Once you have the Haskell Tool Stack installed clone this repository
and execute the following command in the root directory:

	stack build
	stack install

Running these commands will take some minutes because Stack needs to
download lots of libraries from the web and compile them.

# Usage

Scrapetition is a command line tool. Here's its help string:

<!-- BEGIN USAGE -->

<!-- END USAGE -->

So the following scrapes an URL with the scraper for comments on
www.zeit.de:

	scrapetition <URL>

## License

Licensed under either of:

- [BSD-3-Clause license](https://opensource.org/licenses/BSD-3-Clause)
- [Apache License, version 2.0](https://opensource.org/licenses/Apache-2.0)

As a user, you may use this code under either license, at your option.

