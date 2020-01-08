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

```
scrapetition - scrape comments (discussions) from social media web sites.

Usage
-----

scrapetition ((-u|--url URL) | (-d|--db-not-seen DOMAIN))
                  [-f|--follow-links] [-x|--cross-domain] [-a|--visit-again]
                  [-l|--lifo] [--wwwZeitDe-comments] ([-s|--sqlite DATABASE] |
                  [-p|--postgresql CONNECTION] | [-r|--raw])
                  [--logfile LOGFILE] [--items-table ITEMTABLE]
                  [--users-table USERTABLE] [--voting-table VOTINGTABLE]
scrapetition scrapes discussions from social media web sites. It starts with a
URL given by the user and tries to scrape all comments on this URL and
subsequent URL.

Available options:
-h,--help                Show this help text
-u,--url URL             A URL to start with.
-d,--db-not-seen DOMAIN  Visit the URLs from the database, that were no not
                         visited yet. A domain must be given to which the
                         crawling of URLs is restricted. This restriction can
                         be overridden with the --cross-domain option.
-f,--follow-links        Follow all links found on a page. Use this in
                         combination with --cross-domain for cross-domain
                         scraping.
-x,--cross-domain        Follow links pointing outside of the domain of the
                         start URL.
-a,--visit-again         Visit URLs again. Without this option, URLs will not
                         be visited and scraped, if they are already in the
                         database and marked as visited.
-l,--lifo                Last in, first out handling of URLs: With this switch
                         the last found URL is scraped first. By default, the
                         first found URL is scraped first.
--wwwZeitDe-comments     Scraper for discussion on articles at
                         http://www.zeit.de. This is the default scraper --
                         and the only one so far.
-s,--sqlite DATABASE     Output to SQLite3 database. This is the default
                         output method. (default: "data.db")
-p,--postgresql CONNECTION
                         Output to PostgreSQL database given by the connection
                         string. See
                         http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT
                         for info about the connection string. Example: '-p
                         "host=localhost dbname=scrapetition_test user=me
                         password=mine"'
-r,--raw                 Output raw data.
--logfile LOGFILE        Specify a file for logging messages. By default,
                         messages are logged to stderr.
--items-table ITEMTABLE  Table name for scraped items. (default: "comments")
--users-table USERTABLE  Table name for scraped users. (default: "users")
--voting-table VOTINGTABLE
                         Table name for voting by users about
                         items. (default: "comment_voting")

```

<!-- END USAGE -->

So the following scrapes an URL with the scraper for comments on
www.zeit.de:

	scrapetition <URL>

## License

Licensed under either of:

- [BSD-3-Clause license](https://opensource.org/licenses/BSD-3-Clause)
- [Apache License, version 2.0](https://opensource.org/licenses/Apache-2.0)

As a user, you may use this code under either license, at your option.

