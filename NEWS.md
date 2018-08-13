# brandseyer2 0.0.1.9016

# brandseyer2 0.0.1.9015

* `account()` is now fairly flexible with the formatting of account codes.

# brandseyer2 0.0.1.9014

* Now uses POST to fetch mentions from `mentions()`

# brandseyer2 0.0.1.9013

* Filtering of accounts with no brands now occurs at the `count_mentions()` level.

# brandseyer2 0.0.1.9012

* Now has a `with_account()` verb for the filter language.
* Added a way to nicely see all brands in a query: `get_query_brands()`.
* Added a way to nicely see all accounts in a query: `get_query_accounts()`.
* Account information is cached for a short period of time.

# brandseyer2 0.0.1.9011

* The filter language is available.
* `logs()` now shows a progress bar in interactive settings.
* `logs_retrosent()` now makes account retrosend data available.
* Non brandseye staff won't get content for tweets.

# brandseyer2 0.0.1.9010

* Added `with_tag_parents()` to provide parent information to tags.
* `account()` can now load account information from a data.frame.
* `account()` now shows a progress bar when interactive. 
* `account()` can now fetch accounts from a data.frame. 
* Much debugging of how `filter_mentions()` handles multiple 
  accounts, some with no brands.

# brandseyer2 0.0.1.9009

* Now greets people when loaded interactively.

# brandseyer2 0.0.1.9008

* Updated various format functions to provide colour. 
* Began work on filter language
* `account_timezone()`, `account_code()` and `account_name()` are now all vectorised.

# brandseyer2 0.0.1.9007

* Added the function `users()` to read a list of users from an account.

# brandseyer2 0.0.1.9006

* bug fix: `mentions()` no longer raises a condition if `id` isn't selected.
* bug fix: `root_brands()`, when operating on a list of accounts, now includes the account code.

# brandseyer2 0.0.1.9005

* No longer supports V3 accounts.
* More testing for `mentions()`.

# brandseyer2 0.0.1.9004

* Added `logs()`, which returns a tibble of logs from an account.

# brandseyer2 0.0.1.9003

* Added new `topic_trees()` method to find topic trees in the account.
* Bug fix: `brands()` on lists now obeys the `short` parameter.
* `account()` can now be called with multiple account codes, like so: 
  ``` account("TEST01AA", "TEST02AA") ```
* Bug fix: `mentions()` no longer throws an error when returning no mentions.

# brandseyer2 0.0.1.9002

* `account_list()` can now include inactive accounts.

# brandseyer2 0.0.1.9001

* Added a `NEWS.md` file to track changes to the package.
* Bumped the development version for the first time.
* Clarified documentation on `dashboards()`.
