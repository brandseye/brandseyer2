# brandseyer2 0.0.1.9029

* Fixed a bug preventing `count_mentions()` from grouping by month.

# brandseyer2 0.0.1.9028

`rules()` now returns the rule's active flag.

# brandseyer2 0.0.1.9027

* `account_list()` can now find accounts for a particular client.

# brandseyer2 0.0.1.9026

* The data returned by `tags()` now shows whether a tag is used for classification jobs or not.

# brandseyer2 0.0.1.9025

* `brands()` and `root_brands()` now provide brand descriptions.

# brandseyer2 0.0.1.9024

* `update_mentions()` now reports an error when attempting to update V3 accounts.

# brandseyer2 0.0.1.9023

- `profiles()` added, which lets you pull online profiles for an account.

# brandseyer2 0.0.1.9022

*  `account_list()` now shows client code.
*  Added function `account_client_code()` to get client code from account object.

# brandseyer2 0.0.1.9021

* `sections()` now shows a progress bar.

# brandseyer2 0.0.1.9020

* `topic_tree()` returns more exact data.
* Added a series of functions to examine the data models used in the filter language.

# brandseyer2 0.0.1.9019

* Added `compare_mentions_raw()` to dynamically add sub-filters to queries.

# brandseyer2 0.0.1.9018

* `rules()` have better support for `tidyr::unnest()`. 

# brandseyer2 0.0.1.9017

* The table returned by `logs()` has renamed `repeatCount` to `timesDone` for clarity.
* `logs_retrosent()` now includes `requested` and `sent` amounts.
* `rules()` is available to list rules in an account.

# brandseyer2 0.0.1.9016

* Renamed `with_order()` to `with_mention_order()` to avoid conflicts with dplyr.
* Renamed `with_fields()` to `with_mention_fields()` to be consistent with the `with_mention_order()`
  rename.

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
