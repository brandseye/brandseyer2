# brandseyer2 0.0.1.9011

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
