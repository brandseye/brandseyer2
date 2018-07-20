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
