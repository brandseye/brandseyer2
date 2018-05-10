# This is a script for creating the various data sets
# that we either provide to users for tutorial purposes,
# or that we use internally for testing.

test01aa_data <- v3_data <- list(
  storage = "V4",
  name = "Test 1",
  code = "TEST01AA",
  tags = list(
    list(id = 1,
         name = "tag1",
         namespace = "tag"),
    list(id = 2,
         name = "tag2",
         namespace = "tag",
         description = "Deleted",
         deleted = TRUE),
    list(id = 10,
         name = "Parent topic",
         namespace = "topic",
         description = "Has children",
         children = list(11, 12)),
    list(id = 11,
         name = "Child topic 1",
         namespace = "topic",
         description = "I am a child"),
    list(id = 12,
         name = "Child topic 2",
         namespace = "topic",
         description = "I am also a child")
  )
)

test01aa <- brandseyer2:::create_account(test01aa_data)

usethis::use_data(test01aa, internal = TRUE, overwrite = TRUE)
