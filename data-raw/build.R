# This is a script for creating the various data sets
# that we either provide to users for tutorial purposes,
# or that we use internally for testing.

test01aa_data <- list(
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


test02aa_data <- list(
  storage = "V2",
  name = "Test 2 — Best Account",
  code = "TEST02AA",
  tags = list(
    list(id = 101,
         name = "tag1",
         namespace = "tag"),
    list(id = 102,
         name = "tag2",
         namespace = "tag",
         description = "Deleted",
         deleted = TRUE),
    list(id = 1010,
         name = "Parent topic",
         namespace = "topic",
         description = "Has children",
         children = list(1011, 1012)),
    list(id = 1011,
         name = "Child topic 1",
         namespace = "topic",
         description = "I am a child"),
    list(id = 1012,
         name = "Child topic 2",
         namespace = "topic",
         description = "I am also a child")
  )
)

test02aa <- brandseyer2:::create_account(test02aa_data)



usethis::use_data(test02aa, test01aa, internal = TRUE, overwrite = TRUE)