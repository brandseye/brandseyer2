# This is a script for creating the various data sets
# that we either provide to users for tutorial purposes,
# or that we use internally for testing.

test01aa_data <- list(
  storage = "V4",
  name = "Test 1",
  code = "TEST01AA",
  timezone = "Africa/Johannesburg",
  clientService = list(
    email = "noreply@brandseye.com",
    firstName = "Your",
    lastName = "Manager"
  ),
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
  ),
  brands = list(
    list(
      id = 1,
      name = "Your Brand Here",
      tier = "ESSENTIALS",
      crowdSamplePercentage = 0,
      crowdTopicPercentage = 0,
      description = "Your Brand is the Best of Brands™",
      phrases = list(
        list(
          id = 1,
          q = "brand",
          deleted = TRUE
        ),
        list(
          id = 2,
          q = "your brand"
        )
      )
    ),
    list(
      id = 2,
      name = "Sibling Brand",
      mentionFilter = "location is 'ZA'",
      tier = "TOPICS",
      crowdSamplePercentage = 0.8,
      crowdTopicPercentage = 0.5,
      description = "Where your brand goes, this brand follows",
      schema = "b00000001",
      topicTreeId = 100,
      phrases = list(
        list(
          id = 3,
          q = "sibling"
        ),
        list(
          id = 4,
          q = "oh wow",
          inactive = TRUE
        )
      ),
      children = list(
        list(
          id = 3,
          name = "Niece Brand",
          phrases = list(
            list(
              id = 5,
              q = "niece"
            )
          )
        )
      )
    )
  )
)
test01aa <- brandseyer2:::create_account(test01aa_data)

test02aa_data <- list(
  storage = "V2",
  name = "Test 2 — Best Account",
  code = "TEST02AA",
  timezone = "UTC",
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
  ),
  brands = list(
    list(
      id = 100,
      name = "The Best of Brands",
      tier = "TOPICS",
      crowdSamplePercentage = 0.8,
      crowdTopicPercentage = 1,
      description = "Who the Brand‽",
      topicTreeId = 1001,
      phrases = list(
        list(
          id = 101,
          q = "brander"
        ),
        list(
          id = 102,
          q = "branded"
        )
      )
    )
  )
)

test02aa <- brandseyer2:::create_account(test02aa_data)

# -------------------------------------------------------------
# Build mention data

test01aa_mentions <- list(
  list(id = "one",
       sentiment = as.integer(0),
       brands = list(
         list(id = as.integer(1), name = "one"),
         list(id = as.integer(2), name = "two")
       ),
       tags = list(
         list(id = as.integer(1), name = "one")
       ),
       mediaLinks = list(
         list(url = "http://bob.com", mimeType = "application/json")
       ),
       socialNetwork = list(
         id = "TWITTER",
         label = "Twitter"
       ))
)


usethis::use_data(test02aa, test01aa,
                  test01aa_mentions,
                  internal = TRUE, overwrite = TRUE)
