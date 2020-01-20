# Tilpasset channel grouping regler
# Peer Christensen
# januar 2020

# Vi kan ikke hente google analytics data med vores brugerdefinerede kanalgruppering
# dette script anvender de korrekte regler.


library(googleAnalyticsR)
library(tidyverse)
library(lubridate)

ga_auth(email = "hr.pchristensen@gmail.com")

ga_id <- 483103 

source("channelGrouping_function.r")

ga_data <- google_analytics(
  viewId = ga_id,
  date_range = c(
    today() - 2, 	# start date
    today()),		# end date
  metrics = c("users"),
  dimensions = c("source","medium","sourceMedium","campaign","channelGrouping")) %>%
  as_tibble()

ga_data

g <- ga_data %>%
  custom_channel_grouping() %>%
  group_by(channelGrouping) %>%
  summarise(users = sum(users)) %>% arrange(desc(users))

