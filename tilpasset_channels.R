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
    today() - 14, 	# start date
    today()),		# end date
  metrics = c(  "sessions",
                "users",
                "transactions",
                "transactionRevenue"),
  dimensions = c("source","medium","sourceMedium","campaign","channelGrouping"),
  anti_sample = T) %>%
  as_tibble()

ga_data

# standard
std <- ga_data %>%
  #custom_channel_grouping() %>%
  group_by(channelGrouping) %>%
  summarise(users = sum(users)) %>% arrange(desc(users)) %>%
  mutate(grouping = "Standard")

# custom
custom <- ga_data %>%
  custom_channel_grouping() %>%
  group_by(channelGrouping) %>%
  summarise(users = sum(users),
            sessions = sum(sessions),
            transactions = sum(transactions),
            revenue = sum(transactionRevenue)) %>%
  mutate(
            session_share = sessions / sum(sessions),
            sales_share = transactions / sum(transactions),
            revenue_share = revenue / sum(revenue),
            rps = revenue / sessions,
            rpu = revenue / users) %>% 
  arrange(desc(users)) 

# Plots
std_plot <- std %>%
  ggplot(aes(x=reorder(channelGrouping,users),y=users)) +
  geom_col() +
  coord_flip() +
  ggtitle("Standard Grouping") +
  theme_minimal()

custom_plot <- custom %>%
  ggplot(aes(x=reorder(channelGrouping,users),y=users)) +
  geom_col() +
  coord_flip() +
  ggtitle("Custom Grouping") +
  theme_minimal()

gridExtra::grid.arrange(std_plot,custom_plot,ncol=2)


