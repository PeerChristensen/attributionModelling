
library(googleAnalyticsR)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(ChannelAttribution)

ga_auth(email = "hr.pchristensen@gmail.com")

viewId <- 483103 
#viewId <- 209659966


df <- google_analytics_3(id = viewId, 
                         start = today()-30, end = today(), 
                         metrics = c("totalConversions","totalConversionValue"), 
                         dimensions = c("basicChannelGroupingPath"),
                         type="mcf")
df[,1] <- gsub(":?(NA|CLICK|NA):?", "", df[,1]) # remove CLICK and NA
df[,2] <- as.numeric(df[,2])  
df[,3] <- as.numeric(df[,3])  

h_model <- heuristic_models(df,'basicChannelGroupingPath','totalConversions',var_value = 'totalConversionValue')
m_model <- markov_model(df,'basicChannelGroupingPath','totalConversions',var_value = 'totalConversionValue')

columns <- c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion')

models <- inner_join(h_model,m_model, by = "channel_name")

models <- models[ ,(names(models) %in% columns)] %>%
  pivot_longer(-channel_name) %>%
  mutate(name = if_else(name=="total_conversion","markov",name))

p1 <- ggplot(models, aes(channel_name, value, fill = name)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Conversions',
          subtitle = "number of conversions attributed to each channel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1)) 


# conversion value

columns2 <- c('channel_name', 'first_touch_value', 'last_touch_value', 'linear_touch_value', 'total_conversion_value')

models2 <- inner_join(h_model,m_model, by = "channel_name")

models2 <- models2[ ,(names(models2) %in% columns2)] %>%
  pivot_longer(-channel_name) %>%
  mutate(name = if_else(name=="total_conversion_value","markov",name))

p2 <- ggplot(models2, aes(channel_name, value, fill = name)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('Conversion value',subtitle = "monetary value attributed to each channel from a conversion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1))

gridExtra::grid.arrange(p1,p2,ncol=2)


