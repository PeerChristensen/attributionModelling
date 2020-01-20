
library(googleAnalyticsR)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(ChannelAttribution)
# authorize the connection with Google Analytics servers
ga_auth(email = "hr.pchristensen@gmail.com")


# options(googleAuthR.client_id = id)
# options(googleAuthR.client_secret = secret)
# options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")
# options(httr_oob_default=TRUE)


## get your accounts
account_list <- ga_account_list()

## account_list will have a column called "viewId"
account_list$viewId

## View account_list and pick the viewId you want to extract data from. 
ga_id <- 483103 
ga_id <- 209659966


ga_data <- google_analytics(viewId = ga_id,
                            date_range = c(
                              today() , 	# start date
                              today()),		# end date
  metrics = c("users"),
  dimensions = c("source","medium","sourceMedium","channelGrouping","campaign"))

ga_data


df <- google_analytics(ga_id,
                      date_range = c(as.character(today()-3), as.character(today())),
                      metrics = c("totalConversions","totalConversionvalue"),
                      dimensions = c("channelGrouping"))


# transaction filter
trans_filter <- "mcf:conversionType==Transaction" # the conversion you're interested in
visit_filter <- "mcf:conversionGoalNumber==012"   # "visit" conversion

get_data <- function(vid, from, to, filters = "",
                     dim = "sourceMediumPath", max = 5000) {
  df <- google_analytics_3(id = vid, 
                           start = from, end = to, 
                           metrics = c("totalConversions"), 
                           dimensions = dim,
                           filters = filters,
                           type="mcf",
                           max_results = max)
  # clean up and set class
  df[,1] <- gsub(" / ", "/", df[,1])              # remove spacing
  df[,1] <- gsub(":?(NA|CLICK|NA):?", "", df[,1]) # remove CLICK and NA
  df[,2] <- as.numeric(df[,2])                    # conversion column is character :-/
  
  # return the dataframe
  df
}

from <- "2019-12-01"
to   <- today()
dim = "sourceMediumPath"
dim = "basicChannelGroupingPath"
# get transactions
transactions <- get_data(vid=ga_id, from=from, to=to, dim=dim, filters=trans_filter)
colnames(transactions) <- c("path", "transactions")
# get visits (remember: this view has a goal where a visit = a conversion)
visits       <- get_data(vid=ga_id, from=from, to=to, dim=dim, filters=visit_filter)
colnames(visits) <- c("path", "visits")
# merge dataframes, based on path
alldata <- merge.data.frame(visits, transactions, by = "path", all=T)
# not all visit paths have conversions. Change those NA's to zero.
alldata[is.na(alldata$transactions), "transactions"] <- 0
# calculate conversion rate
alldata$rate <- alldata$transactions / alldata$visits
# null = visits without transaction
alldata$null  <- alldata$visits - alldata$transactions
# run the markov model
mm <- markov_model(alldata, var_path = "path",
                   var_conv = "rate",
                   #var_value = "value", #use this if you have conversion values
                   var_null = "null",
                   order=3, nsim=NULL, max_step=NULL, out_more=TRUE)
# run the heuristic model
hm <- heuristic_models(alldata, var_path = "path",
                       #var_value = "value",
                       var_conv = "rate")
# merge mm + hm, and voila.. your modeled acquisition sources
modeled <- merge.data.frame(hm, mm, all=T, by="channel_name")
# and View it (Rstudio)
View(alldata) # the conversion rate of each path
View(modeled) # the attribution table per channel


R <- merge(hm, mm, by='channel_name')

# Select only relevant columns
R1 <- R[, (colnames(R) %in% c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion'))]

# Transforms the dataset into a data frame that ggplot2 can use to plot the outcomes
R1 <- reshape2::melt(R, id='channel_name')
R1 <- R1 %>% filter(value>100)
ggplot(R1, aes(channel_name, value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') +
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20),
        axis.text.x = element_text(angle=45,hjust=1)) +
  ylab("")


df_res1 <- mm$result

# extracting a transition matrix
df_trans1 <- mm$transition_matrix
df_trans1 <- dcast(df_trans1, channel_from ~ channel_to, value.var = 'transition_probability')

### plotting the Markov graph ###
df_trans <- mm$transition_matrix

# adding dummies in order to plot the graph
df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       transition_probability = c(0, 1, 1))
df_trans <- rbind(df_trans, df_dummy)

# ordering channels
df_trans$channel_from <- factor(df_trans$channel_from,
                                levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans$channel_to <- factor(df_trans$channel_to,
                              levels = c('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
df_trans <- dcast(df_trans, channel_from ~ channel_to, value.var = 'transition_probability')

# creating the markovchain object
trans_matrix <- matrix(data = as.matrix(df_trans[, -1]),
                       nrow = nrow(df_trans[, -1]), ncol = ncol(df_trans[, -1]),
                       dimnames = list(c(as.character(df_trans[, 1])), c(colnames(df_trans[, -1]))))
trans_matrix[is.na(trans_matrix)] <- 0
trans_matrix=trans_matrix[1:3,1:3]
trans_matrix1 <- new("markovchain", transitionMatrix = trans_matrix)

# plotting the graph
plot(trans_matrix1, edge.arrow.size = 0.35)
