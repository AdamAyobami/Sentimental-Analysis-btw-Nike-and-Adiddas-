# Load the packages
#
#
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
library(httpuv)
#
#
### *** Searching tweets with a specific hashtag
### ***
#
#
## Search for 3000 tweets using the #NIKE hashtag
## The function search_tweets returns a data frame where each observation (row) is a 
## different tweet
#
rt <- search_tweets(
  q = "#NIKE", n = 3000
)
#
# Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes.
# To request more than that, simply set retryonratelimit = TRUE and rtweet will wait 
# for rate limit resets for you:
#
rt <- search_tweets(q = "#NIKE", n =3000, retryonratelimit = TRUE)
#
#
#
## preview tweets data
rt
#
# Look at the structure
#
dim(rt)
names(rt)
rt$text
rt$location
#
## preview users data - this is a subset of the information retrieved using search_tweets 
ud <- users_data(rt)
ud
#
dim(ud)
names(ud)
#
################################################
#
###
### plot time series with ts_plot() (if ggplot2 is installed)
###
#
# ts_plot() provides a quick visual of the frequency of tweets. By default, ts_plot() 
# will try to aggregate time by the day. It’d also be possible to aggregate by the 
# minute, i.e., by = "mins", or by some value of seconds, e.g.,by = "15 secs"
#
# Quickly visualize frequency of tweets over time using ts_plot() in 1 minute interval
ts_plot(rt, "mins")
#
## plot frequency in 1 second interval
ts_plot(rt, "secs") 
#
## plot frequency in 10 minutes interval
ts_plot(rt, "10 mins")
#
#
#.rs.restartR()
# Often these plots resemble a frowny face with the first and last points appearing 
# significantly lower than the rest. This is because the first and last intervals of time 
# are artificially shrunken by connection and disconnection processes. To remedy this, 
# users can specify trim = 1 to tell R to drop the first and last observation for each time 
# series. This usually yields a much more attractive looking plot.
#
ts_plot(rt, "10 mins", trim = 1)
#
#
## plot time series of tweets in 10 minutes interval 
## improving the plot with some ggplot2 features
ts_plot(rt, "10 mins") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of #NIKE Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using 10-minutes intervals",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )
#
#
## Now group_by retweets and non-retweets
#
## tabulate retweets vs non-retweets
rt %>%
  group_by(is_retweet) %>%
  summarize(n =n()) 
#
#
## plot retweets vs non-retweets
rt %>%
  group_by(is_retweet) %>%
  ts_plot("15 mins", lwd = 1.5) + # Specify time interval and line width
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of #NIKE Twitter statuses",
        subtitle = "Twitter status (tweet) counts aggregated using 15-minutes intervals",
        caption = "Source: Data collected from Twitter's REST API via rtweet",
        colour = "Retweet") +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20))  
#
#
##########################################
#
###
### Information about users
###
#
## Next, let us figure out who is tweeting about NIKE using the #NIKE hashtag.
#
# view column with screen names - top 6
head(rt$screen_name)
#
# get a list of unique usernames
unique(rt$screen_name)
#
#
## Let us learn a bit more about these people tweeting about NIKE. First, where are they from?
#
# how many locations are represented?
length(unique(rt$location))
#
#
# bar plot of the locations
#
rt %>%
  ggplot(aes(location)) +
  geom_bar() + coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users using #NIKE are from")
# Huge number of locations!
# This plot can be improved!
#
#
# Let's sort by count and just plot the top locations. To do this you use head(). 
# head(10) will return the top 10 locations. 
#
#
rt %>%
  count(location, sort = TRUE) %>% # count the frequency for each location
  mutate(location = reorder(location, n)) %>% # make sure that locations are ordered according to frequency
  head(10) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col(fill = "blue") + # This is the same as geom_bar(stat = "identity") + 
  coord_flip() + # flip x and y axes
  labs(x = "Top Locations",
       y = "Frequency",
       title = "Where Twitter users using #NIKE are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
# It looks like we have some blank values.
# Let's transform them in NAs and then remove them with na.omit()
#
rt$location[rt$location==""] <- NA
#
rt %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>% # remove NAs
  head(10) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Top Locations",
       y = "Frequency",
       title = "Where Twitter users using #NIKE are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
# The plot can be improved by merging same locations
#
#
# Let's check the names of the top locations
rt %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  head(10)
#
# To join similar locations we can use the function recode form the dplyr package 
rt_NIKE <- rt %>% mutate(location_rec = 
                                   recode(location, "London, England" = "London", "London, UK" = "London", 
                                          "UK" = "United Kingdom", "u.k." = "United Kingdom",
                                          "United Kingdom, EU" = "United Kingdom",
                                          "England, United Kingdom" = "England",
                                          "united kingdom" = "United Kingdom",
                                          "EU" = "European Union",
                                          "London, United Kingdom" = "London"
                                   ))
#
# check the names of the top locations again
rt_NIKE %>%
  count(location_rec, sort = TRUE) %>%
  mutate(location_rec = reorder(location_rec,n)) %>%
  na.omit() %>%
  head(10)
#
#
# plot top locations again
rt_NIKE %>%
  count(location_rec, sort = TRUE) %>%
  mutate(location_rec = reorder(location_rec,n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = location_rec,y = n)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Top Locations",
       y = "Frequency",
       title = "Where Twitter users using #NIKE are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
#
#
#
##########################################
###
### Information on the device used to tweet about #NIKE
###
#
## Tweet Source
#
# Identify the top 10 devices used to tweet about #NIKE
# and create a bar plot
rt %>% 
  group_by(source) %>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total)) %>% 
  head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) +
  geom_col() +
  coord_flip() + 
  labs(title="Top Tweet Sources for users tweeting about #NIKE", 
       x="Device", 
       caption = "Source: Data collected from Twitter's REST API via rtweet") 
#
#
##################################################################################
#
### ***
### *** Search User Profiles containing a specific hashtag in their profile
### ***
#
#
### Search User Profiles with #NIKE in their profiles
#
#
# search for up to 3000 users using the hashtag #NIKE in their profiles
users <- search_users("#NIKE",
                      n = 3000
)
#
#
# produce a plot showing the user profiles
#
users %>% 
  group_by(screen_name) %>% 
  arrange(desc(followers_count)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(screen_name, followers_count), followers_count, fill = name)) + 
  geom_col() + 
  coord_flip() +
  labs(title='Top Users with #NIKE On Their Profile', 
       x="Users", 
       caption = "Source: Data collected from Twitter's REST API via rtweet") 
#
#
#
#
##################################################################################
#
### ***
### *** Get the statuses posted to the timelines of specified Twitter users.
### ***
#
#
### Compare account activity for some important celebrity figures
#
#
## lookup specific users: 
celebrities <- lookup_users(c("Cristiano", "Kanyewest", "KingJames"))
#
## extract most recent tweet from the famous tweeters
celebrities %>% 
  group_by(screen_name) %>%
  select(screen_name, text)
#
#
#
#
## Get the most recent 500 tweets from cristiano ronaldo, kanye west and lebron james
#
tmls <- get_timeline(
  c("Cristiano", "kanyewest","KingJames"),
  n = 1000
)
#
# See the most recent tweets posted by celebrity figures
tmls %>% 
  arrange(desc(created_at)) %>% 
  group_by(screen_name) %>%
  select(created_at, screen_name, text) 
#
#
## Examine all twitter activity using weekly intervals
## plot the frequency of tweets for each user over time
tmls %>%
  group_by(screen_name) %>%
  ts_plot("weeks", lwd = 1) +
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of Twitter statuses posted by celibrity",
        subtitle = "Tweet counts aggregated every week",
        colour = "Celebrity") +
  scale_colour_manual(values = c("Cristiano" = "red", "Kanyewest" = "blue", "KingJames" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20),
        legend.position = "bottom")  
#
#
## group by screen name and is_retweet
## plot the frequency of tweets for each user over time
tmls %>%
  group_by(screen_name, is_retweet) %>%
  ts_plot("2 weeks", lwd = 1) +
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of Twitter statuses posted by celebrity",
        subtitle = "Tweet counts aggregated every 2 weeks",
        caption = "Source: Data collected from Twitter's REST API via rtweet",
        colour = "Celebrity", 
        linetype = "Retweet") +
  scale_colour_manual(values = c("Cristiano" = "red", "Kanyewest" = "blue", "KingJames" = "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20)) 
#
#
#
#
##################################################################################
#
### ***
### *** Get trending topics in specific locations
### ***
#
#
## Discover what is currently trending in plymouth
Plymouth_trends <- get_trends("plymouth")
#
#
# Look at the trending topics in London
Plymouth_trends$trend
#
#
# Produce a bar chart of top trending topics according to tweet volume
Plymouth_trends %>% 
  arrange(desc(tweet_volume)) %>% 
  head(3) %>% 
  ggplot(aes(reorder(trend, tweet_volume), tweet_volume)) + 
  geom_col(fill = "lightblue", color = "blue") + 
  coord_flip() +
  labs(title="Top Trending Topics in plymouth", 
       x="Trending topics",
       y="Tweet volume",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20)) 
#
#
#
#
##################################################################################
#
### ***
### *** Stream tweets 
### ***
#
#
# In addition to accessing Twitter REST API (e.g., search_tweets, get_timeline), 
# rtweet makes it possible to capture live streams of Twitter data using the stream_tweets() 
# function. By default, stream_tweets will stream for 30 seconds and return a random sample 
# of tweets. To modify the default settings, stream_tweets accepts several parameters, 
# including q (query used to filter tweets), timeout (duration or time of stream), 
# and file_name (path name for saving raw json data).
#
#
# The following code allows you to stream tweets written in english and contaning 
# the hashtag #NIKE, for 60 seconds. 
# The data are saved in your directory and can be loaded at a later time
# This is useful if you wish to store large amount of tweets in files using continuous streams
#
#
# Stream tweets written in english, containing #NIKE, for 60 seconds 
# save the data in the file stream_NIKE.json
# json means JavaScript Object Notation (JSON) format
stream_tweets(
  q = "#NIKE",
  timeout = 60, # stream for 60 seconds
  file_name = "stream_NIKE.json", # file where the data are saved
  lang = "en", # tweets written in english
  parse = FALSE
)
#
## read in the data as a data frame
stream_NIKE_tweets <- parse_stream("stream_NIKE.json")
# 
## look at the data
stream_NIKE_tweets
# 
stream_NIKE_tweets$text
# 
#
# 
##################################################################################
#
### ***
### *** Searching tweets containing a specific word
### ***
#
#
## search for 1000 tweets containing the word football
rt_football <- search_tweets(
  "football", n = 1000
)
#
#
## As before, these tweets can be saved for future use, for example, as a json file
rt_football %>% toJSON() %>% write_lines("rt_football.json")
#
#
# Read in the data
rt_football_tweets <- stream_in(file("rt_football.json"))
#
#
#
## use ts_plot to examine all twitter activity using 1 minute intervals
ts_plot(rt_football_tweets, "mins") +
  theme_minimal() + # white background 
  theme(plot.title = element_text(face = "bold")) + # boldface title
  labs(
    x = NULL, y = NULL, # no labels on the axes
    title = "Frequency of tweets about football in the last hour",
    subtitle = "Tweet counts aggregated every 2 minute",
    caption = "Source: Data collected from Twitter's REST API via rtweet"
  )
#
#
## group by is_retweet
rt_football_tweets %>%
  group_by(is_retweet) %>%
  ts_plot("2 mins", lwd = 1.5) +
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of tweets about football in the last hour",
        subtitle = "Tweet counts aggregated every 2 minute",
        caption = "Source: Data collected from Twitter's REST API via rtweet",
        colour = "Retweet") +
  scale_colour_manual(values = c("FALSE" = "orange", "TRUE" = "darkgreen")) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        title = element_text(size = 20))  
#
#
#################################################
#
###
### Create Maps of Social Media Tweet Locations in R
###
#
#
## perform search for lots of tweets
## making multiple independent search queries
#
## let's search for 1000 tweets containing the words: NIKE, ADIDAS
rt_fet <- Map(  # Map applies search_tweets to each element: "nikeshoes", "adidasshoes", "shoes"
  "search_tweets",
  c("nike_shoes", "adidas_shoes", "shoes"),
  n = 1000
)
sa#
## bind the results by row and transform in a data frame
ds_fet <- do_call_rbind(rt_fet) %>% as.data.frame()
#
#
#
## create variables indicating latitude and longitude using all available 
## tweet and profile geo-location data
ds_fet <- lat_lng(ds_fet)
#
# create new data frame with just the tweet texts, usernames and location data
ds_fet_s <- data.frame(date_time = ds_fet$created_at,
                       username = ds_fet$screen_name,
                       tweet_text = ds_fet$text,
                       long = ds_fet$lng,
                       lat = ds_fet$lat)
#
#
#
# create basemap of the globe
# the theme_map() function cleans up the look of your map.
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()
world_basemap
#
#
# Next, look closely at your data. 
# Notice that some of the location information contains NA values. 
# Let us remove NA values and then plot the data as points using ggplot().
#
head(ds_fet_s)
#
# remove na values
ds_fet_locations <- ds_fet_s %>%
  na.omit()
head(ds_fet_locations)
#
#
# Plot the data modifying the basemap of the globe with ggplot2 features
#
world_basemap +
  geom_point(data = ds_fet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  labs(title = "Locations of tweets on nike_shoes, adidas_shoes, shoes")
#
#
#
### Improve the plot to deal with overlapping points
# 
# round latitude and longitude and group close tweets
ds_fet_locations_grp <- ds_fet_locations %>%
  mutate(long_round = round(long, 2),
         lat_round = round(lat, 2)) %>%
  group_by(long_round, lat_round) %>%
  summarise(total_count = n()) %>%
  ungroup() 
ds_fet_locations_grp
#
#
# Plot tweet data on nike, adidas, shoes, grouping close tweets and 
# using larger points to show higher frequency
grouped_tweet_map <- world_basemap + 
  geom_point(data = ds_fet_locations_grp,
             aes(long_round, lat_round, size = total_count),
             color = "blue", alpha = .5) + 
  coord_fixed() +
  labs(title = "Twitter Activity and locations of tweets on nike_shoes, adidas_shoes, shoes",
       size = "Number of Tweets")
grouped_tweet_map
#
#
#
#
#
##############################################################
#
### ***
### *** Sentiment Analysis and Text Mining of Twitter Data 
### ***
#
###
### Searching for Tweets Related to NIKE
###
#
# let us look at a different workflow - exploring the actual text of the tweets which 
# will involve some text mining.
#
# In this example, let us find tweets that are using the hashtag #NIKE in them.
#
NIKE_tweets <- search_tweets(q = "#NIKE", n = 3000,
                                lang = "en",
                                include_rts = FALSE)
#
rt <- search_tweets(q = "#NIKE", n =3000, retryonratelimit = TRUE)
#
# check text data
head(NIKE_tweets$text)
#
#
###
### Data Clean-Up
###
#
# Looking at the data above, it becomes clear that there is a lot of clean-up associated 
# with social media data.
#
# First, there are urls in your tweets. If you want to do a text analysis to figure out 
# what words are most common in your tweets, the URLs won't be helpful. Let's remove those.
#
#
# First, remove http elements manually
NIKE_tweets$stripped_text <- gsub("http.*","",  NIKE_tweets$text)
NIKE_tweets$stripped_text <- gsub("https.*","", NIKE_tweets$stripped_text)
NIKE_tweets$stripped_text <- gsub("amp","", NIKE_tweets$stripped_text)
head(NIKE_tweets$stripped_text)
#
#
#
# Then, you can clean up your text. If you are trying to create a list of unique words 
# in your tweets, words with capitalization will be different from words that are all 
# lowercase. Also you don't need punctuation to be returned as a unique word
#
# You can use the unnest_tokens() function in the tidytext package to  
# clean up your text. When you use this function the following things will be cleaned up 
# in the text:
#
# 1. Convert text to lowercase: each word found in the text will be converted to lowercase 
# so ensure that you don't get duplicate words due to variation in capitalization.
# 
# 2. Punctuation is removed: all instances of periods, commas etc will be removed from your 
# list of words, and
#
# 3. Unique id associated with the tweet: will be added for each occurrence of the word
#
# The unnest_tokens() function takes two arguments:
#  
# 1) The name of the column where the unique word will be stored and
# 
# 2) The column name from the data.frame that you are using that you want to pull unique 
# words from.
#
# In your case, you want to use the stripped_text column which is where you have your 
# cleaned up tweet text stored.
#
# Let's remove punctuation, convert to lowercase, add id for each tweet:
NIKE_tweets_clean <- NIKE_tweets %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, stripped_text)
head(NIKE_tweets_clean)
#
#
# Now you can plot your data. 
# plot the top 10 words -- notice any issues?
NIKE_tweets_clean %>%
  count(word, sort = TRUE) %>% # count of number of occurencies of each word and sort according to count
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique words found in tweets with #NIKE") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
#
# Your plot of unique words contains some words that may not be useful to use. 
# For instance "a" and "to". In the word of text mining you call those words - "stop words". 
# You want to remove these words from your analysis as they are fillers used to compose 
# a sentence.
#
# Lucky for use, the tidytext package has a function that will help us clean up stop words. 
# To use this you:
#  
# 1. Load the stop_words data included with tidytext. This data is simply a list of words 
# that you may want to remove in a natural language analysis.
#
# 2. Then you use anti_join to remove all stop words from your analysis.
#
#
#
# load list of stop words - from the tidytext package
data("stop_words")
#
# view first 6 words
head(stop_words)
# the lexicon is the source of the stop word.
#
nrow(NIKE_tweets_clean)
#
#
# remove stop words from your list of words
cleaned_tweet_words <- NIKE_tweets_clean %>%
  anti_join(stop_words) # return all rows from NIKE_tweets_clean where there are not matching values in stop_words
#
# there should be fewer words now
nrow(cleaned_tweet_words)
#
#
# Now that you've performed this final step of cleaning, you can try to plot, once again.
#
# plot the top 10 words 
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with #NIKE") + 
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 16))
#
#
#
# Define our own stopwords that we don't want to include 
my_stop_words <- data.frame(word = c("items", "share"))
#
#
# remove our own stopwords from the list of words too
cleaned_tweet_words_2 <- cleaned_tweet_words %>%
  anti_join(my_stop_words) 
#
# there should be fewer words now
nrow(cleaned_tweet_words_2)
#
#
# plot the top 10 most popular words found in tweets with #NIKE
cleaned_tweet_words_2 %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words found in tweets with #NIKE") + 
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 16))
#
#
#
########################################
#
#
###
### Wordclouds
###
#
##
# Let's remind ourselves about the contents of cleaned_tweet_words_2
#
head(cleaned_tweet_words_2)
#
#
# Now for the wordcloud, calculate the count of each word and sort the words 
# according to the count.
# Then, calculate the frequency of each word, as 
# the count of each word / the total count
#
cleaned_tweet_words_3 <- cleaned_tweet_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(cleaned_tweet_words_3)
#
#
#
# Read the help file of wordcloud so that you can see what the arguments do
#
with(cleaned_tweet_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2"), 
               scale = c(4.5, 0.1)))
#
# Add a title
#
title(main = "Wordcloud for Tweets containing #NIKE", 
      cex.main = 2) # Controls the size of the title
#
#
# We are using the brewer colour palettes, as we like the colours
#
#------------------------------------
#
# Look at the possible colours
#
display.brewer.all()
# The first set of palettes are sequential palettes, suited to ordered data that progress from low to high.
#
# Next we see qualitative palettes.  These do not imply magnitude differences between legend classes, 
# and hues are used to create the primary visual differences between classes. Qualitative schemes are best suited to representing nominal or categorical data.
# 
# Finally, we see diverging palettes that put equal emphasis on mid-range critical values and extremes at both ends of the data range. 
# The critical class or break in the middle of the legend is emphasized with light colors and low and high extremes are emphasized with 
# dark colors that have contrasting hues. 
#
# We used the Dark2 colour palette.
#
#------------------------------------
#
# Alternative wordclouds
#
# Select only the variables "word" and "freq"
#
cleaned_tweet_words_4 <- cleaned_tweet_words_3 %>% select(word, freq)
#
# Produce wordcloud
#
wordcloud2(cleaned_tweet_words_4)
#
# Note that this appears in the Viewer and so are not ordinary plots
#
#
#
#
#####################################################
#
###
### Sentiment analysis
###
#
#
#
# The sentiments datatset from the package tidytext contains the Bing lexicon for sentiment analysis 
# each word is associated to a sentiment
sentiments
#
#
###
### Classify sentiments into positive and negative with the "bing" lexicon
###
#
#
# We can join the words extracted from the tweets with the sentiment data. 
# The "bing" sentiment data classifies words as positive or negative. 
# Note that other sentiment datasets use various classification approaches. 
#
#
# Join sentiment classification to the tweet words
bing_word_counts <- cleaned_tweet_words_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
head(bing_word_counts)
#
#
# Finally, plot top words, grouped by positive vs. negative sentiment. 
# it could be interesting to plot sentiment over time to see how sentiment changed over time.
#
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Most common Positive and Negative words in tweets on NIKE",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 15))
#
#
#
#############################################
#
#
###
### Calculate sentiment scores for each tweet
###
#
# We count up how many positive and negative words there are in each tweet
#
# associate sentiment scores to each tweet
NIKE_sentiment <- cleaned_tweet_words_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweetnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
head(NIKE_sentiment)
#
#
# Add a variable to indicate the topic
#
NIKE_sentiment <- NIKE_sentiment %>% 
  mutate(topic = "NIKE")
#
#
# Tabulate the scores
#
NIKE_sentiment %>% count(score)
#
# Let's work out the mean score 
# We'll include it as a line and as a numerical value to our plot
#
sentiment_means_NIKE <- NIKE_sentiment %>% 
  summarize(mean_score = mean(score)) 
sentiment_means_NIKE
#
# Barplot
#
ggplot(NIKE_sentiment, 
       aes(x = score)) + # Sentiment score on x-axis
  geom_bar(fill = "lightgreen", colour = "darkgreen") + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means_NIKE) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean_NIKE above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), # Show to three significant figures
            vjust = 2, 
            data = sentiment_means_NIKE) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10,  # Specify a suitable integer range for the x-axis
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments towards #NIKE give a mean of", signif(sentiment_means_NIKE$mean_score, 3)),
       # Title that gives page name and mean sentiment score, to three significant figures
       x = "Sentiment Score", 
       y = "Number of tweets") 
#
#
#
#############################################
#
#
###
### Comparison of Twitter sentiments
###
#
#
# Let us find tweets that are using the hashtag #adidas in them.
#
adidas_tweets <- search_tweets(q = "#adidas", n = 3000,
                                  lang = "en",
                                  include_rts = FALSE)
adidas_tweets <- search_tweets(q = "#adidas", n =3000,
                                 lang = "en",
                                 include_rts = FALSE,
                                 retryonratelimit = TRUE)
#
#
### Data Clean-Up
#
#
# remove http elements manually
adidas_tweets$stripped_text <- gsub("http.*","",  adidas_tweets$text)
adidas_tweets$stripped_text <- gsub("https.*","", adidas_tweets$stripped_text)
adidas_tweets$stripped_text <- gsub("amp","", adidas_tweets$stripped_text)
#
#
# Let's remove punctuation, convert to lowercase, add id for each tweet:
adidas_tweets_clean <- adidas_tweets %>%
  select(stripped_text) %>%
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, stripped_text)
#
#
# remove stop words from your list of words
adidas_tweets_clean <- adidas_tweets_clean %>%
  anti_join(stop_words) # return all rows from warming_tweets_clean where there are not matching values in stop_words
#
#
# Define our own stopwords that we don't want to include 
my_stop_words_2 <- data.frame(word = c("capsule", "phase"))
#
#
# remove our own stopwords from the list of words too
adidas_tweets_clean_2 <- adidas_tweets_clean %>%
  anti_join(my_stop_words_2) 
#
#
# Calculate the count of each word and its frequency
adidas_word_counts <- adidas_tweets_clean_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n)) 
head(adidas_word_counts)
#
#
# wordcloud of tweets on Adidas
#
with(adidas_word_counts, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2"), 
               scale = c(4.0, 0.1)))
#
# Add a title
#
title(main = "Wordcloud for Tweets containing #adidas", 
      cex.main = 2) # Controls the size of the title
#
#
#
### Calculate sentiment scores 
#
# We count up how many positive and negative words there are in each tweet
#
# associate sentiment scores to each tweet
adidas_sentiment <- adidas_tweets_clean_2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(tweetnumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
head(adidas_sentiment)
#
#
#
# Tabulate the scores
#
adidas_sentiment %>% count(score)
#
# Let's work out the mean score so that it can be added to the graph
#
# We'll include it as a line and as a numerical value
#
sentiment_means_adidas <- adidas_sentiment %>% 
  summarize(mean_score = mean(score)) 
sentiment_means_adidas
#
#
# Add a variable to indicate the topic
#
adidas_sentiment <- adidas_sentiment %>% 
  mutate(topic = "Adidas")
#
#
# Put the sentiment information from both pages together
#
word_counts_both <- rbind(NIKE_sentiment, 
                          adidas_sentiment) # rbind binds the rows of data frames
#
#
# Work out the means for each topic
# so that these can be added to the graph for each topic
# as a line and as a numerical value
#
sentiment_means_both <- word_counts_both %>% 
  group_by(topic) %>% 
  summarize(mean_score = mean(score)) 
sentiment_means_both
#
# Perform the plot
#
ggplot(word_counts_both, 
       aes(x = score, # Sentiment score on x-axis
           fill = topic)) + # Fill bars with a colour according to the topic
  geom_bar() + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), 
             data = sentiment_means_both) +
  # Add a vertical line at the mean scores, calculated and stored in sentiment_mean_both above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
            vjust = 2, 
            data = sentiment_means_both) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -15:15, 
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  scale_fill_manual(values = c("NIKE" = "green", 
                               "ADIDAS" = "blue")) + # Specify your own colours
  labs(x = "Sentiment Score" , 
       y = "Number of tweets", 
       fill = "Topic") +
  facet_grid(topic ~ .) + # One row for each page
  theme(legend.position = "bottom") # Legend on the bottom
#
# ----------------------------------------------------------------------------------
#

