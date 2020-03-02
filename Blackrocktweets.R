install.packages("rtweet")
install.packages("ggmap")
install.packages("igraph")
install.packages("ggraph")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(rtweet)

# Speficy Authentification Token's provided in your Twitter App
create_token(
  app = "sentiment_analysisblackrock",
  consumer_key = "vku9cIc8obbD3WIgyut2JvT4r",
  consumer_secret = "YladQec4dqU26OHWFa7KYFpM3vSNXoRwsch2POnf4HU69n8l1U ",
  access_token = "1135865606870380544-E7V499v7uRmv4Lx90Dh9Np8pObThQt",
  access_secret = "xJcJi3nol3VaEzvKhrC7PSODrqZMZRgysOb98D0LxAX3e"
)


# Collect Tweets that contain specific keywords 


blackrocktweets13 <- search_tweets(
  q = "blackrock",
  n = 18000,
  file_name = "amazon.json",
  parse = TRUE,
  retryonratelimit = TRUE,
  max_id = "1216985298107682816"
)
sum(blackrocktweets$hashtags[!is.na(blackrocktweets$hashtags)] == "blackrock")
blackrock <- parse_stream("blackrock.json")
save(blackrocktweets, file = "blackrock.Rda")
blackrocktweets5$status_id[4917]
sum(blackrocktweets4$lang== "en")
amazon$text

blackrocktweets12$status_id[15858]
max_id = "1217505069811236865",

brten <- rbind(blackrocktweets10[blackrocktweets10$lang == "en",],blackrocktweets11[blackrocktweets11$lang == "en",],blackrocktweets12[blackrocktweets12$lang == "en",])
allblackrocktweets2 <- brten[,c(1:16, 32:69, 73:90)]
allblackrocktweets2 <- allblackrocktweets2[,c(1:53,55:72)]
write.csv2(allblackrocktweets2, file = "allblackrocktweets3.csv")


## last status ID: 1214571087544995840


#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
