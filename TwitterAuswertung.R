setwd("/Users/timkatzer/Desktop/ArtikelBlackrock")
install.packages("maps")
library(shiny)
library(dplyr)
library(tidyverse)
library(quanteda)
require(igraph)
library(visNetwork)
library(geomnet)
library(maps)
library(ggplot2)
source("berechne.R")
source("calculateCoocStatistics.R")  
library("LexisNexisTools")  
options(stringsAsFactors = F)

data<-read.csv2(file = "allblackrocktweets.csv")
data2<-read.csv2(file="allblackrocktweets2.csv")
data3<-read.csv3(file="allblackrocktweets3.csv")
blackrocktweets<-rbind(data,data2,data3)

blackrocktweets<-blackrocktweets[which(!duplicated(blackrocktweets$status_id)),]

## Metadaten hinzufügen
twittercorpus<-corpus(blackrocktweets$text, docnames = blackrocktweets$X)
docvars(twittercorpus, "date")<-blackrocktweets$created_at
docvars(twittercorpus, "retweets")<-blackrocktweets$retweet_count
docvars(twittercorpus, "location")<-blackrocktweets$location
docvars(twittercorpus, "location")<-blackrocktweets$location

load("brten.RData")
## create lat/lng variables using all available tweet and profile geo-location data
karte <- lat_lng(brten)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("world", lwd = 1)

## plot lat and lng points onto state map
with(karte, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


lemma_data<-read.csv2("baseform_en.tsv", header = F, sep = ",", dec = ".")
head(lemma_data,10)
as.data.frame(lemma_data, sep = "/t", col.names = c("lemma","inflected_form"))
colnames(lemma_data)
lemma_data<-lemma_data %>% rename(lemma = V1, inflected_form = V2 )
lemma_data[,3]<-lemma_data[,1]
lemma_data[,1]<-lemma_data[,2]
lemma_data[,2]<-lemma_data[,3]
lemma_data<-lemma_data[,c(1:2)]
lemma_data<-lemma_data[2:47366,]
write.csv2(lemma_data, file = "lemma_data.csv")

lemma_data<-read.csv2(file = "lemma_data.csv")
lemma_data<-lemma_data[,c(2:3)]
stopwords <- append(stopwords("en"), c("https","t.co","https://t.co","www","january", "february", "march", "april", "may", "june", "july", "august","september", "october", "november", "december"))

corpus_tokens <- twittercorpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
  tokens_remove(pattern = stopwords, padding = T)


collocations <- textstat_collocations(corpus_tokens, min_count = 20)
sum(collocations$count > 20)
head(collocations, 50)
tail(collocations, 25)

# compound collocations
corpus_tokens <- tokens_compound(corpus_tokens, collocations)

# Create DTM (also remove padding empty term)
DTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() 




corpus_sentences <- corpus_reshape(twittercorpus, to = "sentences")


corpus_tokens <- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords, padding = T) %>%
  tokens_remove(pattern = stopwords("en"), padding = T)

collocations <- textstat_collocations(corpus_tokens, min_count = 20)
sum(collocations$count > 20)
head(collocations, 50)
tail(collocations, 25)

minimumFrequency <- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = Inf) %>% 
  dfm_weight("boolean")

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM
head(coocCounts,20)
as.matrix(coocCounts[202:205, 202:205])

coocTerm <- "price"
k <- nrow(binDTM)
ki <- sum(binDTM[,coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]


########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]

########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]

########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
               + (k - ki - kj + kij) * log(k - ki - kj + kij) 
               + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
               - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]

# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)


source("calculateCoocStatistics.R")

numberOfCoocs<-20


berechne<- function(coocTerm, numberOfCoocs, binDTM){
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
  # Display the numberOfCoocs main terms
  print(coocs[1:numberOfCoocs])
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  # Sample of some examples from resultGraph
  resultGraph[sample(nrow(resultGraph), 6), ]
  
  require(igraph)
  
  # Set the graph and type
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  
  # Identification of all nodes with less than 2 edges
  graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  graphNetwork <- delete.vertices(graphNetwork, graphVs) 
  
  # Assign colors to edges and nodes (searchterm blue, rest orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
  
  # Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
  halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
  E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
  
  # Disable edges with radius
  E(graphNetwork)$curved <- 0 
  # Size the nodes by their degree of networking
  V(graphNetwork)$size <- log(degree(graphNetwork)) * 2
  
  # All nodes must be assigned a standard minimum-size
  V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 2
  
  # edge thickness
  E(graphNetwork)$width <- 1
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  
  # Finaler Plot
  plot(graphNetwork, rescale=F,              
       layout = layout.kamada.kawai(graphNetwork,dim=3),
       #graphNetwork,repulserad=vcount(graphNetwork)^3,area=vcount(graphNetwork)^2.4),  # Force Directed Layout 
       main = paste(coocTerm, ' Graph'),
       vertex.label.family = "sans",
       vertex.label.cex = 0.8,
       vertex.shape = "circle",
       vertex.label.dist = 0.5,           # Labels of the nodes moved slightly
       vertex.frame.color = 'darkolivegreen',
       vertex.label.color = 'black',      # Color of node names
       vertex.label.font = 2,         # Font of node names
       vertex.label = V(graphNetwork)$name,       # node names
       vertex.label.cex = 1 # font size of node names 
  )
  
  print("here we go")
  library(visNetwork)
  library(geomnet)
  data <- toVisNetworkData(graphNetwork)
  visNetwork(nodes = data$nodes, edges = data$edges, physics = TRUE, height = "1000px", width = "1000px")%>%
    visPhysics(stabilization = TRUE)%>%
    visEdges(smooth = FALSE)
}
berechne("climate",20,binDTM)


berechne("microsoft",20,binDTM)
kwic(twittercorpus,"climate")
kwic(twittercorpus,"paris")
dev.off()



# load package topicmodels
require(topicmodels)
# number of topics
K <- 20
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

ncol(DTM)                # lengthOfVocab
# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over ncol(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nrow(DTM)               # size of collection
# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

require(wordcloud2)
# visualize topics as word cloud
topicToViz <- 1 # change for your own topic of interest
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)





twittercorpus$documents <- twittercorpus$documents %>% separate('date', into=c("year","month","day"), sep = "-")
twittercorpus$documents <- twittercorpus$documents %>% separate('day', into=c("day","time"), sep = " ")
### lets take a why bmw seems to be in articles a lot
terms_to_observe <- c("environment", "green", "climate","stock","price","paris")

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

counts_per_day <- aggregate(DTM_reduced, by = list(day = twittercorpus$documents$day), sum)

# give x and y values beautiful names
days <- counts_per_day$day
frequencies <- counts_per_day[, terms_to_observe]

# plot multiple frequencies
matplot(days, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  







