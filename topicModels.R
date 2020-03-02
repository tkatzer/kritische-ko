require(topicmodels)
library(visNetwork)
library(geomnet)
library(shiny)
library(dplyr)
library(tidyverse)
library(quanteda)
require(igraph)
source("berechne.R")
source("calculateCoocStatistics.R")
library("LexisNexisTools")
# require data from previous shit 
options(stringsAsFactors = F)



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
topicToViz <- 20 # change for your own topic of interest
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)



