setwd("/Users/timkatzer/Desktop/ArtikelBlackrock")
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')
if (!require(lubridate)) install.packages('lubridate')
if (!require(data.table)) install.packages('data.table')
if (!require(quanteda)) install.packages('quanteda')
if (!require(igraph)) install.packages('igraph')
if (!require(visNetwork)) install.packages('visNetwork')
if (!require(geomnet)) install.packages('geomnet')
if (!require(topicmodels)) install.packages('topicmodels')
if (!require(wordcloud2)) install.packages('wordcloud2')
if (!require(repmis)) install.packages('repmis')
if (!require(textdata)) install.packages('textdata')
if (!require(tidytext)) install.packages('tidytext')
if (!require(Matrix)) install.packages('Matrix')
if (!require(topicmodels)) install.packages('topicmodels')
if (!require(reshape2)) install.packages('reshape2')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(remotes)) install.packages("remotes")
if (!require(LexisNexisTools)) remotes::install_github("JBGruber/LexisNexisTools")
if (!require(neuralnet)) install.packages("neuralnet")
library(neuralnet)
library(ggplot2)
library(reshape2)
library(Matrix)
library(textdata)
library(tidytext)
library(repmis)
library(wordcloud2)
library(topicmodels)
library(LexisNexisTools)
library(caret)
library(lubridate)
library(data.table)
library(tidyverse)
library(quanteda)
require(igraph)
library(visNetwork)
library(geomnet)
library(topicmodels)
source("calculateCoocStatistics.R")  
library("LexisNexisTools")  
options(stringsAsFactors = F)

## entity extraction 
#articles <- lnt_read("Files(49).DOCX")
#articles2 <- lnt_read("Files(50).DOCX")
#data<-data.frame(source = articles@meta$Newspaper, date = articles@meta$Date, title = articles@meta$Headline, article = articles@articles$Article)
#data2<-data.frame(source = articles2@meta$Newspaper, date = articles2@meta$Date, title = articles2@meta$Headline, article = articles2@articles$Article)
#dataLN <- rbind(data, data2)
##quanteda_corpus <- lnt_convert(articles, to = "quanteda")
#write.csv2(dataLN, file = "allelexisnexisartikel.csv")
#getwd()

data<-read.csv2(file = "allelexisnexisartikel.csv")
data<-data[,c(2:5)]
data<-na.omit(data)


## forbes Unternehmenliste companycorpus<-corpus(data$name, docnames = data$name)
lexiscorpus<-corpus(data$article, docnames = data$title)
docvars(lexiscorpus, "date")<-data$date
head(lexiscorpus$documents,2)



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
stopwords <- append(stopwords("en"), c("blackrock","january", "february", "march", "april", "may", "june", "july", "august","september", "october", "november", "december"))

corpus_tokens <- lexiscorpus %>% 
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

# IDF: log(N / n_i)
number_of_docs <- nrow(DTM)
term_in_docs <- colSums(DTM > 0)
idf <- log2(number_of_docs / term_in_docs)

lexiscorpus$documents <- lexiscorpus$documents %>% separate('date', into=c("year","month","day"), sep = "-")

for (i in 1:length(lexiscorpus$documents$texts)){
lexiscorpus$documents[i,5]<-paste0(lexiscorpus$documents$year[i],"-",lexiscorpus$documents$month[i])
}


# TF vom ersten Artikel
artikelneu <- which(docvars(lexiscorpus, 'V3') == "2010")
tf <- as.vector(DTM[artikelneu, ])
# Compute TF-IDF
tf_idf <- tf * idf
names(tf_idf) <- colnames(DTM)

df_tf_idf<-sort(tf_idf, decreasing = T)[1:50]
as.matrix(df_tf_idf)

##Beispielplot aus dem ersten Artikel
Ignaural <- lexiscorpus %>% 
  tokens(remove_symbols= TRUE,remove_punct = TRUE) %>% 
  tokens_remove(stopwords("en")) %>%
  dfm() %>% 
  dfm_trim(min_termfreq = 10)


Ignaural_2019 <- textstat_keyness(Ignaural, target = which(docvars(Ignaural, 'year') == "2014"))

textplot_keyness(Ignaural_2019)



corpus_sentences <- corpus_reshape(lexiscorpus, to = "sentences")


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

coocTerm <- "climate"
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

calculateCoocStatistics("climate",binDTM, measure = "DICE")
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
berechne("letter",20,binDTM)
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
  





### 
terms_to_observe <- c("environment", "green", "climate")

DTM_reduced <- as.matrix(DTM[, terms_to_observe])

counts_per_year <- aggregate(DTM_reduced, by = list(year = lexiscorpus$documents$year), sum)

# give x and y values beautiful names
year <- counts_per_year$year
frequencies <- counts_per_year[, terms_to_observe]

# plot multiple frequencies
matplot(year, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  















# sentiment analysis with AFINN sentiment lexicon 
afinn_terms <- get_sentiments(lexicon = "afinn")
positive_terms_all <- afinn_terms$word[afinn_terms$value > 0]
negative_terms_all <- afinn_terms$word[afinn_terms$value < 0]



positive_terms_in_suto <- intersect(colnames(DTM), positive_terms_all)
counts_positive <- rowSums(DTM[, positive_terms_in_suto])

negative_terms_in_suto <- intersect(colnames(DTM), negative_terms_all)
counts_negative <- rowSums(DTM[, negative_terms_in_suto])



counts_all_terms <- rowSums(DTM)

relative_sentiment_frequencies <- data.frame(
  positive = counts_positive / counts_all_terms,
  negative = counts_negative / counts_all_terms
)

sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = lexiscorpus$documents$V3), mean)

head(sentiments_per_month)

df <- melt(sentiments_per_month, id.vars = "month")

# plotting the sentiments ordered by positivity
ggplot(data = df, aes(x = month, y = value)) + geom_bar(stat="identity", position=position_dodge()) 

ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
  geom_bar(stat="identity", position=position_dodge()) 


# add sentiment scores to corpus
taxcorpus$documents[,8] <- relative_sentiment_frequencies$positive
taxcorpus$documents[,9] <- relative_sentiment_frequencies$negative
taxcorpus$documents[,10] <- relative_sentiment_frequencies$positive + relative_sentiment_frequencies$negative


