topicModel<-function(K){
  K <- 15
  # setwd("Your work directory")
  options(stringsAsFactors = FALSE)
  library(quanteda)
  require(topicmodels)
  
  textdata <- data
  textdata <- textdata %>%separate('date', into=c("year","month","day"), sep = "-")
  textdata$year[textdata$year=="0012"]<-"2018"
  sotu_corpus <- lexiscorpus
  #sotu_corpus$documents$year[sotu_corpus$documents$year=="0012"] <- "2018"
  lemma_data<-read.csv2(file = "lemma_data.csv")
  lemma_data<<-lemma_data[,c(2:3)]
  stopwords <- append(stopwords("en"), c("0","1","2","3","4","5","6","7","8","9","https","www.businessinsider.com","january", "february", "march", "april", "may", "june", "july", "august","september", "october", "november", "december"))
  
  corpus_tokens <- lexiscorpus %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed")%>%
    tokens_remove(pattern = stopwords, padding = T)
  
  collocations <<- textstat_collocations(corpus_tokens, min_count = 20)
  
  # compound collocations
  corpus_tokens <<- tokens_compound(corpus_tokens, collocations)
  
  # Create DTM (also remove padding empty term)
  DTM <<- corpus_tokens %>% 
    tokens_remove("") %>%
    dfm() 
  
  dfm_trim(min_docfreq = 0.01, max_docfreq = Inf, docfreq_type = "prop")
  
  # have a look at the number of documents and terms in the matrix
  dim(DTM)
  
  
  top10_terms <- c( "unite_state", "past_year", "year_ago", "year_end", "government", "state", "country", "year", "make", "seek")
  
  DTM <- DTM[, !(colnames(DTM) %in% top10_terms)]
  
  # due to vocabulary pruning, we have empty rows in our DTM
  # LDA does not like this. So we remove those docs from the
  # DTM and the metadata
  sel_idx <- rowSums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  textdata <- textdata[sel_idx, ]
  
  # load package topicmodels
  require(topicmodels)
  # number of topics
 
  # set random number generator seed
  set.seed(9161)
  # compute the LDA model, inference via 1000 iterations of Gibbs sampling
  topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
  
  # have a look a some of the results (posterior distributions)
  tmResult <- posterior(topicModel)
  # format of the resulting object
  attributes(tmResult)
  
  # topics are probability distribtions over the entire vocabulary
  beta <- tmResult$terms   # get beta from results
  dim(beta)                # K distributions over ncol(DTM) terms
  
  
  # for every document we have a probability distribution of its contained topics
  theta <- tmResult$topics 
  dim(theta)               # nDocs(DTM) distributions over K topics
  
  
  terms(topicModel, 10)
  
  top5termsPerTopic <- terms(topicModel, 5)
  topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
  
  require(wordcloud2)
  # visualize topics as word cloud
  topicToViz <- 11 # change for your own topic of interest
  # select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
  top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
  words <- names(top40terms)
  # extract the probabilites of each of the 40 terms
  probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
  # visualize the terms as wordcloud
  wordcloud2(data.frame(words, probabilities), shuffle = FALSE, size = 0.8)
  
  exampleIds <- c(2, 100, 200)
  # load libraries for visualization
  library("reshape2")
  library("ggplot2")
  N <- length(exampleIds)
  # get topic proportions form example documents
  topicProportionExamples <- theta[exampleIds,]
  colnames(topicProportionExamples) <- topicNames
  vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
  
  ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
    coord_flip() +
    facet_wrap(~ document, ncol = N)
  
  
  
  
  
  
  topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
  tmResult <- posterior(topicModel2)
  theta <- tmResult$topics
  beta <- tmResult$terms
  topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topicnames
  
  # get topic proportions form example documents
  topicProportionExamples <- theta[exampleIds,]
  colnames(topicProportionExamples) <- topicNames
  vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
  
  ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
    coord_flip() +
    facet_wrap(~ document, ncol = N)
  
  # re-rank top topic terms for topic names
  topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
  
  # What are the most probable topics in the entire collection?
  topicProportions <- colSums(theta) / nrow(DTM)  # mean probablities over all paragraphs
  names(topicProportions) <- topicNames     # assign the topic names we created before
  sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order
  
  countsOfPrimaryTopics <- rep(0, K)
  names(countsOfPrimaryTopics) <- topicNames
  for (i in 1:nrow(DTM)) {
    topicsPerDoc <- theta[i, ] # select topic distribution for document i
    # get first element position from ordered list
    primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
    countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
  }
  sort(countsOfPrimaryTopics, decreasing = TRUE)
  
  
  topicToFilter <- 6  # you can set this manually ...
  # ... or have it selected by a term in the topic name
  topicThreshold <- 0.1 # minimum share of content must be attributed to the selected topic
  selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
  filteredCorpus <- sotu_corpus %>% corpus_subset(subset = selectedDocumentIndexes)
  
  # show length of filtered corpus
  filteredCorpus
  
  
  # append decade information for aggregation
  
  # get mean topic proportions per decade
  topic_proportion_per_decade <- aggregate(theta, by = list(decade = textdata$year), mean)
  # set topic names to aggregated columns
  colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames
  
  # reshape data frame
  vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")
  vizDataFrame<-vizDataFrame[which(vizDataFrame$value>0.1),]
  # plot topic proportions per deacde as bar plot
  require(pals)
  ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
    geom_bar(stat = "identity") + ylab("proportion") + 
    scale_fill_manual(values = paste0(alphabet(14), "FF"), name = "decade") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}
