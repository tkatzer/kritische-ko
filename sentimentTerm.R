sentimentTerm<-function(term,grade) {
  
  
  com3m<-lexiscorpus$documents[which(grepl(term,lexiscorpus$documents$texts)),]
  sentimentcorpus<-corpus(com3m$texts)
  docvars(sentimentcorpus, "date")<-com3m$V5
 
  
  lemma_data<-read.csv2(file = "lemma_data.csv")
  lemma_data<-lemma_data[,c(2:3)]
  stopwords <- append(stopwords("en"), c("0","1","2","3","4","5","6","7","8","9","https","www.businessinsider.com","january", "february", "march", "april", "may", "june", "july", "august","september", "october", "november", "december"))
  
  corpus_tokens <- sentimentcorpus %>% 
    tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
    tokens_tolower() %>% 
    tokens_replace(as.character(lemma_data$inflected_form), as.character(lemma_data$lemma), valuetype = "fixed")%>%
    tokens_remove(pattern = stopwords, padding = T)
  
  collocations <- textstat_collocations(corpus_tokens, min_count = 20)
  
  # compound collocations
  corpus_tokens <- tokens_compound(corpus_tokens, collocations)
  
  # Create DTM (also remove padding empty term)
  DTM <<- corpus_tokens %>% 
    tokens_remove("") %>%
    dfm() 
  
  afinn_terms <- get_sentiments(lexicon = "afinn")
  afinn_terms <- rbind(afinn_terms,data.frame(word = "fraud",value = -4),data.frame(word = "money_launder", value = -4)) 
  positive_terms_all <- afinn_terms$word[afinn_terms$value > 0]
  negative_terms_all <- afinn_terms$word[afinn_terms$value < 0]
  
  
  
  positive_terms_in_suto <- intersect(colnames(DTM), positive_terms_all)
  counts_positive <- rowSums(DTM[, positive_terms_in_suto])
  
  negative_terms_in_suto <- intersect(colnames(DTM), negative_terms_all)
  counts_negative <- rowSums(DTM[, negative_terms_in_suto])
  
  
  
  counts_all_terms <- rowSums(DTM)
  
  relative_sentiment_frequencies <- data.frame(
    # positive = counts_positive / counts_all_terms
   # negative = counts_negative / counts_all_terms
     emotion = counts_positive / counts_all_terms - counts_negative / counts_all_terms
  )
  
  sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = sentimentcorpus$documents$date), mean)
  
  head(sentiments_per_month)
  
  df <- melt(sentiments_per_month, id.vars = "month")
  options(stringsAsFactors = T)
  ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
    geom_point(stat="identity") + geom_smooth(method = "lm", formula = df$value ~ poly(df$month, 3), se = FALSE)

  plot(ksmooth(time(df$value), df$value, "normal", bandwidth = grade), lwd=2, col=4,type="l")
  df$month
 }
