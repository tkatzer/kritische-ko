sentimentAnalysis <- function(DTM){
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
  negative = counts_negative / counts_all_terms,
  emotionality = counts_positive / counts_all_terms + counts_negative / counts_all_terms
)

sentiments_per_month <- aggregate(relative_sentiment_frequencies, by = list(month = lexiscorpus$documents$V5), mean)

head(sentiments_per_month)

df <- melt(sentiments_per_month, id.vars = "month")

#ggplot(data = df, aes(x = month, y = value, fill = variable)) + 
 # geom_bar(stat="identity", position=position_dodge()) 
lexiscorpus$documents[,7] <<- relative_sentiment_frequencies$positive
lexiscorpus$documents[,8] <<- relative_sentiment_frequencies$negative
lexiscorpus$documents[,09] <<- relative_sentiment_frequencies$positive - relative_sentiment_frequencies$negative

# plotting the sentiments ordered by positivity
ggplot(data = df, aes(x = month, y = value)) + geom_bar(stat="identity", position=position_dodge()) 

}