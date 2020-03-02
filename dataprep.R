dataprep <- function(data){
lexiscorpus<-corpus(data$article, docnames = data$title)

lexdiv<<-textstat_lexdiv(dfm(lexiscorpus),measure = c("TTR","Maas"))

docvars(lexiscorpus, "date")<-data$date

lexiscorpus$documents <- lexiscorpus$documents %>% separate('date', into=c("year","month","day"), sep = "-")

for (i in 1:length(lexiscorpus$documents$texts)){
  lexiscorpus$documents[i,5]<-paste0(lexiscorpus$documents$year[i],"-",lexiscorpus$documents$month[i])
}

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


corpus_sentences <<- corpus_reshape(lexiscorpus, to = "sentences")


corpus_tokens <<- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "fixed") %>% 
  tokens_remove(pattern = stopwords, padding = T) %>%
  tokens_remove(pattern = stopwords("en"), padding = T)

minimumFrequency <<- 10

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <<- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = Inf) %>% 
  dfm_weight("boolean")
lexiscorpus <<-lexiscorpus
}
