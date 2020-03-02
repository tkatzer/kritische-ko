compareyears <- function(data,year){
  
  
  ##Beispielplot aus dem ersten Artikel
  Ignaural <- lexiscorpus %>% 
    tokens(remove_symbols= TRUE,remove_punct = TRUE) %>% 
    tokens_remove(stopwords("en")) %>%
    dfm() %>% 
    dfm_trim(min_termfreq = 10)
  
  
  Ignaural_2019 <- textstat_keyness(Ignaural, target = which(docvars(Ignaural, 'year') == year))
  
  textplot_keyness(Ignaural_2019)
}