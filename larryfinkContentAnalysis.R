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
if (!require(lda)) install.packages("lda")
if (!require(pals)) install.packages("pals")
library(pals)
library(lda)
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
source("berechne.R")
source("signCooc.R")
source("compareyears.R")
source("dataprep.R")
source("topicModel.R")
source("relTerms.R")
source("sentimentAnalysis.R")
source("sentimentTerm.R")
library("LexisNexisTools")  
options(stringsAsFactors = F)

data<-read.csv2(file = "larryfinkarticle.csv")
data<-data[,c(2:5)]
data<-na.omit(data)

dataprep(data)
lexiscorpus$documents$year[974] <- "2018"
mean(lexdiv$Maas)
sd(lexdiv$Maas)


terms_to_observe<-c("climate","social")
relTerms(DTM)


terms_to_observe<-c("blockchain","crypto","bitcoin")
relTerms(DTM)

terms_to_observe<-c("gun","weapon","violence")
relTerms(DTM)

terms_to_observe<-c("nuclear","weapon","coal","social","climate","car")
relTerms(DTM)

terms_to_observe<-c("greece","ecb")
relTerms(DTM)


terms_to_observe<-c("syria","weapon","assad")
relTerms(DTM)

terms_to_observe<-c("parkland","weapon","gun")
relTerms(DTM)

terms_to_observe<-c("putin","ukraine","russia")
relTerms(DTM)


#"pension" Artikel 2381
compareyears(data,"2018")
sum(lexiscorpus$documents$year=="2015")
sum(collocations$count > 20)
head(collocations, 50)


#minimumFrequency <- 10

#coocTerm <- "climate"

signCooc("bitcoin",binDTM)
#numberOfCoocs<-20
berechne("cryptocurrency",15,binDTM)
berechne("pension",20,binDTM)
berechne("saudi",15,binDTM)
topicModel(20)

sentimentTerm("coal",20)

# Untersuchung der positivsten Artikel
# Sentiment schlechte Monate
dev.off()
bitcoinKurs<-read.csv2("BTC-USD.csv", sep = ",")
bitcoin_year<-read.csv("BTCUSD=X.csv",sep=",")
bitcoin_next<-read.csv("BTCUSD=XY.csv",sep=",")
plot(bitcoin_year$Close, type="l")
abline(v=141)
plot(bitcoinKurs$Close[220:366],type = "l")
abline(v=69)
plot(bitcoin_next$Close, type="l")

