setwd("D://Personal//Data Science//git//Capstone//data//")

library(tm)
library(RWeka)
library(data.table)
library(plyr)
library(dplyr)
library(slam)
library(stringr)
library(ggplot2)

createCorpus <- function(x){
  corpus <- Corpus(VectorSource(x))
  print(paste(Sys.time(), " - Staring transformations..."))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, removeWords, stopwords())
  corpus <- tm_map(corpus, removeNumbers)
  corpus
}
createNGram <- function(corpus, m){
  print(paste(Sys.time(), " - Staring tokenization..."))
  nGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = m, max = m))
  tdmNgram <- TermDocumentMatrix(corpus, control = list(tokenize = nGramTokenizer))
  if (is.null(rownames(tdmNgram))){
    data.table(N=m, Term = "",  Next="", Frequency = 0)
  }else{
  data.table(N=m, Term = rownames(tdmNgram),  Next="", Frequency = unname((row_sums(tdmNgram))))
  }
}
plotHist <- function(dt, l, f){
  popular <- arrange(head(arrange(dt, desc(Frequency)), 20), desc(Frequency))
  ggplot(popular) + 
    geom_bar(mapping=aes(x = Term, y = Frequency), stat="summary", fun.y=sum, fill=f)+ 
    labs(title=l, x="Term", y="Frequency") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
}


con <- file("en_US//en_US.news.txt", open="rb")
US_news <- readLines(con, skipNul=TRUE, n=100000)
close(con)
con <- file("en_US//en_US.blogs.txt", open="rb")
US_blogs <- readLines(con, skipNul=TRUE, n=10000)
close(con)
con <- file("en_US//en_US.twitter.txt", open="rb")
US_twitter <- readLines(con, skipNul=TRUE, n=200000)
close(con)

mainDT1 <- data.table()
mainDT2 <- data.table()
mainDT3 <- data.table()

cr <- createCorpus(US_blogs)
mainDT1 <- rbind(mainDT1, createNGram(cr,1))
mainDT2 <- rbind(mainDT2, createNGram(cr,2))
mainDT3 <- rbind(mainDT3, createNGram(cr,3))
print(nrow(mainDT1))
print(nrow(mainDT2))
print(nrow(mainDT3))

t <- split(US_twitter, 1:20)
for (i in 1:20){
  cr <- createCorpus(US_twitter[i])
  mainDT1 <- rbind(mainDT1, createNGram(cr,1))
  mainDT2 <- rbind(mainDT2, createNGram(cr,2))
  mainDT3 <- rbind(mainDT3, createNGram(cr,3))
  print(nrow(mainDT1))
  print(nrow(mainDT2))
  print(nrow(mainDT3))
}

t <- split(US_news, 1:10)
for (i in 1:10){
  cr <- createCorpus(US_news[i])
  mainDT1 <- rbind(mainDT1, createNGram(cr,1))
  mainDT2 <- rbind(mainDT2, createNGram(cr,2))
  mainDT3 <- rbind(mainDT3, createNGram(cr,3))
  print(nrow(mainDT1))
  print(nrow(mainDT2))
  print(nrow(mainDT3))
}

dt1 <- aggregate(Frequency ~ Term, mainDT1, FUN=sum)
dt2 <- aggregate(Frequency ~ Term, mainDT2, FUN=sum)
dt3 <- aggregate(Frequency ~ Term, mainDT3, FUN=sum)

dt1$FrequencyLevel <- ifelse(dt1$Frequency > 100,"E: More than 100", ifelse(dt1$Frequency > 50,"D: Between 50 and 100", ifelse(dt1$Frequency > 10,"C: Between 10 and 50", ifelse(dt1$Frequency > 1,"B: Between 1 and 10", "A: Just 1"))))

dt2$FrequencyLevel <- ifelse(dt2$Frequency > 100,"E: More than 100", ifelse(dt2$Frequency > 50,"D: Between 50 and 100", ifelse(dt2$Frequency > 10,"C: Between 10 and 50", ifelse(dt2$Frequency > 1,"B: Between 1 and 10", "A: Just 1"))))
table(dt2[,c("FrequencyLevel")])

dt3$FrequencyLevel <- ifelse(dt3$Frequency > 100,"E: More than 100", ifelse(dt3$Frequency > 50,"D: Between 50 and 100", ifelse(dt3$Frequency > 10,"C: Between 10 and 50", ifelse(dt3$Frequency > 1,"B: Between 1 and 10", "A: Just 1"))))
table(dt3[,c("FrequencyLevel")])


#saveRDS(mainDT1, file="mainDT1.Rds")
#saveRDS(mainDT2, file="mainDT2.Rds")
#saveRDS(mainDT3, file="mainDT3.Rds")

#setDT(mainDT2)[,Next := word(mainDT2$Term, -1)]; setDT(mainDT2)[,Term := word(mainDT2$Term, 1)]
#setDT(mainDT3)[,Next := word(mainDT3$Term, -1)]; setDT(mainDT3)[,Term := word(mainDT3$Term, 1,2)]

#Remove the combinations that don't have highest frequencies.
#Remove the equally probable combinations.

#TBD







