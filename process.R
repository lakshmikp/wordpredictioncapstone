setwd("D://Personal//Data Science//git//Capstone//data//")
#file.split("D://Personal//Data Science//git//Capstone//data-original//", size = 1024, same.dir = FALSE, verbose = TRUE, suf = "part", win = TRUE)

library(quanteda)
library(data.table)
library(plyr)
library(dplyr)
library(slam)
library(stringr)

nGramIndex <- 5
minimumFrequency <- c(0,20,10,5,2,1)
numberOfLines <- 100

createNGram <- function(x, m){
  tdmNgram <- dfm(unlist(x), ngrams = m, verbose = FALSE, stem = TRUE)
  d <- data.table(N=m, Term = colnames(tdmNgram), Frequency = unname((col_sums(tdmNgram))))
  
  #Remove non-alpha terms
  pt <- "[^_[:alpha:]]"
  d[grep(x=d$Term, pattern=pt, invert=TRUE)]
}


#******************************************************************************
mainDT <- list()
dt <- list()

for (m in 2:nGramIndex){
  mainDT[[m]] <- data.table(N= numeric(0), Term= character(0), Frequency = numeric(0))
  dt[[m]] <- data.table()
}


flist <- list.files(path = ".")

for (f in flist){  
  print(paste0(Sys.time(), "- file=", f))
  con <- file(f, open="rb")
  fdata <- readLines(con, skipNul=TRUE, n=numberOfLines)
  close(con)
  for (j in 2:nGramIndex){
    try(mainDT[[j]] <- rbind(mainDT[[j]], createNGram(fdata,j)),silent=TRUE)
    print(paste0(Sys.time(), "- Number of rows in mainDT[[",j,"]]=", nrow(mainDT[[j]])))
  }
  
}

#Saving the intemediate tables just as a backup (in case something goes wrong during aggregation)
#for (m in 2:nGramIndex){
 # fname <- paste0("..//rds//mainDT", m, ".Rds")
  #saveRDS(mainDT[[m]], file=fname)
#}

#for (m in 2:nGramIndex){
 # fname <- paste0("..//rds//mainDT", m, ".Rds")
  #mainDT[[m]] <- readRDS(fname)
#}

for (m in 2:nGramIndex){
  dt[[m]] <- aggregate(Frequency ~ Term+N, mainDT[[m]], FUN=sum)
  dt[[m]] <- subset(dt[[m]], Frequency>=minimumFrequency[m])
  
  dt[[m]] <- mutate(dt[[m]], Next= word(Term, start=-1,end=-1, sep=fixed('_')))
  dt[[m]] <- mutate(dt[[m]], Input = word(Term, start=1, end=(N-1), sep=fixed('_')))
  fname <- paste0("..//word-prediction//dt", m, ".Rds")
  saveRDS(dt[[m]], file=fname)
}


# Spell corrections
# Profanity
# score based on length of word  example: "conference call" (in this case 3gram shd get higher priority irrespective of the frequency)
# Note on UI about English only



library(tm)
library(SnowballC)

stemDocumentfix <- function(x)
{
  t <- stemDocument(unlist(strsplit(as.character(x), " ")))
  r <- ""
  for (i in 1:length(t)){
    r <- paste(r, t[i])
  }
  str_trim(r)
}


getNextWord <- function(x){
  print(x)
  x <- str_trim(stripWhitespace(removeNumbers(stemDocumentfix(removePunctuation(tolower(x))))))
  x <- gsub(x=x, pattern=" ", replacement="_")
  print(x)
  res <- data.table(Next=character(0), N=integer(0), Frequency=integer(0), Input=character(0))
  
  try(
    for (k in nGramIndex:2){
      st <- word(x, -1*(k-1), -1, sep=fixed('_'))
      print(paste("Looking for", st))
      print(head(dt[[k]]))
      result <- 
        dt[[k]] %>%
        filter (Input==st)%>%
        select(Next, Frequency, N, Input)
      
      res <- rbind(res, result)
      
    }, silent = TRUE)
  
  if (nrow(res)==0){
    print ("the")
  }else{
    res <- mutate(res, Score=ifelse(Next  %in% stopwords(), yes=Frequency*N, no=Frequency*(10*N)))
    print(arrange(res, desc(Score)))
    print(head(arrange(res, desc(Score)),1)$Next)
  }
  
}

#TBD
#Spelling corrections
