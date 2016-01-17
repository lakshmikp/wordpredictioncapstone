memory.limit(size=100000)
setwd("D://Personal//Data Science//git//Capstone//data//")
source("..//spell-correction.R")
#file.split("D://Personal//Data Science//git//Capstone//data-original//", size = 1024, same.dir = FALSE, verbose = TRUE, suf = "part", win = TRUE)

library(quanteda)
library(data.table)
library(plyr)
library(dplyr)
library(slam)
library(stringr)

nGramIndex <- 5
minimumFrequency <- c(0,20,10,5,2,1)
numberOfLines <- 200

createNGram <- function(x, m){
  tdmNgram <- dfm(unlist(x), ngrams = m, verbose = FALSE, stem = TRUE)
  d <- data.table(N=m, Term = colnames(tdmNgram), Frequency = unname((col_sums(tdmNgram))))
  
  #Remove non-alpha terms
  pt <- "[^_[:alpha:]]"
  d[grep(x=d$Term, pattern=pt, invert=TRUE)]
}

getCorrectSpelling <- function(x, l){
  print(paste0(Sys.time(), "- Inside function getCorrectSpelling"))
  n <- length(x)
  for (i in 1:n){
    res <- subset(l, Word==x[i])$Correction
    if (! is.null(res)){
      x[i] <- head(res,1)
    }
    
  }
  return(x)
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
  
  
  #Spell corrections
  splist <- data.table(Word=character(0), Correction=character(0))
  uniqueWords <- unique(dt[[m]]$Next)
  for (w in uniqueWords){
    splist <- rbind(splist, data.table(Word=w, Correction=correct(w)))
    
  }
  dt[[m]] <- mutate(dt[[m]], Next=getCorrectSpelling(Next, splist))
  
  

  fname <- paste0("..//word-prediction//dt", m, ".Rds")
  saveRDS(dt[[m]], file=fname)
}







                    

