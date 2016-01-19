library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)

nGramIndex <- 5
numberOfPredictions <- 1
dt <- list()

for (m in 2:nGramIndex){
  dt[[m]] <- data.table()
}

for (m in 2:nGramIndex){
 fname <- paste0("dt", m, ".Rds")
 dt[[m]] <- readRDS(fname)
}

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
  if (x=="")  {
    return("")
  }
  print(x)
  x <- str_trim(stripWhitespace(removeNumbers(stemDocumentfix(removePunctuation(tolower(x))))))
  x <- gsub(x=x, pattern=" ", replacement="_")
  res <- data.table(Next=character(0), N=integer(0), Frequency=integer(0), Input=character(0))
  
  try(
    for (k in nGramIndex:2){
      st <- word(x, -1*(k-1), -1, sep=fixed('_'))
      
      
      result <- 
        dt[[k]] %>%
        filter (Input==st)%>%
        select(Next, Frequency, N, Input)
      
      res <- rbind(res, result)
      
    }, silent = TRUE)
  
  if (nrow(res)==0){
    if (numberOfPredictions >=3)
      print ("the,and,that")
    else print ("the")
    
  }else{
    #res <- mutate(res, Score=ifelse(Next  %in% stopwords(), yes=Frequency*N, no=Frequency*(10*N)))
    res <- res[order(-N, -Frequency)]
    print(res)
    #easi, import, excit,happi, peopl, amaz
    print(head(unique(res$Next),numberOfPredictions))
  }
  
}

shinyServer(
  function(input, output) {
    output$nextWord <- reactive({getNextWord(input$inputWords)})
    
  }
)