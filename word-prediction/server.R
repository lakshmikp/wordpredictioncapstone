library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)

nGramIndex <- 4
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

shinyServer(
  function(input, output) {
       output$nextWord <- reactive({getNextWord(input$inputWords)})
     }
)