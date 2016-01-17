# Read in big.txt, a 6.5 mb collection of different English texts.
con <- file("D://Personal//Data Science//git//Capstone//big.txt", open="rb")
raw_text <- paste(readLines(con), collapse = " ")
close(con)


# Make the text lowercase and split it up creating a huge vector of word tokens.
split_text <- strsplit(tolower(raw_text), "[^a-z]+")
remove (raw_text)

# Count the number of different type of words.
word_count <- table(split_text)
remove(split_text)

# Sort the words and create an ordered vector with the most common type of words first.
sorted_words <- names(sort(word_count, decreasing = TRUE))
remove(word_count)


correct <- function(word) {
  edit_dist <- adist(word, sorted_words)
  min_edit_dist <- min(edit_dist, 2)
  
  proposals_by_prob <- c(sorted_words[ edit_dist <= min(edit_dist, 2)])
  remove(edit_dist)
    # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, word)
  res <- proposals_by_prob[1]
  remove(proposals_by_prob)
  res
}

