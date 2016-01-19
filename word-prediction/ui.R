library(shiny)
library(shinythemes)

shinyUI(
  fluidPage(
    theme=shinytheme("united"),
    headerPanel("Word Prediction"),
 
    mainPanel(
      h3('This Shiny app predicts the next word.'),
      textInput('inputWords', 'Enter your text in English language:'),
      submitButton("Submit"),
      h5('Your next word could be:'),
      verbatimTextOutput("nextWord")
    )
)
)
