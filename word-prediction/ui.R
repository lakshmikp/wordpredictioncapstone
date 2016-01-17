library(shiny)
library(shinythemes)

shinyUI(
  fluidPage(
    theme=shinytheme("united"),
    headerPanel("Word Prediction"),
 
    mainPanel(
      h3('This Shiny app predicts the next word.'),
      textInput('inputWords', 'Enter some text in English language'),
      #actionButton("do", "Submit"),
      hr(),
      h2('The next word could be'),
      verbatimTextOutput("nextWord")
    )
)
)
