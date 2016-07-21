#
# This is the user-interface definition of a Shiny web application predicting the next word
#

library(shiny)

# Define UI for application that predicts next word
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next word prediction"),
  h3('Application Description'),
  p('This is a sample application where an string is entered as input partially and submit to get the next word predicted'),
  # Sidebar with a text input for a string 
  sidebarLayout(
    sidebarPanel(
       textInput("inputId", "Enter a partial sentence", value = ""),
       submitButton('Submit')
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4('Next word predicted'), 
      verbatimTextOutput('wordprediction'),
      textOutput('text1')
      
      
    )
  )
))
