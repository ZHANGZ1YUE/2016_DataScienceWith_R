library(twitteR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Donald Trump Wordcloud"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("event", "Which event do you want to select: ", 
                  choices = c("Candidacy announced", 
                              "First presidential debate", 
                              "Second presidential debate", 
                              "Third presidential debate",
                              "Election Day"),
                  selected = "Candidacy announced"
      )
    ),
    
    mainPanel(
       plotOutput("trump")
    )
  )
))
