library(twitteR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(shiny)
library(tm)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$trump <- renderPlot({
    
    if (input$event == "First presidential debate"){
      setup_twitter_oauth(consumer_key = 'UP8lDP2w7ahKLGWGkgW9om38A',
                          consumer_secret = 's9fesLr08f7VDEeoxgOu3s48CSKchFhld1H7JSCZDiJiDzGSUr',
                          access_token = '823535222180483075-0PnQfgUDNOv8d5E3kZ5WdmcAJP1bknQ',
                          access_secret = 'UUqezwvDC7FFjY8cqGG2cmQpaCWzGFLIprtYOq2YrI6wK')
      
      myTweets <- searchTwitter('Donald Trump', n = 30, since = '2016-09-16',lang = "en")
      
      set.seed(1234) 
      tweetTexts<-unlist(lapply(myTweets, function(t) { t$text})) 
      words<-unlist(strsplit(tweetTexts, " "))
      
      clean_words<-words[-grep("http|@|#|ü|ä|ö", words)]
      clean_words <- iconv(clean_words, to = "latin1", sub = " ")
      clean_words_stem <- wordStem(clean_words)
      wordcloud(clean_words_stem, min.freq=2)
    }
    if (input$event == "Second presidential debate"){
      setup_twitter_oauth(consumer_key = 'UP8lDP2w7ahKLGWGkgW9om38A',
                          consumer_secret = 's9fesLr08f7VDEeoxgOu3s48CSKchFhld1H7JSCZDiJiDzGSUr',
                          access_token = '823535222180483075-0PnQfgUDNOv8d5E3kZ5WdmcAJP1bknQ',
                          access_secret = 'UUqezwvDC7FFjY8cqGG2cmQpaCWzGFLIprtYOq2YrI6wK')
      
      myTweets <- searchTwitter('Donald Trump', n = 30, since = '2016-10-04',lang = "en")
      
      set.seed(1234)
      tweetTexts<-unlist(lapply(myTweets, function(t) { t$text}))
      words<-unlist(strsplit(tweetTexts, " "))
      
      clean_words<-words[-grep("http|@|#|ü|ä|ö", words)] 
      clean_words <- iconv(clean_words, to = "latin1", sub = " ")
      clean_words_stem <- wordStem(clean_words)
      wordcloud(clean_words_stem, min.freq=2)
    }
    if (input$event == "Third presidential debate"){
      setup_twitter_oauth(consumer_key = 'UP8lDP2w7ahKLGWGkgW9om38A',
                          consumer_secret = 's9fesLr08f7VDEeoxgOu3s48CSKchFhld1H7JSCZDiJiDzGSUr',
                          access_token = '823535222180483075-0PnQfgUDNOv8d5E3kZ5WdmcAJP1bknQ',
                          access_secret = 'UUqezwvDC7FFjY8cqGG2cmQpaCWzGFLIprtYOq2YrI6wK')
      
      myTweets <- searchTwitter('Donald Trump', n = 30, since = '2016-10-14',lang = "en")
      
      set.seed(1234)
      tweetTexts<-unlist(lapply(myTweets, function(t) { t$text}))
      words<-unlist(strsplit(tweetTexts, " "))
      
      clean_words<-words[-grep("http|@|#|ü|ä|ö", words)] 
      clean_words <- iconv(clean_words, to = "latin1", sub = " ")
      clean_words_stem <- wordStem(clean_words)
      wordcloud(clean_words_stem, min.freq=2)
    }
    if (input$event == "Candidacy announced"){
      setup_twitter_oauth(consumer_key = 'UP8lDP2w7ahKLGWGkgW9om38A',
                          consumer_secret = 's9fesLr08f7VDEeoxgOu3s48CSKchFhld1H7JSCZDiJiDzGSUr',
                          access_token = '823535222180483075-0PnQfgUDNOv8d5E3kZ5WdmcAJP1bknQ',
                          access_secret = 'UUqezwvDC7FFjY8cqGG2cmQpaCWzGFLIprtYOq2YrI6wK')
      
      myTweets <- searchTwitter('Donald Trump', n = 30, since = '2015-6-16',lang = "en")
      
      set.seed(1234)
      tweetTexts<-unlist(lapply(myTweets, function(t) { t$text}))
      words<-unlist(strsplit(tweetTexts, " "))
      
      clean_words<-words[-grep("http|@|#|ü|ä|ö", words)] 
      clean_words <- iconv(clean_words, to = "latin1", sub = " ")
      clean_words_stem <- wordStem(clean_words)
      wordcloud(clean_words_stem, min.freq=2)
    }
    if (input$event == "Election Day"){
      setup_twitter_oauth(consumer_key = 'UP8lDP2w7ahKLGWGkgW9om38A',
                          consumer_secret = 's9fesLr08f7VDEeoxgOu3s48CSKchFhld1H7JSCZDiJiDzGSUr',
                          access_token = '823535222180483075-0PnQfgUDNOv8d5E3kZ5WdmcAJP1bknQ',
                          access_secret = 'UUqezwvDC7FFjY8cqGG2cmQpaCWzGFLIprtYOq2YrI6wK')
      
      myTweets <- searchTwitter('Donald Trump', n = 30, since = '2016-11-08',lang = "en")
      
      set.seed(1234)
      tweetTexts<-unlist(lapply(myTweets, function(t) { t$text}))
      words<-unlist(strsplit(tweetTexts, " "))
      
      clean_words<-words[-grep("http|@|#|ü|ä|ö", words)] 
      clean_words <- iconv(clean_words, to = "latin1", sub = " ")
      clean_words_stem <- wordStem(clean_words)
      wordcloud(clean_words_stem, min.freq=2)
    }
    
  })
  
})
