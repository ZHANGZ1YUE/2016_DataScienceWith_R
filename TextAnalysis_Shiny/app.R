library(syuzhet)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(SnowballC)
library(gridExtra)
library(stringr)
library(readr)
library(plotly)
library(tidytext)
library(tidyverse)
library(tm)
library(qdap)
library(gridExtra)
library(shinyjs)


ui <- fixedPage(fixedRow(
  useShinyjs(),
  column(
    6,
    fileInput(
      "file1",
      "Choose CSV File",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    h3("Top 7 words in the text"),
    actionButton("button", "Click me for explanation"),
    textInput("text", "A polar plot of the top seven words used in the text. Stopwords are removed from the list."),
    plotOutput("wordfrequency"),
    h3("Sentence Length Statistic"),
    actionButton("button1", "Click me for explanation"),
    textInput("text1", "The x-axis is the No. of sentence, and y-axis is the word count for each sentence. In order to avoid overlap caused by huge text with too many sentences, a smooth curve is used to display the general trend."),
    plotOutput("SentenceLength"),
    h3("Syllable Count"),
    actionButton("button2", "Click me for explanation"),
    textInput("text2","The table below is includes the frequency and syllable count for all the words used in the text, including stopwords and names. The default sequence is the order of word frequency from the most to the least, click on syllable can change the sequence. To search a particular word, type the word in Search."),
    dataTableOutput("Syllable"),
    h3("Puncuation Frequency"),
    actionButton("button3", "Click me for explanation"),
    textInput("text3", "This is a table of puncuation use and count."),
    tableOutput("Punctuation"),
    h3("Use of Comma"),
    actionButton("button4", "Click me for explanation"),
    textInput("text4", "The plots are the combination of different comma counts in the text. The left plot displays which amount of comma used in a single sentence is the most popular one, while the right plot is the actual count of comma use in each sentence. "),
    plotOutput("com"),
    tableOutput("test")
  ),
  column(6,
         fileInput(
           "file2",
           "Choose CSV File",
           accept = c("text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
         ),
         h3("Top 7 words in the text"),
         actionButton("button5", "Click me for explanation"),
         textInput("text5", "A polar plot of the top seven words used in the text. Stopwords are removed from the list."),
         plotOutput("wordfrequency2"),
         h3("Sentence Length Statistic"),
         actionButton("button6", "Click me for explanation"),
         textInput("text6", "The x-axis is the No. of sentence, and y-axis is the word count for each sentence. In order to avoid overlap caused by huge text with too many sentences, a smooth curve is used to display the general trend."),
         plotOutput("SentenceLength2"),
         h3("Syllable Count"),
         actionButton("button7", "Click me for explanation"),
         textInput("text7","The table below is includes the frequency and syllable count for all the words used in the text, including stopwords and names. The default sequence is the order of word frequency from the most to the least, click on syllable can change the sequence. To search a particular word, type the word in Search."),
         dataTableOutput("Syllable2"),
         h3("Puncuation Frequency"),
         actionButton("button8", "Click me for explanation"),
         textInput("text8", "This is a table of puncuation use and count."),
         tableOutput("Punctuation2"),
         h3("Use of Comma"),
         actionButton("button9", "Click me for explanation"),
         textInput("text9", "The plots are the combination of different comma counts in the text. The left plot displays which amount of comma used in a single sentence is the most popular one, while the right plot is the actual count of comma use in each sentence. "),
         plotOutput("com2"),
         tableOutput("test2")
         )
))

server <- function(input, output) {
  #browser()
  #This function is repsonsible for loading in the selected file
  content <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read_file(infile$datapath)
  })
  
  content2 <- reactive({
    infile2 <- input$file2
    if (is.null(infile2)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read_file(infile2$datapath)
  })

  output$wordfrequency <- renderPlot({
    cleancont <- rm_stopwords(content(), Top200Words)
    frequency <- all_words(cleancont, "")
    print("im here")
    freqss<- frequency
    freq_s<- freqss%>%
      group_by(FREQ)
    freq_ss <- arrange(freq_s, desc(FREQ))%>%
      head(7)
    bar<- ggplot(freq_ss)+
      geom_bar(aes(x = WORD, y = FREQ, fill = WORD), stat = "identity")+
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL)
    bark<- bar + coord_polar()
    bark
  })


  output$SentenceLength <- renderPlot({
    dfsent <- tibble(Sent = get_sentences(content()),
                     Number = seq_along(Sent))
    df_sentencelength <- dfsent %>%
      mutate(`Sentence length` = wc(dfsent$Sent))
    head(df_sentencelength)
    ggplot_sentence_length <- ggplot(df_sentencelength) +
      geom_line(aes(x = Number, y = `Sentence length`))+
      geom_smooth(aes(x = Number, y = `Sentence length`))+
      xlab("Number of Sentence")
    ggplot_sentence_length
  })
  

  
  output$Syllable <- renderDataTable({
    word <- str_split(content(), boundary("word"))[[1]]
    several <- sort(table(word), decreasing = T)
    syl <- as.data.frame(several) %>%
      mutate(syllable = syllable_sum(word))
    syl
    },
    options = list(pageLength = 5))

  output$Punctuation <- renderTable(
    tibble(
      Comma = str_count(content(), ","),
      Semicolons = str_count(content(), ";"),
      Colons = str_count(content(), ":"),
      Dashes = str_count(content(), "-"),
      QuestionMarks = sum(gregexpr("[?]", content())[[1]] > 0),
      Exclamation = sum(gregexpr("[!]", content())[[1]] > 0)
    )
  )
  
  output$com <- renderPlot({
    
    dfsent <- tibble(
      Sent = get_sentences(content()),
      Number = seq_along(Sent)
    )
    
    df_sentencelength <- dfsent %>%
      mutate(`Sentence length` = wc(dfsent$Sent))
    
    CommaSentence <- dfsent%>%
      select(Sent)
    CommaSent <- tibble(
      Sent = get_sentences(content()),
      Number = seq_along(Sent),
      Comma = str_count(CommaSentence[[1]], ",")
    )
    
    Rel_Com <- CommaSent%>%
      full_join(df_sentencelength)
    
    Com<- ggplot(Rel_Com)+
      geom_point(aes(x = Number, y = Comma/`Sentence length`))+
      xlab("Number of sentence")+
      ylab("Number of Comma")
    
    Com2 <- ggplot(CommaSent) +
      geom_bar(aes(x = Comma, stat = "identity"))+
      xlab("Number of Comma in a sentence")
    
    grid.arrange(Com2, Com, ncol = 2)
    
    
  })
  
  
  
  output$test <- renderTable({
    dfsent <- tibble(
      Sent = get_sentences(content()),
      Number = seq_along(Sent)
    )
    CommaSentence <- dfsent%>%
      select(Sent)
    CommaSent <- tibble(
      Sent = get_sentences(content()),
      Number = seq_along(Sent),
      Comma = str_count(CommaSentence[[1]], ",")
    )
    tibble(
    mean = mean(str_count(CommaSentence[[1]], ",")),
    s.d. = sd(str_count(CommaSentence[[1]], ",")),
    p_value = t.test(str_count(CommaSentence[[1]], ","))$p.value,
    t_value = t.test(str_count(CommaSentence[[1]], ","))$statistic
    )
  })
  

  
  output$wordfrequency2 <- renderPlot({
    cleancont <- rm_stopwords(content2(), Top200Words)
    frequency <- all_words(cleancont, "")
    print("im here")
    freqss<- frequency
    freq_s<- freqss%>%
      group_by(FREQ)
    freq_ss <- arrange(freq_s, desc(FREQ))%>%
      head(7)
    bar<- ggplot(freq_ss)+
      geom_bar(aes(x = WORD, y = FREQ, fill = WORD), stat = "identity")+
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL)
    bark<- bar + coord_polar()
    bark
  })
  
  
  output$SentenceLength2 <- renderPlot({
    print("sdf")
    print(content2())
    dfsent <- tibble(Sent = get_sentences(content2()),
                     Number = seq_along(Sent))
    df_sentencelength <- dfsent %>%
      mutate(`Sentence length` = wc(dfsent$Sent))
    head(df_sentencelength)
    ggplot_sentence_length <- ggplot(df_sentencelength) +
      geom_line(aes(x = Number, y = `Sentence length`))+
      geom_smooth(aes(x = Number, y = `Sentence length`))+
      xlab("Number of Sentence")
    ggplot_sentence_length
  })
  
  
  
  output$Syllable2 <- renderDataTable({
    word <- str_split(content2(), boundary("word"))[[1]]
    several <- sort(table(word), decreasing = T)
    syl <- as.data.frame(several) %>%
      mutate(syllable = syllable_sum(word))
    syl
  },
  options = list(pageLength = 5))
  
  output$Punctuation2 <- renderTable(
    tibble(
      Comma = str_count(content2(), ","),
      Semicolons = str_count(content2(), ";"),
      Colons = str_count(content2(), ":"),
      Dashes = str_count(content2(), "-"),
      QuestionMarks = sum(gregexpr("[?]", content2())[[1]] > 0),
      Exclamation = sum(gregexpr("[!]", content2())[[1]] > 0)
    ))
  
  output$com2 <- renderPlot({
    
    dfsent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent)
    )
    
    df_sentencelength <- dfsent %>%
      mutate(`Sentence length` = wc(dfsent$Sent))
    
    CommaSentence <- dfsent%>%
      select(Sent)
    CommaSent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent),
      Comma = str_count(CommaSentence[[1]], ",")
    )
    
    Rel_Com <- CommaSent%>%
      full_join(df_sentencelength)
    
    Com<- ggplot(Rel_Com)+
      geom_point(aes(x = Number, y = Comma/`Sentence length`))+
      xlab("Number of sentence")+
      ylab("Number of Comma")
    
    Com2 <- ggplot(CommaSent) +
      geom_bar(aes(x = Comma, stat = "identity"))+
      xlab("Number of Comma in a sentence")
    
    grid.arrange(Com2, Com, ncol = 2)
  })
  
  
  
  output$rel_com_2 <- renderPlot({

    dfsent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent)
    )
    CommaSentence <- dfsent%>%
      select(Sent)
    CommaSent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent),
      Comma = str_count(CommaSentence[[1]], ",")
    )
    Com<- ggplot(CommaSent)+
      geom_point(aes(x = Number, y = Comma))
    Com
    
  })
  
  output$test2 <- renderTable({
    dfsent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent)
    )
    CommaSentence <- dfsent%>%
      select(Sent)
    CommaSent <- tibble(
      Sent = get_sentences(content2()),
      Number = seq_along(Sent),
      Comma = str_count(CommaSentence[[1]], ",")
    )
    tibble(
      mean = mean(str_count(CommaSentence[[1]], ",")),
      s.d. = sd(str_count(CommaSentence[[1]], ",")),
      p_value = t.test(str_count(CommaSentence[[1]], ","))$p.value,
      t_value = t.test(str_count(CommaSentence[[1]], ","))$statistic
    )
  })
  
  observeEvent(input$button, {
    toggle("text")
  })
  observeEvent(input$button1, {
    toggle("text1")
  })
  observeEvent(input$button2, {
    toggle("text2")
  })
  observeEvent(input$button3, {
    toggle("text3")
  })
  observeEvent(input$button4, {
    toggle("text4")
  })
  
  observeEvent(input$button5, {
    toggle("text5")
  })
  observeEvent(input$button6, {
    toggle("text6")
  })
  observeEvent(input$button7, {
    toggle("text7")
  })
  observeEvent(input$button8, {
    toggle("text8")
  })
  observeEvent(input$button9, {
    toggle("text9")
  })
}

shinyApp(ui, server)
