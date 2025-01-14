# Title: Text Analysis of Harry Potter Books
# Description: Analysis 1: Change of Sentiments Throughout Each Book
#              Analysis 2: Bigrams
# Details: 4 input widgets and 2 output tabs
# Author: Yihang Luo
# Date: 11/14/2024


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()



# ===============================================
# Import data
# ===============================================
hp_books = read_csv(file = "harry_potter_books.csv", col_types = "ccc")

book_names = hp_books$book


# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis of Harry Potter Books"),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  fluidRow(
    # replace with your widgets
    column(2,
           p(em("Analysis 1 & 2")),
           selectInput(inputId = "selected_book", 
                       label = "Select a book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), # closes column 1
    
    # replace with your widgets
    column(2,
           p(em("Analysis 1")),
           radioButtons(inputId = "lexicon", 
                        label = "Select Sentiment Lexicon:", 
                        choices = c("bing.csv", "afinn.csv", "nrc.csv", "loughran.csv"))
    ), # closes column 2
    
    # replace with your widgets
    column(2,
           p(em("Analysis 2")),
           sliderInput(inputId = "top_n", 
                       label = "Top n words",
                       min = 1,
                       max = 30,
                       value = 10)
    ), # closes column 3
    
    # replace with your widgets
    column(2,
           p(em("Analysis 2")),
           textInput(inputId = "word",
                         label = "Select a word for bigram",
                         value = "harry")
    ), # closes column 4
    
    column(2,
           p(em("Analysis 2")),
           radioButtons(inputId = "stopwords", 
                        label = "Stopwords", 
                        choices = c("use all tokens" = "opt1",
                                    "remove stopwords" = "opt2"), 
                        selected = "opt1")
    ) # closes column 5
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Change of Sentiments Throughout Each Book"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Bigrams"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. dummy data frame to be used in plot1)
  hp_books_filter <- reactive({
    if (input$selected_book == "All books") {
      hp_books
    } else {
      hp_books |>
      filter(book == input$selected_book)
    }
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  
  # tokenize
  cos_tokens = reactive({
    hp_books_filter() |>
      group_by(book) |>
      mutate(
        linenumber = row_number(),
        chapter = cumsum(
          str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) |>
      ungroup() |>
      unnest_tokens(word, text)
  })
  
  # choose a lexicon
  cos_sentiments = reactive({
    if (input$lexicon != "afinn.csv") {
      cos_tokens() |>
        inner_join(read_csv(input$lexicon), by = "word", relationship = "many-to-many") |>
        filter(sentiment %in% c("positive", "negative")) |>
        count(book, index = linenumber %/% 80, sentiment) |>
        pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
        mutate(sentiment = positive - negative)
    } else {
      cos_tokens() |>
        inner_join(read_csv(input$lexicon), by = "word") |>
        group_by(book, index = linenumber %/% 80) |>
        summarise(sentiment = sum(value), .groups='drop')
    }
  })
  
  output$plot1 <- renderPlot({
    ggplot(data = cos_sentiments(),
           aes(x = index, y = sentiment, fill=book)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ book, ncol = 2, scales = "free_x") +
      theme_bw() +
      labs(title = paste0("Change of Sentiment throughout ", input$selected_book),
           subtitle = paste0("Lexicon: ", input$lexicon)) +
      theme_bw()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    cos_sentiments()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # create a bigram with stop words removed
  hp_bigrams = reactive({
    hp_books_filter() |>
      unnest_tokens(
        output = bigram, 
        input = text, 
        token = "ngrams", 
        n = 2)
  }) 
  
  bigrams_filtered <- reactive({
    # Should stopwords be removed?
    if (input$stopwords == "opt2") {
      hp_bigrams() |>
        separate(bigram, c("word1", "word2"), sep = " ") |>
        filter(!word1 %in% stop_words$word) |>
        filter(!word2 %in% stop_words$word) |>
        unite(bigram, word1, word2, sep = " ")
    } else {
      hp_bigrams()
    }
  })
  
  
  # search for word
  word_bigrams = reactive({
    bigrams_filtered() |>
    filter(str_detect(bigram, input$word)) |>
    count(bigram, sort = TRUE, name = "count") |>
    slice_head(n = input$top_n)
  }) 
  
  # code for plot2
  output$plot2 <- renderPlot({
    word_bigrams() |>
      ggplot(aes(x = count, y = reorder(bigram, count))) +
      geom_col(fill="skyblue") +
      labs(title = paste0("Top-", as.character(input$top_n), " bigrams containing word '", input$word, "'"),
           y = "bigram") +
      theme_bw()
  })
  
  # code for statistics
  output$table2 <- renderDataTable({
    word_bigrams()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

