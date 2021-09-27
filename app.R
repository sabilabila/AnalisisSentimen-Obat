library(dplyr)
library(vroom)
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(reshape2)
library(shiny)
library(tm)
library(memoise)

drugs = vroom(here("training_data.tsv"), delim = "\t")
option_urlDrugName = unique(drugs$urlDrugName)

count_reviews = function() {
  drugs %>%
    nrow()
}

count_sentiments = function(x) {
  drugs %>%
    unnest_tokens(word, commentsReview) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment) %>%
    filter(sentiment == x)
}

table_drugs = function() {
  drugs %>%
    mutate(effectiveness = case_when(
      effectiveness == "Highly Effective" ~ "Positive",
      effectiveness == "Marginally Effective" ~ "Netral",
      TRUE ~ "Negative"
    )) %>%
    select(urlDrugName,commentsReview,rating,effectiveness) %>%
    head(30)
}


# USER INTERFACE
ui = fluidPage(
  title = "Sentiment Analysis of Drugs Ranking Prediction App",
  headerPanel("Sentiment Analysis of Drugs Ranking Prediction App"),
  
  fluidRow(
    column(
      4,
      h3("Total Reviews"),
      h4(strong(textOutput(outputId = "total_reviews")))
    ),
    column(
      4,
      h3("Positive Words"),
      h4(strong(textOutput(outputId = "total_positive")))
    ),
    column(
      4,
      h3("Negative Words"),
      h4(strong(textOutput(outputId = "total_negative")))
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "urlDrugName",
        label = "urlDrugName of Drugs Type",
        choices = option_urlDrugName,
        multiple = TRUE,
        selected = option_urlDrugName[[1]]
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "plot_word_freq", height = "700px"),
      br(),
      plotlyOutput(outputId = "plot_word_usage", height = "700px"),
      h3("Words Cloud", align = "center"),
      plotOutput(outputId = "plot_word_cloud", height = "1200px"),
      h3("Table Reviews"),
      tableOutput(outputId = "plot_reviews")
    )
  )
)


# SERVER
server = function(input, output, session) {
  plot_word_freq = reactive({
    drugs %>% 
      group_by(urlDrugName) %>%
      unnest_tokens(word, commentsReview) %>%
      group_by(urlDrugName) %>%
      anti_join(stop_words) %>%
      count(word, sort = T) %>%
      na.omit() %>%
      filter(n >= 30) %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = urlDrugName)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Words",
        y = "Frequency",
        title = "Word Frequency Graphic"
      ) +
      theme_light()
  })
  
  output$plot_word_freq = renderPlotly({
    ggplotly(plot_word_freq())
  })
  
  plot_word_usage = reactive({
    drugs %>%
      filter(urlDrugName %in% input$urlDrugName) %>%
      unnest_tokens(word, commentsReview) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments("bing")) %>%
      group_by(sentiment, urlDrugName) %>%
      count(word) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(word, n), y = n, fill = urlDrugName)) +
      geom_col(show.legend = T) +
      coord_flip() +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(
        x = "Words",
        y = "Frequency",
        title = "Word Usage Graphic"
      ) +
      theme_light()
  })
  
  output$plot_word_usage = renderPlotly({
    ggplotly(plot_word_usage())
  })
  
  output$plot_word_cloud = renderPlot({
    drugs %>%
      filter(urlDrugName %in% input$urlDrugName) %>%
      unnest_tokens(word, commentsReview) %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment) %>% 
      acast(word~sentiment, value.var = "n", fill = 0) %>% 
      comparison.cloud(colors = c("#1b2a49", "#00909e"), max.words = 200, scale = c(4,1))
  })
  
  output$total_reviews = renderText({
    count_reviews()
  })
  
  output$total_positive = renderText({
    count_sentiments("positive")$n
  })
  
  output$total_negative = renderText({
    count_sentiments("negative")$n
  })
  
  output$plot_reviews = renderTable({
    table_drugs()
  })
}


# RUN APP
shinyApp(ui = ui, server = server)