# LIBRARY

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)
library(tidytext)
library(forcats)
library(topicmodels)
library(wordcloud)
library(NLP)
library(tm)


# DATAFRAME STUFF

data <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//education_final.csv")
dataset <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//education_final.csv")
education_df <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//education_final.csv") 
pos_df <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//positive_words_final.csv")
neg_df <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//negative_words_final.csv")
df<-read.csv("C://Users//bianc//Desktop//Dashboard Strategic//education_final.csv", stringsAsFactors = FALSE)
data_patents<-df[20001:20848, ]
data_articoli <- head(data, 20000)
data_patents <- data[20001:20848, ]
data_tweets <- data[20848:nrow(data), ]


# QUESTION 2 STUFF

dataset_copia <- dataset

data_articoli = head(dataset, 20000)
data_patents = dataset[20001:20848, ]
data_tweets = dataset[20848:nrow(dataset), ]  
l_data <- c(data_articoli$Abstract, data_patents$Abstract, data_tweets$original_text)

# List of words to filter 
filter_words <- c("engineer", "medicine", "computer science", "literat", "philosophy", 'geogra', 'machine learning', 'econom', 'finance', 'law', 'chemist','physic')
co <- 1
eng = 0 
med = 0
cs = 0 
lit = 0
phi = 0
geo = 0
ml = 0
econom = 0
fin = 0
law = 0
chem = 0
phy = 0
lista <- list()
for (i in l_data){
  for (j in i){
    for(e in filter_words){
      if (grepl(e, j)){
        if(e == 'engineer'){
          eng = eng + 1
        }
        if(e == 'medicine'){
          med = med + 1
        }
        if(e == 'computer science'){
          cs = cs + 1
        }
        if(e == 'literature'){
          lit = lit + 1
        }
        if(e == 'philosophy'){
          phi = phi + 1
        }
        if(e == 'geogra'){
          geo = geo + 1
        }
        if(e == 'machine learning'){
          ml = ml + 1
        }
        if(e == 'economy'){
          econom = econom + 1
        }
        if(e == 'finance'){
          fin = fin + 1
        }
        if(e == 'law'){
          law = law + 1
        }
        if(e == 'chemist'){
          chem = chem + 1
        }
        if(e == 'physic'){
          phy = phy + 1
        }
        
      }
    }
  }
}


lista_var <- list(c("engineer", "medicine", "computer science", "literat", "philosophy", 'geogra', 'machine learning', 'econom', 'finance', 'law', 'chemist','physic'))
lista_value <- list(c(eng ,med,cs ,lit,phi,geo,ml,econom,fin,law,chem,phy))
dataframe <- data.frame( lista_var, lista_value)
colnames(dataframe) <- c("ambito", "valore")
dataframe <- dataframe[order(dataframe$valore, decreasing = TRUE), ]

# Select the top 10 variables
top_10_data <- head(dataframe, 10)


#  QUESTION 3 STUFF

university_count <- 0
high_school_count <- 0
secondary_school_count <- 0
primary_school_count <- 0

update_counters <- function(text) {
  if (!is.na(text)) {
    if (grepl("university", text, ignore.case = TRUE)) {
      university_count <<- university_count + 1
    }
    if (grepl("college", text, ignore.case = TRUE)) {
      university_count <<- university_count + 1
    }
    if (grepl("high school", text, ignore.case = TRUE)) {
      high_school_count <<- high_school_count + 1
    }
    if (grepl("secondary school", text, ignore.case = TRUE)) {
      secondary_school_count <<- secondary_school_count + 1
    }
    if (grepl("middle school", text, ignore.case = TRUE)) {
      secondary_school_count <<- secondary_school_count + 1
    }
    if (grepl("primary school", text, ignore.case = TRUE)) {
      primary_school_count <<- primary_school_count + 1
    }
  }
}

for (i in 1:nrow(education_df)) {
  update_counters(education_df[i, "Abstract"])  
  update_counters(education_df[i, "original_text"]) 
}


results_domanda3 <- data.frame(parola = c("university", "high school", "secondary school", "primary school"),
                               conteggio = c(university_count, high_school_count,  secondary_school_count, primary_school_count))



#  QUESTION 5 STUFF

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus,
                   content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,
                   c(stopwords("en"), "education","chatgpt","will","can"))
  return(corpus)
}



topic_modeling<-function(df,column_name){
  column <- df[[column_name]]
  edu_source <- VectorSource(column)
  corpus <- Corpus(edu_source)
  minimumFrequency <- 5
  DTM <- DocumentTermMatrix(clean_corpus(corpus), control = list(bounds = list(global = c(minimumFrequency, Inf))))
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  # number of topics
  K <- 5
  set.seed(9161)
  topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25)) %>% tidy(matrix = "beta")
  
  word_probs <- topicModel %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>%
    ungroup() %>%
    mutate(term2 = fct_reorder(term, beta))
  plot<-ggplot(word_probs,aes(term2,beta,fill = as.factor(topic))) + geom_col( show.legend =FALSE)+facet_wrap(~topic,scales="free")+coord_flip() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
  return (plot)
}

l_data <- c(data_articoli$Abstract, data_patents$Abstract, data_tweets$original_text)

topic_modeling_new<-function(df){
  
  edu_source <- VectorSource(df)
  corpus <- Corpus(edu_source)
  minimumFrequency <- 5
  DTM <- DocumentTermMatrix(clean_corpus(corpus), control = list(bounds = list(global = c(minimumFrequency, Inf))))
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  # number of topics
  K <- 5
  # set random number generator seed
  set.seed(9161)
  topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 150, verbose = 25)) %>% tidy(matrix = "beta")
  
  word_probs <- topicModel %>%
    group_by(topic) %>%
    slice_max(beta, n = 15) %>%
    ungroup() %>%
    mutate(term2 = fct_reorder(term, beta))
  plot<-ggplot(word_probs,aes(term2,beta,fill = as.factor(topic))) + geom_col( show.legend =FALSE)+facet_wrap(~topic,scales="free")+coord_flip()+
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )
  return (plot)
}

D1='1) What are the main players ?'
D2='2) In which areas (medicine, engineering, physics, etc.) is generativeAI most commonly used ?'
D3='3) In which education level(primary school, middle school, high school, college, etc.) is generativeAI mainly used ?'
D4='4) Has the introduction of Generative AI had negative or positive effects on teaching ?'
D5='5) How are most widely used genAI technologies in the academic field?'

##################  SHINY APP ##################


ui <- fluidPage(
  titlePanel("S4CI - GenerativeAI usage in the Education field"),
  sidebarPanel(
    selectInput("question", "Choose a question:", choices = unique(c(D1,D2,D3,D4,D5)))
  ),
  mainPanel(
    uiOutput("tab_content")
  )
)

server <- function(input, output, session) {
  
  # Tabella di corrispondenza tra tab e domande
  tab_question_mapping <- data.frame(
    tab = c("Answer 1", "Answer 2", "Answer 3", "Answer 4", "Answer 5"),
    #question = c("D1", "D2", "D3", "D4", "D5")
    question = c(D1,D2,D3,D4,D5)
  )
  
  # Funzione per generare dinamicamente i pannelli tab
  output$tab_content <- renderUI({
    selected_tab <- tab_question_mapping$tab[tab_question_mapping$question == input$question]
    tabsetPanel(
      id = "tabset",
      selected = selected_tab,
      tabPanel("Answer 1", 
               fluidRow(
                 column(12, h4("Top 10 Player in GenAi field", style = "font-weight:bold;")),
                 column(6, tableOutput('plot1')),
                 column(6, tableOutput('plot14'))
               )
      ),
      tabPanel("Answer 2", plotOutput('plot2')),
      tabPanel("Answer 3",
               fluidRow(
                 column(12, h4("Interest in GenAI per education level", style = "font-weight:bold;")),
                 column(6,plotOutput('plot3')),
                 column(6,plotOutput('plot31'))
               )
      ),
      tabPanel("Answer 4", fluidRow(
        column(4, plotOutput('plot41')),
        column(4, plotOutput('plot42')),
        column(4, plotOutput('plot43'))
      )),
      tabPanel("Answer 5",
               fluidRow(
                 column(12, h4("Topic modeling ", style = "font-weight:bold;")),
                 column(6,plotOutput('plot5'))
               )
               )
    )
  })
  
  # Funzione per aggiornare la domanda in base al tab selezionato
  observeEvent(input$tabset, {
    selected_question <- tab_question_mapping$question[tab_question_mapping$tab == input$tabset]
    updateSelectInput(session, inputId = "question", selected = selected_question)
  })
  
#################################### TAB 1 ####################################
  
  ### First Table
  
  # Funzione per mostrare i top 10 Affiliations per frequenza [ ARTICLES(SCOPUS) ]
  output$plot1 <- renderTable({
    
    # Calcolo i top 10 Affiliations
    affiliations_frequency <- table(data_articoli$Affiliations)
    top_10_affiliations <- sort(affiliations_frequency, decreasing = TRUE)[1:11]
    top_10_affiliations_df <- data.frame(Affiliation = names(top_10_affiliations),
                                         Frequency = as.numeric(top_10_affiliations))
    
    top_10_affiliations_df$Country <- str_extract(top_10_affiliations_df$Affiliation, "(?<=,\\s)([^,]+)$")
    colnames(top_10_affiliations_df) <- c("Player (Scopus Articles)", "Frequency", "Country")
    
    top_10_affiliations_df <- top_10_affiliations_df[order(-top_10_affiliations_df$`Frequency`), ]
    top_10_affiliations_df <- top_10_affiliations_df[-1, ]
    
    top_10_affiliations_df <- mutate(top_10_affiliations_df, Type = "Affiliation")
    
    # Calcolo i top 10 Publisher
    publisher_frequency <- table(data_articoli$Publisher)
    top_10_publisher <- sort(publisher_frequency, decreasing = TRUE)[1:11]
    top_10_publisher_df <- data.frame(Publisher = names(top_10_publisher),
                                      Frequency = as.numeric(top_10_publisher))
    
    top_10_publisher_df$Country <- 'da riempire'
    colnames(top_10_publisher_df) <- c("Player (Scopus Articles)", "Frequency", "Country")
    
    top_10_publisher_df <- top_10_publisher_df[order(-top_10_publisher_df$`Frequency`), ]
    top_10_publisher_df <- top_10_publisher_df[-1, ]
    
    top_10_publisher_df <- mutate(top_10_publisher_df, Type = "Publisher")
    
    
    # Faccio il merge
    merged_df0 <- bind_rows(top_10_affiliations_df, top_10_publisher_df)
    merged_df0 <- merged_df0[order(-merged_df0$Frequency), ]
    merged_df0 <- head(merged_df0, 10)
    
    country_4merge0 = c('Germany','Germany','United States','United Kingdom','Netherlands','Netherlands','United States','United States','Switzerland','United Kingdom')
    merged_df0$Country <- country_4merge0
    colnames(merged_df0) <- c("Player (Scopus Articles)", "No.Articles", "Country","Type")
    merged_df0$No.Articles <- as.integer(merged_df0$No.Articles)
    return(merged_df0)
    
    
  })
  
  
  ### Second Table
  
  output$plot14 <- renderTable({
    
    # Calcolo i top 10 Applicants
    applicants_frequency <- table(data_patents$Applicants)
    top_10_applicants <- sort(applicants_frequency, decreasing = TRUE)[1:11]
    top_10_applicants_df <- data.frame(Applicant = names(top_10_applicants),
                                       Frequency = as.numeric(top_10_applicants))
    
    top_10_applicants_df$Country <- sapply(top_10_applicants_df$Applicant, function(x) {
      jurisdiction <- data_patents$Jurisdiction[data_patents$Applicants == x]
      return(jurisdiction[1])
    })
    
    top_10_applicants_df <- mutate(top_10_applicants_df, Type = "Applicant")
    colnames(top_10_applicants_df) <- c("Player (Lens patents)", "Frequency", "Country", "Type")
    
    # Calcolo i top 10 Inventors
    inventors_frequency <- table(data_patents$Inventors)
    top_10_inventors <- sort(inventors_frequency, decreasing = TRUE)[1:11]
    top_10_inventors_df <- data.frame(Inventor = names(top_10_inventors),
                                      Frequency = as.numeric(top_10_inventors))
    top_10_inventors_df$Country <- sapply(top_10_inventors_df$Inventor, function(x) {
      jurisdiction <- data_patents$Jurisdiction[data_patents$Inventors == x]
      return(jurisdiction[1])
    })
    
    
    top_10_inventors_df <- mutate(top_10_inventors_df, Type = "Inventor")
    colnames(top_10_inventors_df) <- c("Player (Lens patents)", "Frequency", "Country", "Type")
    
    # Faccio il merge
    merged_df <- bind_rows(top_10_applicants_df, top_10_inventors_df)
    merged_df <- merged_df[order(-merged_df$Frequency), ]
    merged_df <- head(merged_df, 10)
    
    country_4merge = c('China','China','World Intellectual Property Organisation (WIPO)','China','China','Korea','Korea','China','Korea','China')
    merged_df$Country <- country_4merge
    colnames(merged_df) <- c("Player (Lens Patents)", "No.Patents", "Country","Type")
    merged_df$No.Patents <- as.integer(merged_df$No.Patents)
    return(merged_df)
  })
  
  
  
  
  
  
#################################### TAB 2 ####################################
  
  
  
  
  

  
  output$plot2 <- renderPlot({
    top_10_data <- top_10_data %>%
      mutate(ambito = recode(ambito, 'chemist' = 'chemistry', 'physic' = 'physics','geogra' = 'geography')) %>%
      rename(value = valore)
    
    ggplot(top_10_data, aes(x = reorder(ambito, -value), y = value, fill = value)) +
      geom_bar(stat = "identity") +
      labs(x = "Fields", y = "Occurencies", title = "Top 10 Education Gen AI fields") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"),  
            axis.title.x = element_text(size = 16),  
            axis.title.y = element_text(size = 16),  
            axis.text.x = element_text(size = 14),  
            legend.text = element_text(size = 14)) +  
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  
  }, width = 800, height = 600)
  
  
  
  
  
  
  
  
  
#################################### TAB 3 ####################################
  
  
  
  
  output$plot3 <- renderPlot({
    
    ggplot(results_domanda3, aes(x = "", y = conteggio, fill = parola)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(conteggio / sum(conteggio) * 100, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5) + 
      labs(x = NULL, y = NULL, fill = "Education Level") +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 14))  
  }, width = 800, height = 800)  
  
  
 # output$plot31 <- renderPlot({
 #    topic_modeling_new(l_data)
 #  })
 
  
  
  
  
#################################### TAB 4 ####################################
  
  
  ### First Plot
  output$plot41 = renderPlot({
    
    data <- read.csv("C://Users//bianc//Desktop//Dashboard Strategic//educationFinal_sentiment.csv")
    data_articoli <- head(data, 20000)
    data_patents <- data[20001:20848, ]
    data_tweets <- data[20848:nrow(data),]
    
    sentiment_df <- data
    posCounter <- sum(sentiment_df$sentiment=="positive")
    negCounter <- sum(sentiment_df$sentiment=="negative")
    
    count_df <- data.frame(
      sentiment = c("Positive", "Negative"),
      conteggio = c( posCounter, negCounter)
    )
    
    
    
    
    ggplot(count_df, aes(x = "", y = conteggio, fill = sentiment)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(conteggio / sum(conteggio) * 100, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5) +
      labs(x = NULL, y = NULL, fill = "Education Level") +
      ggtitle("Opinions about GenAI") +
      theme_void() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 14))  
  }, width = 300, height = 300)
  
    
  ### Second Plot
  output$plot42 = renderPlot({
    wordcloud(neg_df$lemma_parola, neg_df$X, max.words = 50)
  })
  
  
  
  ### Third Plot
  output$plot43 = renderPlot({
    wordcloud(pos_df$lemma_parola, pos_df$X, max.words = 50)
  })
  
  
#################################### TAB 5 ####################################
  
  output$plot5 = renderPlot({
    topic_modeling(data_patents,"Title")
  })
  
  
}


app<-shinyApp(ui, server)
runApp(app, port = 7000)
