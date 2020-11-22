#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(tidyverse)
library(scales)
library(readr)
library(wordcloud2)
library(tidytext)
library(gameofthrones)

# load the data
got_scripts <- read_csv("Game_of_Thrones_Script.csv")

# breakup the sentences into words
got_words <- got_scripts %>% 
    unnest_tokens(word, Sentence)

# get rid of common stop words (they are not interesting)
data(stop_words)
tidy_got_words <- got_words %>%
    anti_join(stop_words)

# define the possible character name inputs
characters <- unique(tidy_got_words$Name)
characters <- str_to_title(characters) # make the first letters upper case
characters <- sort(characters) # sort alphabetically

# Define UI for application that creates a wordcloud
ui <- fluidPage(

    # Application title
    titlePanel("Game of Thrones Wordcloud"),

    # Sidebar with a select input for the name of the character
    sidebarLayout(
        sidebarPanel(
            selectInput("character",
                        "Choose a Character:",
                        choices = characters),
            selectInput("shape",
                        "Choose the Shape of the Wordcloud:",
                        choices = c('circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', 'star')),
            selectInput("pal",
                        "Choose a Color Palette:",
                        choices = c('Targaryen', 'Targaryen2', 'Stark', 'Stark2', 'Lannister', 'Martell', 'Tully', 'Greyjoy', 'Baratheon', 'Baratheon2', 'Tyrell', 'White_Walkers', 'Jon_Snow', 'Margaery', 'Daenerys', 'Game_of_Thrones', 'Wildfire', 'Arya')),
        ),

        # Show a plot of the generated wordcloud
        mainPanel(
           wordcloud2Output("wordcloud2", width = "100%", height = "1000px")
        )
    )
)

# Define server logic required to draw the wordcloud
server <- function(input, output) {
    
    wordcloud <- reactive({
        
        res <- tidy_got_words %>%
            filter(Name == str_to_lower(input$character)) %>%
            count(word, Name) %>%
            select(-Name)
        
        res
    })
    
    pal <- reactive({
        
        res <- got(length(wordcloud()$word), option = input$pal) # create a unique color for each word according to the GoT palette
        
        res
    })
    
    output$wordcloud2 <- renderWordcloud2({
        
        # draw the wordcloud for the specified character
        wordcloud2(wordcloud(), size = 0.7, shape = input$shape, color = pal())

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
