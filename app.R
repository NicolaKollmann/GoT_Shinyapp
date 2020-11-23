#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(tidyverse)
library(readr)
library(wordcloud2)
library(tidytext)
library(gameofthrones)
library(shinythemes)
library(colourpicker)
library(gitlink)

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
ui <- fluidPage(theme = shinytheme("darkly"),
    
    # add the GitHub ribbon
    ribbon_css("https://github.com/NicolaKollmann/Shinyapp", color = "#6edfb0", link_css = list("font-family" = "GoT", "font-size" = "11px")),
    
    # use the GoT font Trajan Pro
    tags$head(
        includeCSS("www/CSS.css")
    ),
                                        
    # Application title
    titlePanel(p(class = "p_Style", "Game of Thrones Wordcloud"), windowTitle = "Game of Thrones Wordcloud - Nicola Kollmann"),
    h4(p(class = "p_Style", tags$a(href = "https://nkollmann.netlify.app/", "Nicola Kollmann"))),

    # Sidebar with a select input for the name of the character
    sidebarLayout(
        sidebarPanel(
            div(selectInput("character",
                        "Choose a Character:",
                        choices = characters), class = "p_Style"),
            hr(),
            div(selectInput("shape",
                        "Choose the Shape of the Wordcloud:",
                        choices = c('circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', 'star')), class = "p_Style"),
            hr(),
            div(selectInput("pal",
                        "Choose a Color Palette:",
                        choices = c('Targaryen', 'Targaryen2', 'Stark', 'Stark2', 'Lannister', 'Martell', 'Tully', 'Greyjoy', 'Baratheon', 'Baratheon2', 'Tyrell', 'White_Walkers', 'Jon_Snow', 'Margaery', 'Daenerys', 'Game_of_Thrones', 'Wildfire', 'Arya')), class = "p_Style"),
            hr(),
            div(colourInput("col", p(class = "p_Style", "Background color"), value = "white"), class = "p_Style"),
            hr(),
            
            # GoT Theme Song
            tags$audio(src = "GoT_Theme.mp3", type = "audio/mp3", autoplay = T, controls = NA),
        ),

        # Show a plot of the generated wordcloud
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel(p(class = "p_Style","Wordcloud"),
                                 wordcloud2Output("wordcloud2", width = "100%", height = "900")),
                        tabPanel(p(class = "p_Style", "Credits"),
                                 br(),
                                 p(class = "p_Style", "Data: ", a(href="https://www.kaggle.com/albenft/game-of-thrones-script-all-seasons", "Kaggle")),
                                 p(class = "p_Style", "GoT Theme Song: ", a(href="https://mp3offline.org/uXZd_W5B7N0", "mp3offline.org")),
                                 p(class = "p_Style", "GoT Color Palettes: ", a(href="https://github.com/aljrico/gameofthrones", "Alejandro Jiménez")),
                                 p(class = "p_Style", "Wordcloud2: ", a(href="https://github.com/Lchiffon/wordcloud2", "Dawei Lang")),
                                 p(class = "p_Style", "GitHub Corner: ", a(href="https://github.com/colearendt/gitlink", "Cole Arendt")),
                                 p(class = "p_Style", "Font: ", a(href="https://fontsgeek.com/fonts/Trajan-Pro-Regular", "Trajan Pro")),
                                 strong(class = "p_Style", "Thank you very much!"),
                                 hr(),
                                 tags$iframe(src = "https://giphy.com/gifs/tk8aCAvTg8Hjq/html5", width = "300", height = "229")
                                 ),
           hr()
            )
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
        wordcloud2(wordcloud(), fontFamily = 'GoT', size = 0.7, shape = input$shape, color = pal(), backgroundColor = input$col)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
