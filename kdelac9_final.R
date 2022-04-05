#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyWidgets)

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

profile = read.csv("profiles_cleaned.csv")
profile = profile[-1,]
profile[which(profile$sex == "m"),]$sex = "male"
profile[which(profile$sex == "f"),]$sex = "female"

profile$sex = factor(profile$sex, levels = c("male","female"), ordered =TRUE)
profile$orientation = as.factor(profile$orientation)
profile$drinks = as.factor(profile$drinks)
dat <- profile %>% 
    dplyr::select(age, ethnicity, sex, height, orientation, drinks, drugs, sign) %>% 
    mutate(ethnicity = as.factor(ethnicity), age = as.numeric(age))
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
                    body {background-color: darkred;color: white; /* text color */}
                    /* Change header text to imported font */
                    h2 {font-family: 'Yusei Magic', sans-serif;}
                    /* Make text visible on inputs */
                    .shiny-input-container {color: #474747;}"))),
    titlePanel("Dataing Science"),
    tags$p("By Brianna Diaz, Haley Ferrini, Kobe Dela Cruz, Minchu Feng, Yu Du"),
    sidebarPanel(width = 5,
                 tags$style(HTML("
                  .tabbable > .nav > li > a{font-weight: bold; background-color: pink; color:white}
                  ")),
                 tabsetPanel(type = "pills",selected = "Bio",
                             tabPanel("Bio",
                                      pickerInput("checkGroup", h4("Ethnicities"), 
                                                  choices = list(
                                                      "asian" = "asian", 
                                                      "black" = "black",
                                                      "hispanic / latin" = "hispanic / latin",
                                                      "middle eastern" = "middle eastern",
                                                      "native american" = "native american",
                                                      "white" = "white",
                                                      "mixed" = "mixed"), 
                                                  selected = list(
                                                      "asian" = "asian", 
                                                      "black" = "black",
                                                      "hispanic / latin" = "hispanic / latin",
                                                      "middle eastern" = "middle eastern",
                                                      "native american" = "native american",
                                                      "white" = "white",
                                                      "mixed" = "mixed"), 
                                                  multiple = T,
                                                  options = list(`actions-box` = TRUE)
                                      ),
                                      checkboxGroupInput("checksex", h4("Sex"), 
                                                         choices = list("Male" = "male", 
                                                                        "Female" = "female"),
                                                         selected = "male"
                                      ),
                                      varSelectInput("checkheight","Variable:", data = profile)
                             ),
                             tabPanel("Substances",
                                      pickerInput("checkdrinks", h4("Drinks"),
                                                  choices = list("Not at all" = "not at all",
                                                                 "Rarely" = "rarely",
                                                                 "Socially" = "socially",
                                                                 "Often" = "often",
                                                                 "Very often" = "very often"),
                                                  selected = "not at all", multiple = T,
                                                  options = list(`actions-box` = TRUE)
                                      ),
                                      pickerInput("checkdrugs", h4("Drugs"),
                                                  choices = list("sometimes"= "sometimes",
                                                                 "never"= "never", 
                                                                 "often"= "often"), 
                                                  selected = list("sometimes"= "sometimes",
                                                                  "never"= "never", 
                                                                  "often"= "often"),
                                                  multiple = T,
                                                  options = list(`actions-box` = TRUE))),
                             tabPanel("More Options",
                                      selectInput("checkorientation", h4("Orientation"), 
                                                  choices = list("Heterosexual" = "straight", 
                                                                 "Homosexual" = "gay",
                                                                 "Bisexual" = "bisexual"),
                                                  selected = "straight"), 
                                      pickerInput("checkSign", h4("Zodiac Sign"), 
                                                  choices = list(
                                                      "Aries" = "aries", 
                                                      "Taurus" = "taurus",
                                                      "Gemini" = "gemini",
                                                      "Cancer" = "cancer",
                                                      "Leo" = "leo",
                                                      "Virgo" = "virgo",
                                                      "Libra" = "libra",
                                                      "Scorpio" = "scorpio",
                                                      "Sagittarius" = "sagittarius",
                                                      "Capricorn" = "capricorn",
                                                      "Aquarius" = "aquarius",
                                                      "Pisces" = "pisces"), 
                                                  selected = list(
                                                      "Aries" = "aries", 
                                                      "Taurus" = "taurus",
                                                      "Gemini" = "gemini",
                                                      "Cancer" = "cancer",
                                                      "Leo" = "leo",
                                                      "Virgo" = "virgo",
                                                      "Libra" = "libra",
                                                      "Scorpio" = "scorpio",
                                                      "Sagittarius" = "sagittarius",
                                                      "Capricorn" = "capricorn",
                                                      "Aquarius" = "aquarius",
                                                      "Pisces" = "pisces"),
                                                  multiple = T,
                                                  options = list(`actions-box` = TRUE))
                                      
                             )
                 ),
                 tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: pink}")),
                 sliderInput("slider_age", h3("Age Range"),
                             min = floor(min(profile$age)), 
                             max = ceiling(max(profile$age)), 
                             value = c(18, 27)),
                 checkboxInput("viewavg", "Display Age Average and Mode")
    ),
    mainPanel( width = 7,
               code("  sponsored by"),
               img(src="okcupid1.png", width = 200),
               plotOutput("Plot"),
               span(textOutput("view"), style="color:white"),
    )
)





# Define server logic required to draw a ggplot
server <- function(input, output, session) {
    
    x = reactive({
        
        if(input$checkheight == "Above Average"){
            dat %>% filter(input$slider_age[1] <= age & age <= input$slider_age[2], 
                           ethnicity %in% input$checkGroup, 
                           sign %in% input$checkSign,
                           orientation == input$checkorientation,
                           sex %in% input$checksex, 
                           drinks %in% input$checkdrinks,
                           drugs %in% input$checkdrugs,
                           height > mean(height))
        }else if(input$checkheight =="Average"){
            dat %>% filter( input$slider_age[1] <= age & age <= input$slider_age[2], 
                            ethnicity %in% input$checkGroup, 
                            sign %in% input$checkSign,
                            sex %in% input$checksex, 
                            drinks %in% input$checkdrinks,
                            drugs %in% input$checkdrugs,
                            orientation == input$checkorientation,
                            mean(height) - 3< height & height < mean(height)+3)
        }else if(input$checkheight =="Below Average"){
            dat %>% filter(input$slider_age[1] <= age & age <= input$slider_age[2], 
                           ethnicity %in% input$checkGroup, 
                           sign %in% input$checkSign,
                           sex %in% input$checksex,
                           drinks %in% input$checkdrinks,
                           drugs %in% input$checkdrugs,
                           orientation == input$checkorientation,
                           height < mean(height))
        }
    })
    
    
    
    text = reactive({
        if(input$viewavg == TRUE){
            if (!is.na(mean(x()$age))) {
                paste("Average Age =", round(mean(x()$age),2), "and Mode =", Mode(x()$age))
            } else {
                "Oops, no unicorn here."
            }
        }else{
            ""
        }
    })
    
    output$view = renderText({
        text()
    })
    
    plot = reactive({
        if(input$viewavg == TRUE){
            ggplot(x(), aes(x = age, fill = sex, color = sex)) + 
                geom_histogram(position = "identity", alpha=.5, binwidth = 1) + 
                theme(legend.position = "top") + 
                scale_y_continuous() + 
                xlab("Age") + ggtitle("Age Distribution Based On Your Preferences")+theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 17, face = "bold")) + geom_vline(xintercept = mean(x()$age), linetype = "dashed", color = "red", size = 1.75)
        }else{
            ggplot(x(), aes(x = age, fill = sex, color = sex)) + 
                geom_histogram(position = "identity", alpha=.5, binwidth = 1) + 
                theme(legend.position = "top") + 
                scale_y_continuous() + 
                xlab("Age") + ggtitle("Age Distribution Based On Your Preferences")+theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 17, face = "bold")) 
        }
    })
    
    
    output$Plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        plot()
        
    })
    
    
}
# Run the app

shinyApp(ui = ui, server = server, options = list(height = 650))









