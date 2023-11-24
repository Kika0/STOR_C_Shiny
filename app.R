#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(evd)
library(tidyverse)
source("plot_functions.R")
library(latex2exp)
all_comb <- readRDS("all_comb.rda")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  withMathJax(),
  # Application title
  title="Density plots of angular elements \\(W_1\\) and 
                      \\(W_2\\) of random variables with \\(X-Y-Z\\)  dependence generated from trivariate logistic GEV distribution",
  sidebar = sidebar(
    p("\\(n=5000\\)"),
    radioButtons(inputId="u", label="Choose threshold",choices=c(0.9,0.99,0.999), selected = NULL,inline=FALSE),
    hr(),
    sliderInput("alpha",  "Choose dependence parameter \\(\\alpha\\)",min=0.05,max=0.95, step=0.05,value=0.5),
    hr(),
    sliderInput("a_1","Choose dependence parameter \\(\\alpha_1\\)",min=0.05,max=0.95, step=0.05,value=0.75),
    sliderInput("a_2","Choose dependence parameter \\(\\alpha_2\\)",min=0.05,max=0.95, step=0.05,value=0.75),
    hr()
  ),
  layout_columns(
    card(card_header("Same dependence between pairs \\(\\alpha\\)"),
         plotOutput("symmetric")
    ),
    card(card_header("Different dependence between pairs \\(\\alpha_1\\) and \\(\\alpha_2\\)"),
         plotOutput("asymmetric")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$ch <- renderPrint(as.numeric(input$u))
  output$symmetric <- renderPlot({
    generate_dependent_X_Y_Z(N=5000,dep=input$alpha) %>% plot_clusters(u=as.numeric(input$u),dep=input$alpha)
  })
  
  output$asymmetric <- renderPlot({
    
    #generate_dependent_X_Y_Y_Z(N=5000,dep=c(input$a_1,input$a_2)) %>% 
    all_comb[as.numeric(input$a_1)*20,as.numeric(input$a_2)*20,,] %>% as.data.frame() %>%  plot_clusters_2dep(u=as.numeric(input$u),dep=c(input$a_1,input$a_2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

