#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(evd)
library(tidyverse)
source("plot_functions.R")
library(latex2exp)
all_comb <- readRDS("all_comb.rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
  withMathJax(),
    # Application title
    titlePanel("Density plots of angular elements \\(W_1\\) and 
                      \\(W_2\\) of random variables generated from EVD (extreme value distribution)"),
hr(),
fluidRow(
  column(4,),
  column(8,
radioButtons(inputId="u", label="Choose threshold", choices=c(0.9,0.99,0.999), selected = NULL,inline=TRUE)
)),
hr(),
  fluidRow(
    column(6,
           sliderInput("alpha",  "Choose dependence parameter \\(\\alpha\\)",min=0.05,max=0.99, step=0.01,value=0.5)),
       column(3,
           sliderInput("a_1","Choose dependence parameter \\(\\alpha\\)_1",min=0.05,max=0.99, step=0.01,value=0.5)),
    column(3,
           sliderInput("a_2","Choose dependence parameter \\(\\alpha_2\\)",min=0.05,max=0.99, step=0.01,value=0.5))),
  hr(),
  fluidRow(
    
    column(6,
           h2("Trivariate symmetric logistic GEV"),
           plotOutput("symmetric")
    ),
    column(6,
           h2("Trivariate assymetric logistic GEV with \\(X-Y\\) and \\(X-Y\\) dependence"),
           plotOutput("assymetric")
    )
    
  ),
   verbatimTextOutput("ch")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

 # output$ch <- renderPrint(as.numeric(input$u))
    output$symmetric <- renderPlot({
      generate_dependent_X_Y_Z(N=50,abc=abc,dep=input$alpha) %>% plot_clusters(u=as.numeric(input$u),dep=input$alpha)
    })
    
    output$assymetric <- renderPlot({
      i <- match(input$a_1,seq(0.5,0.99,0.01))
      j <- match(input$a_2,seq(0.5,0.99,0.01))
      #generate_dependent_X_Y_Y_Z(N=50,abc=abc,dep=c(input$a_1,input$a_2)) %>% 
       s[i,j,,] %>% as.data.frame() %>%  plot_clusters_2dep(u=as.numeric(input$u),dep=c(input$a_1,input$a_2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
