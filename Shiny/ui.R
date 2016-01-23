library(shiny)

shinyUI(fluidPage(
        titlePanel("Text Prediction App"),
        
        sidebarLayout(
                sidebarPanel( 
                        helpText("Please input a word or phrase to get a prediction. When no input exists you will get a starter word.")
                        ,textInput("input.str", label = "Input phrase:")
                        #,submitButton("Predict!")
                        ,actionButton("goButton", "Predict!")
                ),
                mainPanel(tabsetPanel(
                                tabPanel("Results"
                                         , h2(textOutput("top.result"))
                                         , fluidRow()
                                         , p("Flip over to 'Details' if you'd like to see more results....")
                                         )
                                , tabPanel("Details"
                                           , tableOutput("all.results")
                                           , fluidRow()
                                           , p("Here you can see more comprehensive output. I have returned the results where the input phrase matched an N-gram as well as the Maximum Likelihood Estimate for that prediction.")
                                           )
                          )
                )
        )
))