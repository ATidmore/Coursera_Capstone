library(shiny)
load(file = "./data/DFM.Pred.RData", envir=.GlobalEnv)
load(file = "./data/Prediction.RData", envir=.GlobalEnv)
print(getwd())
library(dplyr, quietly = T)
library(stringr, quietly = T)
library(hashr, quietly = T)




shinyServer(
        function(input, output) {
                
                tab.res <- reactive({
                        input$goButton
                                f.predict(input$input.str)
                                })
            
                output$top.result <- renderText({
                        input$goButton
                        
                        isolate({tab.res()
                        top <- results.app %>% top_n(1) %>% select(predicted)
                        paste("Next word: ",top[1,])
                                })
                        })
                
               
                
                output$all.results <- renderTable({
                        input$goButton
                        
                        isolate({tab.res()
                        results.app})
                        })
                       
                
                        
                
        }
        
)
