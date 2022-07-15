#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggpubr)
# Define UI for application that draws a histogram

ui <- fluidPage(
    titlePanel("Scatter plot with a linear model"),
     sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            # show Linear Regression
            checkboxInput("showLM", "Add a regression line",
                    value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           tableOutput("contents")
        )
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

        dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                      )
        return(df)
    })
    
    output$distPlot <- renderPlot({
        #plot(dataInput()$x,dataInput()$y, main="scatter plot", xlab="data1$x", ylab="data1$y")
    

    p2 <- {if (input$showLM){
    ggplot(data = dataInput(), aes(x = x, y = y)) + 
      geom_point() + geom_smooth(method = "lm", se = FALSE) + stat_cor(label.y = 15) + stat_regline_equation(label.y = 14.2)
    
    
    }
     else {
         ggplot(data = dataInput(), aes(x = x, y = y)) + 
      geom_point()  
          } 
         
         }

    return(p2)
    
    })
    
    output$contents <- renderTable({
    fit <- lm(dataInput()$y ~ dataInput()$x)
    model_summary = summary(fit)
    t1 <- {if (input$showLM){
      model_summary$coefficients[,1:2] 
    }
      else {
        return(head(dataInput()))
      }
    }
    return(t1)
  })  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
