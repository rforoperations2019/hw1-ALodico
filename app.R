
library(shiny)
library(DT)
library(ggplot2)


parking <- read.csv("WPRDCparkingDATA.csv")
# Added a "lots" column for barplot
parking[8] <- 1
names(parking)[8]<-"lots"

# Define UI 
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            # number for maximum willing to pay for parking
            numericInput("maxpay", "The most I will pay (in dollars) for an hour of parking is:",
                         5, min = .5, max = 5
            ),
            #checkbox to display on and off street parking options
            checkboxGroupInput("lottype", "Show me parking options that are:",
                               c("on-street", "off-street"),
                               selected = c("on-street", "off-street")
            ),
            # Dropdown for barplot input
            selectInput("Yaxis", "Choose units to be broken down by rate", 
                        c("spaces", "meters", "lots")
            ),
            actionButton("omits", "See table of omitted results")
        ),
        mainPanel(
            # Show the total units in the plots here:
            textOutput("totalText"),
            # and the download link here:
            downloadLink('downloadData', 'Download'),
            # and the barplot here
            plotOutput("parkbarPlot"),
            # and the boxplot here
            plotOutput("parkboxPlot"),
            #and the data table  
            dataTableOutput('table')
        )
    )
)

# Define Server logic for barplot

server <- function(input, output) {
    parkour <- reactive({
        # Filter results by rate and lot type
        validate(
            need(input$lottype, 'Check at least one type of parking!')
        )
        parkpay <- filter(parking, rate <= input$maxpay)
        parknona <- na.omit(parkpay, cols=c(input$Yaxis, "rate"))
        parkomit <- parking[!(parkpay[,1] %in% parknona[,1]),]
        if(length(input$lottype) == 2){
            parklot = parknona
        } else if(length(input$lottype) == 1) {
            parklot <- filter(parknona, type == input$lottype)
        } else {
            print("Pick at least one lot type")
            stop()
        }
    })
    
    # Create a summary of selectinput sum by parking rate
    output$parkbarPlot <- renderPlot({
        parklot <- parkour()
        parkbar <- tapply(parklot[,input$Yaxis], parklot$rate, FUN=sum, na.rm=TRUE)
        barplot(parkbar, col=c(1:100), main=paste("sum of", input$Yaxis, "by parking rate", sep=" "), xlab="Parking rate", ylab=input$Yaxis)
    })
    
    # Create boxplots showing the distribution of selectinputs by parking rate
    output$parkboxPlot <- renderPlot({
        parklot <- parkour()
        parklot$rate <- as.factor(parklot$rate)
        ggplot(parklot, aes(x=rate, y=parklot[,input$Yaxis], color=rate)) +
            geom_boxplot() + scale_y_continuous(name = paste("distribution of", input$Yaxis, "per lot", sep=" "))
    })
    
    #text for # of whatever is in the plots
    output$totalText <- renderText({
        parklot <- parkour()
        parktot <- sum(parklot[,input$Yaxis])
        paste("Based on a total of", parktot, input$Yaxis)
        #        paste(input$lottype, length(input$lottype))
    })
    #Download of data used to make graphs
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("PPALotInfo", Sys.Date(), ".csv", sep='')
        },
        content = function(cont) {
            parklot <- parkour()
            write.csv(parklot, cont)
        })
    
    observeEvent(input$omits, {
        #optional table of omitted results
        output$table <- renderDataTable(parkomit,
                                        options = list(
                                            pageLength = 5 ))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)