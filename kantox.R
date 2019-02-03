
library(tidyverse)
library(shiny)
library(ggplot2)


kantox<-read.csv("data/kantox.csv", sep = ";", header = TRUE)
kantox2<-read.csv("data/kantox2.csv", sep = ";", header = TRUE)
kantox3<-read.csv("data/kantox3.csv", sep = ";", header = TRUE)

kantox.tibble<-as.tibble(kantox)  
kantox2.tibble<-as.tibble(kantox2)
kantox3.tibble<-as.tibble(kantox3)

# works on kantox 2
kantox2.tibble$ActualCumulativeRevenue<-cumsum(kantox2.tibble$Revenue)  #cumsums
kantox2.tibble$dateconv<- as.Date(kantox2.tibble$Date, "%d/%m/%y")

# works on kantox 1
kantox.tibble$TargetCumulativeRevenue<-cumsum(kantox.tibble$Target.Revenue)
kantox.tibble$dateconv<- as.Date(kantox.tibble$Date, "%d/%m/%y")

# actual cumulated revenues
cumulative<-kantox2.tibble %>% group_by(month=lubridate::floor_date(dateconv, "month")) %>%
  summarize(amount=sum(Revenue))
cumulative$cumulative<-cumsum(cumulative$amount)

# target cumulated revenues
y1<-filter(kantox.tibble, TargetCumulativeRevenue<3770)
cumulative$TargetRevenues<-y1$TargetCumulativeRevenue



# Define UI for application that draws a histogram
ui<-fluidPage(
  
  mainPanel(
    
    titlePanel("John's sales behavior"),
    
    tabsetPanel(type="tabs",
                tabPanel("Revenues to date",plotOutput("salesPlot",width = "100%"),
                         plotOutput("salesPlot2", width = "100%", height = "300px"),
                         plotOutput("salesPlot3", width = "100%", height = "300px")),
                tabPanel("Opportunities",plotOutput("salesPlot4",width = "100%")),
                tabPanel("Data Rev as of 15th Oct", dataTableOutput("salesTbl")),
                tabPanel("Data Opp as of 15th Oct", dataTableOutput("salesTbl2"))
                
                
    )
    
  )
  
)
# Define server logic required to draw a histogram
server<-function(input, output, session) {   
  
  output$salesPlot <- renderPlot({
    
    # Get all data that should be visible to the current user.
    data <- select(kantox2.tibble,1:7)
    data2<- select(kantox3.tibble, 1:6)
    
    # Generate the sales plot
    
    output$salesPlot <- renderPlot({
      
      p <- ggplot(cumulative, aes(month)) + 
        geom_line(aes(y = cumulative, colour = "Revenues to date"))+ 
        geom_line(aes(y = TargetRevenues, colour = "Target Revenues"))+
        geom_line(aes(y = 4160, colour="Target for 31/12/2018"))+
        #geom_hline(yintercept=4160, linetype=3)+
        xlab("Months")+
        ylab("Acumulated Revenues")
      print(p)
    })
    
    output$salesPlot2 <- renderPlot({
      
      p2<-ggplot(kantox2.tibble, aes(Client.s.Category, fill=Client.s.Category))+
        geom_bar()+
        xlab("Client Category")+
        ylab("Number of clients")
      print(p2)
    })
    
    output$salesPlot3 <- renderPlot({
      
      p3<-ggplot(kantox2.tibble, aes(Client.s.Country, fill=Client.s.Category))+
        geom_bar()+
        xlab("Client country")+
        ylab("Number of clients")
      print(p3)
    })
    
    output$salesPlot4 <- renderPlot({
      
      p4<-ggplot(kantox3.tibble, aes(Status, fill=Oppty.Category))+
        geom_bar()+
        xlab("Status by client's category")+
        ylab("Number")
      #facet_grid(~Oppty.Category)
      print(p4)
    })
    
    output$salesTbl <- renderDataTable({
      
      data
      
    })
    output$salesTbl2 <- renderDataTable({
      
      data2
      
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

