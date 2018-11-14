#~~~~~~~~~~~~~~~~~~~~*
# Anne's Shiny App
#~~~~~~~~~~~~~~~~~~~~*
library(shinydashboard)
library(plyr)

ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "Forecast by region and corporate size"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Simulation", tabName = "simulation", icon = icon("dashboard"))
                      )
                    ),#close the sidebar menu
                    
                    dashboardBody(
                      tabItems(  
                        tabItem(tabName = "simulation",
                                # Here is an example viz with a the controls of interest
                                # you can change this as much as you like
                                fluidRow(
                                  box( width=4,
                                       title = "State",
                                       selectInput("state", 
                                                   label = "Select State", 
                                                   choices = list("AGUASCALIENTES", 
                                                                  "BAJA CALIFORNIA NORTE", 
                                                                  "BAJA CALIFORNIA SUR",
                                                                  'CAMPECHE',
                                                                  'CHIAPAS', 
                                                                  'CHIHUAHUA', 
                                                                  'COAHUILA', 
                                                                  'COLIMA',
                                                                  'DISTRITO FEDERAL', 
                                                                  'DURANGO',
                                                                  'EXTRANJERO',
                                                                  'GUANAJUATO', 
                                                                  'GUERRERO',
                                                                  'HIDALGO', 
                                                                  'JALISCO', 
                                                                  'MEXICO', 
                                                                  'MICHOACAN', 
                                                                  'MORELOS',
                                                                  'NAYARIT', 
                                                                  'NUEVO LEON', 
                                                                  'OAXACA', 
                                                                  'PUEBLA', 
                                                                  'QUERETARO', 
                                                                  'QUINTANA ROO',
                                                                  'SAN LUIS POTOSI', 
                                                                  'SINALOA', 
                                                                  'SONORA', 
                                                                  'TABASCO',
                                                                  'TAMAULIPAS', 
                                                                  'TLAXCALA',
                                                                  'VERACRUZ', 
                                                                  'YUCATAN',
                                                                  'ZACATECAS'),
                                                   selected = "AGUASCALIENTES")
                                       
                                  ),
                                  box( width=4,
                                       title = "Corporate Size",
                                       selectInput("size", 
                                                   label = "Select Corporate Size", 
                                                   choices = list("Entidades Financieras",
                                                                  "Fideicomiso",
                                                                  "Grande", 
                                                                  "PyME"),
                                                   selected = "PyME")
                                  )
                                ),#close fluid row
                                fluidRow(
                                  box( width=8,
                                       plotOutput("plot_meanRate"))
                                ),#close fluid row
                                fluidRow(
                                  box( width=8,
                                       plotOutput("plot_nonPerf"))
                                )# close fluid row
                                
                        )#close tabItem 
                      )#close tabItems
                    )# close dashbordBody
)# close UI


###################### Loading Data ################################

# Loading data
train_data = read.csv(file="../../data/Corporate_Loans_Data.csv", 
                      header=TRUE, sep=",")
forecast = read.csv(file="../../results/prediction1.csv",
                    header=TRUE, sep=",")
train_data["Date"] <- as.Date(paste(as.character(train_data$Year), as.character(train_data$Month), "1", sep="-"), format="%Y-%m-%d")
forecast["Date"] <- as.Date(paste(as.character(forecast$Year), as.character(forecast$Month), "1", sep="-"), format="%Y-%m-%d")

train_facts <- ddply(train_data, .(State, Corporate.Size, Date), summarise, 
                     Mean.Rate = mean(Average.Rate),
                     Mean.Term = mean(Average.Term),
                     Sum.M.Income = sum(Average.Monthly.Income),
                     Portfolio.Size = sum(Total.Portfolio),
                     Non.Performing = sum(Non.Performing.Portfolio),
                     Perc.Non.Perf = sum(Non.Performing.Portfolio)/sum(Total.Portfolio))


###################### Server ################################

server <- function(input, output) {
  #plot meanRate data with forecast
  output$plot_meanRate <- renderPlot({
    
    sub_data <- train_facts[train_facts$State == input$state & train_facts$Corporate.Size == input$size,]
    sub_fore <- forecast[forecast$State == input$state & forecast$Corporate.Size == input$size,]
    t_data <- sub_data$Date
    t_fore <- sub_fore$Date
    rate_data <- sub_data$Mean.Rate
    rate_fore <- sub_fore$MeanRate
    
    if (length(t_data) > 0){
      
      xLimits <- c(min(train_data$Date), max(forecast$Date))
      xLimits <- as.numeric(xLimits)
      plot(t_data, rate_data, 
           main = 'Mean rate over time with forecast',
           xlab = 'Time',
           ylab = 'Mean interest rate in %',
           type= "o", 
           col = 'black', 
           xlim= xLimits)
      points(t_fore, rate_fore, type="o", col = 'darkgreen')
      legend("topright",
             c("True data","Forecast"),
             fill=c("black","darkgreen"))
    } else {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No data available for this case"),
           cex = 1.6, col = "black")
    }
  })
  
  # plot Non performing rate
  output$plot_nonPerf <- renderPlot({
    
    sub_data <- train_facts[train_facts$State == input$state & train_facts$Corporate.Size == input$size,]
    sub_fore <- forecast[forecast$State == input$state & forecast$Corporate.Size == input$size,]
    t_data <- sub_data$Date
    t_fore <- sub_fore$Date
    nonp_data <- sub_data$Perc.Non.Perf
    nonp_fore <- sub_fore$PercentNonPerforming
    
    if (length(t_data) > 0){
      
      xLimits <- c(min(train_data$Date), max(forecast$Date))
      xLimits <- as.numeric(xLimits)
      plot(t_data, nonp_data, 
           main = '% Non performing portfolio over time',
           xlab = 'Time',
           ylab = 'Non performing portfolio in %',
           type= "o", 
           col = 'black', 
           xlim= xLimits)
      points(t_fore, nonp_fore, type="o", col = 'darkgreen')
      legend("topright",
             c("True data","Forecast"),
             fill=c("black","darkgreen"))
    } else {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("No data available for this case"),
           cex = 1.6, col = "black")
    }
  })
}

shinyApp(ui, server)
