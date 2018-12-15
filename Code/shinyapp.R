library(shiny)
library(rsconnect)

#ANLY503-TakeHome-Shiny
setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")

data<- read.csv("FIFA19 - Ultimate Team players.csv")
col_class<-sapply(data,class)
col_class[9]="character"
data<-read.csv("FIFA19 - Ultimate Team players.csv",colClasses = col_class)

data<-data[,c("player_extended_name","quality","overall","club","league","nationality","position","age","height","weight")]

data[!data$league %in% c("Bundesliga","Premier League",
                         "LaLiga Santander","Serie A TIM","Ligue 1 Conforama"),"league"]="Others"
data$league<- as.factor(data$league)
#data[,5]<- sapply(data[,5],as.factor())
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("FIFA 19-Player Height Distribution by Leagues and Overall Scores"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "league",
                  label = "Different Leagues",
                  choices = levels(data$league), 
                  selected = "Premier League"),
      
      sliderInput(inputId="overall", label="Overall Score Range:",
                  min = 47, max = 98,
                  value = c(47,98))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    category=input$league
    
    x <- data[(data$league == category) & (data$overall>=input$overall[1]),]
    x <- x[x$overall<=input$overall[2],]
    x <- x$height

    
    hist(x, breaks = 15, col = "#75AADB", border = "white",
         xlab = "Height in cm",
         main = "Histogram of Height Distribution")
    
  })
  
}

shinyApp(ui, server)
deployApp()
