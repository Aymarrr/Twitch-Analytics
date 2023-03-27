#Load libraries and Datasetx
library(tidyverse)      
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(gganimate)
library(shinyWidgets)


load("TidyTwitchAnalDataset.RData")

#pour le dashboard, Boutons : Home, Games, Plots, Table


streamerstab <- select(
    TidyDataAnalFinal, 
    Games, 
    Rank,
    Date,
    Hours_Streamed,
    Streamers_Number,
    Peak_Channels,
    Average_Channels
  )


viewerstab <- select(
    TidyDataAnalFinal,
    Games,
    Rank,
    Date,
    Hours_Watched,
    Peak_Viewers,
    Average_Viewers,
  )

#GamesBruh <- select(TidyDataAnalFinal, Games, Date == input$DPID)

#Dashboard UI

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = tags$a(href = "https://www.twitch.com", tags$img(src="https://mir-s3-cdn-cf.behance.net/projects/max_808_webp/e96eed166531375.Y3JvcCwxNTU1LDEyMTYsMCwxNjk.png", height = "60", width = "70")), titleWidth = "270px"),
  dashboardSidebar( width = "270px",
    sidebarMenu(
      id = "tabs",
      menuItem(" Home", tabName = "home", icon = icon("home")),
      menuItem( " Games", tabName = "games", icon = icon("gamepad")),
      menuItem( " Plots", tabName = "plots", icon = icon("line-chart")),
      menuItem( " Tables", tabName = "tables", icon = icon("list-alt")
                ),
      airDatepickerInput(
        inputId = "DPID",
        label = "Date analyzed :",
        value = "2023-01",
        placeholder = "Bruh",
        dateFormat = "MM/yyyy", 
        minDate = "2016-01",
        maxDate = "2023-01",
        highlightedDates = "2023-01",
        view = "months",
        startView = "2023-01",
        minView = "months",
        multiple = FALSE,
        clearButton = TRUE,
      )
    )
  ), 

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow( h2("  Top Twitch Games :")),
        fluidRow(
          column(4, valueBoxOutput("BoxTop1", width = 12)),
          column(4, valueBoxOutput("BoxTop2", width = 10)),
          column(4, valueBoxOutput("BoxTop3", width = 10)),
        fluidRow(
          h3("Here you can see the Peak Viewers of the Top3 twitch entertainments over time :"),
          column(12, plotlyOutput("PlotPeakVTop"))
        ),
        fluidRow(
          h3("Here you can see the Average Viewers of the Top3 twitch entertainments over time :"),
          column(12, plotlyOutput("PlotAvVTop")),
        ),
        fluidRow(
          h3("Here you can see the Peak Streamers numbers of the Top3 twitch entertainments over time :"),
          column(12, plotlyOutput("PlotPeakSTop")),
        )
          
        )
      ),
      tabItem(
        tabName = "games",
        fluidRow(
          column(4, virtualSelectInput(
            inputId = "SGID",
            label = "Games to be analyzed :",
            choices = arrange(GamesBruh), 
            selected = "League of Legends",
            showValueAsTags = TRUE,
            search = TRUE,
            hideClearButton = FALSE,
            autoSelectFirstOption = TRUE,
            multiple = FALSE
          )
          ),
          column(4, valueBoxOutput("Boxrank", width = 12)
            
          ),
          column(4, valueBoxOutput("Boxpeak", width = 12)
            
          )
        ),
        fluidRow(
          h3("Here are the main metrics about the viewership of the selected entertainment :"),
          column(6, plotlyOutput("PlotPeakVGames")),
          column(6, plotlyOutput("PlotAvVGames"))
        ),
        fluidRow(
          "You can see the evolution of the streamer numbers here :",
          column(12, plotlyOutput("PlotPeakSGames"))
          
        )
      ),
      tabItem(
        tabName = "plots",
        fluidRow(
          column(4, h3("The type plot :"), h4("In this plot, we can see over time what type of entertainment have being streamed on Twitch and the rising of IRL contents.
                                              We clearly can see the rising of IRL content without hurting other kinds of entertainments.")),
          column(8, plotlyOutput("PlotType")),
          #column(4, checkboxGroupInput("Type", h4("Type of entertainment :"),
                                       #choices = list("Co-op" = "Co-op",
                                                      #"IRL" = "IRL",
                                                      #"Multiplayer" = "Multiplayer",
                                                      #"Singleplayer" = "Singleplayer"),
                                       #selected = c("IRL", "Multiplayer", "Singleplayer")))
        )
      ),
      tabItem(
        tabName = "tables",
        fluidRow(
          tabBox(
            title = "Database",
            width = 12,
            tabPanel("Streamers", dataTableOutput("streamerstab")),
            tabPanel("Viewers", dataTableOutput("viewerstab")),
            
          )
        )
      )
    )
  ))



#server 

server <- function(input, output){
  GamesBruh <- reactive(filter(TidyDataAnalFinal, Date == input$DPID) %>%
      select(TidyDataAnalFinal, Games, Date))
  
  DataGamesBruh <- reactive(
      filter(TidyDataAnalFinal, Games == input$SGID, Date == input$DPID)
  )
  Bruh <- TidyDataAnalFinal %>% group_by(Type, Date, Average_Viewers)
  PlotBruh <- Bruh %>% 
    ggplot(aes(x = Date, y = Average_Viewers, color = Type)) + geom_point() + theme_minimal()    
  
  output$streamerstab <- renderDataTable(streamerstab, options = list(pageLength = 10))
  output$viewerstab <- renderDataTable(viewerstab, options = list(pageLength = 10))
  output$PlotType <- renderPlotly(ggplotly(PlotBruh))
  
  output$Boxrank <- renderValueBox(
    valueBox(
      value = DataGamesBruh()$Rank,
      subtitle = "Twitch Rank (viewership)",
      icon = icon("twitch", lib = "font-awesome"),
      color = "purple"
      
    )
  )
  output$Boxpeak <- renderValueBox(
    valueBox(
      value = DataGamesBruh()$Peak_Viewers,
      subtitle = "Peak Viewers in this month",
      icon = icon("users", lib = "font-awesome"),
      color = "orange"
    )
  )
  #Top Home Page
  Top1rank <- reactive(filter(TidyDataAnalFinal, Rank == "1", Date == input$DPID))
  Top2rank <- reactive(filter(TidyDataAnalFinal, Rank == "2", Date == input$DPID))
  Top3rank <- reactive(filter(TidyDataAnalFinal, Rank == "3", Date == input$DPID))
  
  output$BoxTop1 <-renderValueBox(
    valueBox(
      value = Top1rank()$Rank,
      subtitle = h2(Top1rank()$Games),
      icon = icon("twitch", lib = "font-awesome"),
      color = "yellow"
    )
  )
  output$BoxTop2 <-renderValueBox(
    valueBox(
      value = Top2rank()$Rank,
      subtitle = h4(Top2rank()$Games),
      icon = icon("twitch", lib = "font-awesome"),
      color = "teal"
    )
  )
  output$BoxTop3 <-renderValueBox(
    valueBox(
      value = Top3rank()$Rank,
      subtitle = h4(Top3rank()$Games),
      icon = icon("twitch", lib = "font-awesome"),
      color = "maroon"
    )
  )
  PeakVTop <- TidyDataAnalFinal  %>% 
    filter(Rank < 4) %>% 
    group_by(Games, Date, Peak_Viewers)
  
  PlotPeakVTop <- PeakVTop %>% 
    ggplot(aes(x = Date, y = Peak_Viewers, color = Games)) + geom_point() + theme_minimal()  
  
  output$PlotPeakVTop <- renderPlotly(ggplotly(PlotPeakVTop))
  
  AvVTop <- TidyDataAnalFinal %>% 
    filter(Rank < 4) %>% 
    group_by(Games, Date, Average_Viewers)

  PlotAvVTop <- AvVTop %>% 
    ggplot(aes(x = Date, y = Average_Viewers, color = Games)) + geom_point() + theme_minimal() 
  
  output$PlotAvVTop <- renderPlotly(ggplotly(PlotAvVTop))
  
  PeakSTop <- TidyDataAnalFinal  %>% 
    filter(Rank < 4) %>% 
    group_by(Games, Date, Peak_Channels) 
  
  PlotPeakSTop <- PeakSTop %>% 
    ggplot(aes(x = Date, y = Peak_Channels, color = Games)) + geom_point() + theme_minimal() 
  
  output$PlotPeakSTop <- renderPlotly(ggplotly(PlotPeakSTop))
  
  #Games Page
 # PeakVGames <- reactive(TidyDataAnalFinal %>% 
  #  filter(Games == input$SGID) %>% 
   # group_by(Games, Date, Peak_Viewers))
  
  #PlotPeakVGames <- PeakVGames %>% 
   # ggplot(aes(x = Date, y = Peak_Viewers, color = Games)) + geom_point() + theme_minimal()  
  
  #output$PlotPeakVGames <- renderPlotly(ggplotly(PlotPeakVGames))
  
 #AvVGames <- reactive(TidyDataAnalFinal %>% 
  #                         filter(Games == input$SGID) %>% 
  #                         group_by(Games, Date, Average_Viewers))
  
  #PlotAvVGames <- AvVGames %>% 
  #  ggplot(aes(x = Date, y = Average_Viewers, color = Games)) + geom_point() + theme_minimal()  
  
  #output$PlotAvVGames <- renderPlotly(ggplotly(PlotAvVGames))
  
 # PeakSGames <- reactive(TidyDataAnalFinal %>% 
  #                         filter(Games == input$SGID) %>% 
  #                         group_by(Games, Date, Peak_Channels))
  
 # PlotPeakSGames <- PeakSGames %>% 
 #   ggplot(aes(x = Date, y = Peak_Channels, color = Games)) + geom_point() + theme_minimal()  
  
  #output$PlotPeakSGames <- renderPlotly(ggplotly(PlotPeakSGames))
  
}

shinyApp(ui, server)

