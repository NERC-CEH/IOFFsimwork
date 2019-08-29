
# read data
readresult <- read.csv("newscen_results_repeat-compiled_final50.csv")

# prepare data
readresult <- readresult[,-1]   # delete index column
readresult$sim <- ceiling(readresult$sim/9)  # 

# values in each columns
lam = unique(readresult$lambda)
env = unique(readresult$env.beta)
sig = unique(readresult$sigma2x)
kap = unique(readresult$kappa)
nsa = unique(readresult$nsamp)
mod = paste(unique(readresult$Model), sep=", ")
sec = paste(unique(readresult$section), sep=", ")



library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(title = "NewScenario"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot", tabName = "Plot", icon = icon("chart-bar")),
      menuItem("Data", tabName = "Data", icon = icon("th"))),
    
      
      selectizeInput(inputId = "lambda",
                     label = "Intensity",
                     choices = NULL, 
                     multiple = TRUE),
      
      selectizeInput(inputId = "env.beta",
                     label = "Environment covariate coefficient", 
                     choices = NULL, 
                     textOutput("values"),
                     multiple = TRUE),
      
      selectizeInput(inputId = "sigma2x",
                     label = "Variance", 
                     choices = NULL,  
                     textOutput("values"),
                     multiple = TRUE),
      
      selectizeInput(inputId = "kappa",
                     label = "Scale", 
                     choices = NULL, 
                     textOutput("values"),
                     multiple = TRUE),
      
      selectizeInput(inputId = "nsamp",
                     label = "Number of samples", 
                     choices = NULL,  
                     textOutput("values"),
                     multiple = TRUE),
      
      selectizeInput(inputId = "section",
                     label = "Coverage area", 
                     choices = NULL, 
                     textOutput("values"),
                     multiple = TRUE),
      
      selectizeInput(inputId = "model",
                     label = "Model", 
                     choices = NULL, 
                     textOutput("values"),
                     multiple = TRUE),
    
    radioButtons("metrics", "Metrics",
                 c("MAE" = "MAE",
                   "Correlation" = "correlation"), 
                 selected = "MAE"),
    
    radioButtons("grouping", "Group by",
                 c("Model" = "bymodel",
                   "Section" = "bysection"), 
                 selected = "bymodel"),
    
    actionButton("update", "Update")
    
  ),
  
  dashboardBody(
    #tags$div(c("Plot", "Data"), class = "tab-content", ),
    
    tabItems(
      
      # First tab content
      tabItem(tabName = "Plot",
              
              fluidRow(
                
                tags$h1("Boxplot"),
                
                #sidebarLayout(
                #sidebarPanel(
                
                
                plotOutput("box")
      
      )),
    
    # Second tab content
      tabItem(tabName = "Data",
              
              h2("Data table"),
              
              checkboxGroupInput("columns", "Select Columns",
                                 choices = names(readresult),
                                 selected = names(readresult),
                                 inline = T),
              
              textOutput("numrows"),
              
              div(style = 'overflow-x: scroll', tableOutput("tab"))
              )
    )
    )
  )



server <- function(input, output, session){
  
  updateSelectInput(session, 'lambda',
                    choices = lam, selected = lam[1])
  
  updateSelectInput(session, 'env.beta',
                    choices = env, selected = env[1])
  
  updateSelectInput(session, 'sigma2x',
                    choices = sig, selected = sig[1])
  
  updateSelectInput(session, 'kappa',
                    choices = kap, selected = kap[1])
  
  updateSelectInput(session, 'nsamp',
                    choices = nsa, selected = nsa[1])
  
  updateSelectInput(session, 'model',
                    choices = mod, selected = mod[1])
  
  updateSelectInput(session, 'section',
                    choices = sec, selected = sec[1])
  
  
  
  updatelambda <- eventReactive(input$update, {
    input$lambda
  })
  
  updateenv.beta <- eventReactive(input$update, {
    input$env.beta
  })
  
  updatesigma2x <- eventReactive(input$update, {
    input$sigma2x
  })
  
  updatekappa <- eventReactive(input$update, {
    input$kappa
  })
  
  updatensamp <- eventReactive(input$update, {
    input$nsamp
  })
  
  updateModel <- eventReactive(input$update, {
    input$model
  })
  
  updatesection <- eventReactive(input$update, {
    input$section
  })
  
  updategrouping <- eventReactive(input$update, {
    input$grouping
  })
  
  updatemetrics <- eventReactive(input$update, {
    input$metrics
  })

  
  output$box <- renderPlot({
    
    library(dplyr)
    
    df <- filter(readresult,
                 lambda %in% updatelambda() &
                   env.beta %in% updateenv.beta() &
                   sigma2x %in% updatesigma2x() &
                   kappa %in% updatekappa() &
                   section %in% updatesection() &
                   Model %in% updateModel())
    
    library(ggplot2)
    
    if("bysection" %in% updategrouping()){
      
      ggplot(df, aes(y = eval(parse(text = updatemetrics()))
                     , x = section)) +
        geom_boxplot(aes(fill = section)) +
        facet_wrap(. ~ Model, scales = "free") +
        labs(fill = "Coverage Area", y = updatemetrics()) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 10),
              legend.text = element_text(size = 10),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10))
      
    } else if("bymodel" %in% updategrouping()){
      
      ggplot(df, aes(y = eval(parse(text = updatemetrics())), 
                     x = Model)) +
        geom_boxplot(aes(fill = Model)) +
        facet_wrap(. ~ section, scales = "free") +
        labs(fill = "Model", y = updatemetrics()) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size = 10),
              legend.text = element_text(size = 10),
              axis.text = element_text(size = 10),
              axis.title = element_text(size = 10))
    }
    
  })
  
  observeEvent(input$columns, {
    #cols <- input$columns
    
      df <- filter(readresult,
                  lambda %in% updatelambda() &
                    env.beta %in% updateenv.beta() &
                    sigma2x %in% updatesigma2x() &
                    kappa %in% updatekappa() &
                    section %in% updatesection() &
                    Model %in% updateModel())[input$columns]
      names(df) <- input$columns
      
      output$numrows <- renderText({
        paste("Number of rows:", nrow(df))
      })
      
      output$tab <- renderTable({
        df
      })
        
    
    })
  }
  


shinyApp(ui = ui, server = server)
