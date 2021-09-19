addResourcePath(prefix = 'www', directoryPath = './www')
ui <- fluidPage(
  titlePanel("EveryData"),
  sidebarLayout(
    sidebarPanel(
      h2("Willkommen!"),
      p("Datenhunger? Dann seid Ihr hier richtig!"),
      br(),
      br(),
      # Input: Choose dataset ----
      #selectInput("dataset", "Choose a dataset:",
        #          choices = c("food")),
      
      # Button
      downloadButton("downloadData", "Download"),
      br(),
      br(),
      img(src = "www/wir.png", height = 100, style="display: block; margin-left: auto; margin-right: auto;"),
      br()
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )
)


# Define server logic ----
server <- function(input, output) {
  # Reactive value for selected dataset ----
  #datasetInput <- reactive({
   # switch(input$dataset,
  #         "food" = food)
  #})
  #This function is responsible for loading in the selected file
  # filedata <- reactive({
  #   infile <- input$dataset
  #   if (is.null(infile)) {
  #     # User has not uploaded a file yet
  #     return(NULL)
  #     print("NOPE")
  #   }
  #   #read.csv(paste(infile, ".csv", sep=""), sep=";")
  #   read.csv("food.csv", sep=";")
  # })
  # filedata <- read.csv("food.csv", sep=";")
  # Table of selected dataset ----
  output$table <- renderDataTable({
    read.csv("food.csv", sep=";")
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filedata(), file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)