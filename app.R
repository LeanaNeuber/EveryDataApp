addResourcePath(prefix = 'www', directoryPath = './www')

library(shinydashboard)
library(DT)
library(ggplot2)

de <- list(
  sProcessing = "Verarbeitung läuft...",
  sSearch = "Suche&nbsp;:",
  sLengthMenu = "Zeige _MENU_ Einträge",
  sInfo = "Zeige _START_ bis _END_ von _TOTAL_ Elementen",
  sInfoEmpty = "Zeige 0 bis 0 von 0 Elementen",
  sInfoFiltered = "(_MAX_ Elemente durchsucht)",
  sInfoPostFix = "",
  sLoadingRecords = "Laden...",
  sZeroRecords = "Keine Elemente anzuzeigen",
  sEmptyTable = "Keine Daten in der Tabelle..",
  oPaginate = list(
    sFirst = "Erste",
    sPrevious = "Vorherige",
    sNext = "Nächste",
    sLast = "Letzte"
  ),
  oAria = list(sSortAscending = ": aktivieren, um die Spalte in aufsteigender Reihenfolge zu sortieren",
               sSortDescending = ": activer aktivieren, um die Spalte in absteigender Reihenfolge zu sortieren")
)

kontakttab <- fluidPage(
  # TEAM BIO
  fluidRow(column(3),
           column(
             6,
             shiny::HTML("<br><br><center> <h5>Das EveryData Team</h5> </center><br>"),
             shiny::HTML(
               "<h6><center>Das Team rund um <i>EveryData</i> arbeitet für und im Auftrag von <a href='https://www.uni-due.de/biwi/lls/angela_heine.php'>Frau Heine</a>.</center></h6>"
             )
           ),
           column(3)),
  
  fluidRow(style = "height:50px;"),
  
  fluidRow(
    column(3),
    
    # Ella
    column(2,
           div(
             class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "www/ella.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Ella"),
                   tags$h6(
                     tags$i("Psychologin, Projektleiterin und Instagram-Beauftragte")
                   )),
               div(style = "width:10%;"),
               div("Ich liebe es, blablabla....")
             )
           )),
    # Katrin
    column(2,
           div(
             class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "katrin.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Katrin"),
                   tags$h6(
                     tags$i("Mensch-Computer Mensch und kreativer Kopf")
                   )),
               div(style = "height:10%;"),
               div("Grafiken? Kann ich. Blablab")
             )
           )),
    # Lea
    column(2,
           div(
             class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "lea.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Lea"),
                   tags$h6(
                     tags$i("Informatikerin und Verantwortlich für die Shiny-App")
                   )),
               div(style = "height:10%;"),
               div(
                 "Wenn es um die Entwicklung von IT-Lösungen in interdisziplinären Teams geht bin ich immer gern dabei!"
               )
             )
           )),
    column(3)
    
  ),
  fluidRow(style = "height:150px;")
)

infotab <- fluidPage(
  tags$head(tags$script(
    HTML(
      'var fakeClick = function(tabName) {
       var dropdownList = document.getElementsByTagName("a");
       for (var i = 0; i < dropdownList.length; i++) {
       var link = dropdownList[i];
       if(link.getAttribute("data-value") == tabName) {
       link.click();
       };
       }
       };'
    )
  )),
  # WHAT
  fluidRow(column(3),
           column(
             6,
             shiny::HTML(
               "<br><br><center> <h1>Was Ihr hier finden werdet</h1> </center><br>"
             ),
             shiny::HTML(
               "<h5>Eine Möglichkeit Daten zu verstehen und zu visualisieren.... TODO</h5>"
             )
           ),
           column(3)),
  
  fluidRow(style = "height:50px;"),
  
  # PAGE BREAK
  tags$hr(),
  
  # HOW
  fluidRow(column(3),
           column(
             6,
             shiny::HTML(
               "<br><br><center> <h1>Wie diese Seite Euch helfen kann</h1> </center><br>"
             ),
             shiny::HTML(
               "<h5>Ihr könnt die Daten anschauen und frei entscheiden, was Ihr wie visualisieren wollt. Außerdem helfen wir Euch bei den Übungen und erklären Euch
               interessante Konzepte hier und auf unserem Instagram Account oder auf Moodle:</h5>"
             )
           ),
           column(3)),
  fluidRow(style = "height:25px;"),
  fluidRow(column(3),
           column(
             6,
             tags$div(
               align = "center",
               tags$a("Instagram",
                      href = "https://www.instagram.com/everydata_ude/",
                      class = "btn btn-primary btn-lg"),
               tags$a("Moodle",
                      href = "https://moodle.uni-due.de/course/view.php?id=29686",
                      class = "btn btn-primary btn-lg"),
             )
           ),
           column(3)),
  fluidRow(style = "height:50px;"),
  
  # PAGE BREAK
  tags$hr(),
  
  # WHERE
  fluidRow(column(3),
           column(
             6,
             shiny::HTML("<br><br><center> <h1>Wer wir sind</h1> </center><br>"),
             shiny::HTML(
               "<h5>Wir sind Ella, Lea und Katrin und arbeiten an
               der UDE in Essen im Arbeitsbereich von Frau Heine.</h5>"
             )
           ),
           column(3)),
  fluidRow(style = "height:50px;"),
  fluidRow(
    # column(3),
    
    # Ella
    column(4,
           div(
             # class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "www/ella.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Ella"),
                   tags$h6(
                     tags$i("Psychologin, Projektleiterin und Instagram-Beauftragte")
                   )),
               div(style = "width:10%;"),
               div("Ich liebe es, blablabla....")
             )
           )),
    # Katrin
    column(4,
           div(
             # class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "katrin.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Katrin"),
                   tags$h6(
                     tags$i("Mensch-Computer Mensch und kreativer Kopf")
                   )),
               div(style = "height:10%;"),
               div("Grafiken? Kann ich. Blablab")
             )
           )),
    # Lea
    column(4,
           div(
             # class = "panel panel-default",
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "lea.png",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Lea"),
                   tags$h6(
                     tags$i("Informatikerin und Verantwortlich für die Shiny-App")
                   )),
               div(style = "height:10%;"),
               div(
                 "Wenn es um die Entwicklung von IT-Lösungen in interdisziplinären Teams geht bin ich immer gern dabei!"
               )
             )
           )),
    # column(3)
    
  ),
  
  
  # PAGE BREAK
  tags$hr(),
  
  
  fluidRow(column(3),
           column(
             6,
             shiny::HTML("<br><br><center> <h1>Bereit loszulegen?</h1> </center><br>"),
             shiny::HTML(
               "<h5>Wir empfehlen Euch, mit der Betrachtung der Visualisierungen zu beginnen.</h5>"
             )
           ),
           column(3)),
  fluidRow(style = "height:25px;"),
  fluidRow(column(3),
           column(6,
                  tags$div(
                    align = "center",
                    tags$a("Los geht's!",
                           onclick = "fakeClick('vis')",
                           class = "btn btn-primary btn-lg")
                  )),
           column(3)),
  fluidRow(style = "height:25px;")
  
)

datentab <-
  fluidPage(
            sidebarLayout(
              sidebarPanel(
                h2("Willkommen!"),
                width = 4,
                p(
                  "Hier seht ihr unser aktuelles Datenset, was sich täglich aktualisiert. Stay tuned!"
                ),
                # Button
                downloadButton("downloadData", "Ich will die Daten!"),
              ),
              mainPanel(dataTableOutput("table"), width = 8)
            ))


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "EveryData",
                  dropdownMenu(
                    type = "notifications", 
                    headerText = strong("Kontakt"), 
                    icon = icon("address-card"),
                    badgeStatus = NULL,
                    notificationItem(
                      text = ("Ella: ella.posny@uni-due.de"),
                      icon = icon("female")
                    ),
                    notificationItem(
                      text = ("Katrin: katrin.hartmann@uni-due.de"),
                      icon = icon("female")
                    ),
                    notificationItem(
                      text = ("Lea: leana.neuber@uni-due.de"),
                      icon = icon("female")
                    ),
                    notificationItem(
                      text = ("Frau Angela Heine: angela.heine@uni-due.de"),
                      icon = icon("chalkboard-teacher")
                    )
                    )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Daten", tabName = "data", icon = icon("table")),
      menuItem("Übungen", tabName = "exercises", icon = icon("award")),
      menuItem("Info", tabName = "info", icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("Essen"),
                infoBoxOutput("Zeit"),
                infoBoxOutput("Ausgaben")
              ),
              
              fluidRow(
                box(title="Histogram",plotOutput("plot1", height = 250)),
                
                box(
                  title = "Kategorie",
                  radioButtons(
                    "category",
                    "Wähle eine Spalte:",
                    choices = c("Name", "Kategorie", "Kosten", "Zeit", "Geschmack", "Fun")
                  ),
                  conditionalPanel(
                    "input.category == 'Zeit' | input.category == 'Kosten'| input.category == 'Geschmack'| input.category == 'Fun'",
                    sliderInput(
                      inputId = "bins",
                      label = "Number of bins:",
                      min = 1,
                      max = 10,
                      value = 3
                    )
                  )
                )
              ),
              fluidRow(
                box(title="Wer isst am meisten?", plotOutput("pie1"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              datentab
      ),
      
      tabItem(tabName = "exercises",
              "Sobald es mit den Übungen losgeht, findet Ihr hier Tips und Tricks... :)"
      ),
      tabItem("info", infotab)
    )
  )
)

server <- function(input, output) {

  datatable <- read.csv("init_food.csv", sep = ";")

  output$Essen <- renderInfoBox({infoBox("Essen", nrow(datatable), icon = icon("hamburger"), color = "purple", fill=TRUE)})
  output$Zeit <- renderInfoBox({infoBox("Minuten", sum(datatable$Zeit), icon = icon("clock"), fill = TRUE)})
  output$Ausgaben <- renderInfoBox({infoBox("Euro", sum(datatable$Kosten), icon = icon("euro-sign"),color="yellow", fill = TRUE)})
  output$table <- DT::renderDataTable({
    datatable
  },  rownames = FALSE, options = list(language = de))

  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      "food.csv"
    },
    content = function(file) {
      write.csv(datatable, file, row.names = FALSE)
    }
  )
  
  output$plot1 <-  renderPlot({
    if (input$category == "Zeit" | input$category == "Kosten"| input$category == "Geschmack"| input$category == "Fun")  {ggplot(data = datatable, aes_string(x = input$category)) + geom_histogram( bins=input$bins, fill = "#098474")} 
    else{ggplot(data = datatable, aes_string(x = input$category)) + geom_bar(stat="count", fill = "#098474") } 
  })
  output$pie1 <- renderPlot({
    ggplot(datatable, aes(x=factor(1), fill=datatable[,"Name"])) +
      geom_bar(stat="count") +
      # scale_y_continuous(breaks = seq(0,12,3), labels = c("0", "25%", "50%", "75%", "100%")) + 
      coord_polar("y", start=0)+
      theme_void()+ 
      theme(legend.position="right") +
      guides(fill=guide_legend(title="Name"))
  })
}

shinyApp(ui, server)