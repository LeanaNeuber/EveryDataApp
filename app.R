addResourcePath(prefix = 'www', directoryPath = './www')
library(DT)
library(bslib)
library(ggplot2)

fr <- list(
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


datentab <-
  fluidPage(theme = bslib::bs_theme(bootswatch = "flatly"),
            sidebarLayout(
              sidebarPanel(
                h2("Willkommen!"),
                width = 3,
                p(
                  "Hier seht ihr unser aktuelles Datenset, was sich täglich aktualisiert. Stay tuned!"
                ),
                # Button
                downloadButton("downloadData", "Ich will die Daten!"),
                br(),
                br(),
                img(
                  src = "www/wir.png",
                  height = 100,
                  style = "display: block; margin-left: auto; margin-right: auto;"
                ),
                br()
              ),
              mainPanel(dataTableOutput("table"), width = 9)
            ))


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
               der UDE in Essen im Arbeitsbereich von Frau Prof. Heine.</h5>"
             )
           ),
           column(3)),
  
  fluidRow(column(
    12,
    align = "center",
    div(style = "display: inline-block;", img(src = "www/wir_baum.png", width =
                                                "50%"))
  )),
  
  
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

kontakttab <- fluidPage(
  # TEAM BIO
  fluidRow(column(3),
           column(
             6,
             shiny::HTML("<br><br><center> <h5>Das EveryData Team</h5> </center><br>"),
             shiny::HTML(
               "<h6><center>Das Team rund um <i>EveryData</i> arbeitet für und im Auftrag von <a href='https://www.uni-due.de/biwi/lls/angela_heine.php'>Frau Professor Heine</a>.</center></h6>"
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


vistab <- fluidPage(
  titlePanel(title = h4("Visualisierungen", align = "center")),
  sidebarLayout(sidebarPanel(
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
    ),
    width = 3
  ),
  mainPanel(plotOutput("hist"), width = 9)
)
)


ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  "EveryData",
  selected = "vis",
  tabPanel("Überblick", infotab),
  tabPanel("Daten", datentab),
  tabPanel("Visualisierungen", vistab, value = "vis"),
  tabPanel(
    "Übungen",
    "Sobald es mit den Übungen losgeht, findet Ihr hier Tips und Tricks... :)"
  ),
  tabPanel("Kontakt", kontakttab)
)


# Define server logic
server <- function(input, output) {
  datatable <- read.csv("init_food.csv", sep = ";")
  customcol <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                  "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
  output$hist <- renderPlot({
    category <- toString(input$category)
    if (category == "Zeit" | category == "Kosten"| category == "Geschmack"| category == "Fun")  {ggplot(data = datatable, aes_string(x = category)) + geom_histogram( bins=input$bins, fill = "#098474")} 
    else{ggplot(data = datatable, aes_string(x = category)) + geom_bar(stat="count", fill = "#098474") } 
    
    # qplot(datatable$category,
    #       geom="histogram",
    #       main = paste("Histogram for ", category), 
    #       xlab = category,  g
    #       fill=I("blue"), 
    #       col=I("red"), 
    #       alpha=I(.2))
  })
  
  output$table <- DT::renderDataTable({
    print(str(datatable))
    datatable
  },  rownames = FALSE, options = list(language = fr))
  
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      "food.csv"
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)