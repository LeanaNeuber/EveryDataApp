#addResourcePath(prefix = 'www', directoryPath = './www')

library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

#####################
day = 20
show_histo = 1
show_normaldist = 1
####################

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

normalverteilung <- fluidPage(
  fluidRow(tabsetPanel(
    tabPanel(
      "Normalverteilung verstehen lernen!",
      fluidRow(box(width = 12,
                   uiOutput("slider")), style = "padding-left:2%;padding-right:2%;padding-top:2%"),
      fluidRow(
        style = "padding-left:2%;padding-right:2%;padding-top:2%",
        column(
          3,
          titlePanel("Fun with Data!"),
          tags$hr(),
          fluidRow(column(6,
                          radioButtons(
                            'uni', 'Universität',
                            c("FU" = 'fu',
                              "UDE" = 'ude')
                          )),
                   column(
                     6,
                     conditionalPanel(
                       condition = "input.uni == 'fu'", radioButtons(
                         'gender',
                         'Geschlecht',
                         c(
                           "Alle" = "all",
                           "Weiblich" = "Female",
                           "Männlich" = "Male"
                         )
                       ))
                   )),
          column(
            width = 12,
            DT::dataTableOutput("dataview"),
            style = "overflow-x: scroll;"
          )
        ),
        
        column(
          9,
          box(
            width = NULL,
            status = "primary",
            plotlyOutput("main_plot")
          ),
          box(
            width = NULL,
            actionButton("add", "Füge 5 hinzu!"),
            actionButton("minus", "Entferne 5!")
          )
        )
      )
      
    ),
    tabPanel(
      "Normalverteilungen vergleichen!",
      
      fluidRow(
        style = "padding-left:2%;padding-right:2%;padding-top:2%",
        box(title = "Datenset 1",
            column(
              6,
              radioButtons('uni_base', 'Universität',
                           c("FU" = 'fu',
                             "UDE" = 'ude'), selected = "fu")
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.uni_base == 'fu'", radioButtons(
                  'gender_base',
                  'Geschlecht',
                  c(
                    "Alle" = "all",
                    "Weiblich" = "Female",
                    "Männlich" = "Male"
                  )
                ))
            )),
        box(title = "Datenset 2",
            column(
              6,
              radioButtons('uni_comp', 'Universität',
                           c("FU" = 'fu',
                             "UDE" = 'ude'), selected = "ude")
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.uni_comp == 'fu'", radioButtons(
                  'gender_comp',
                  'Geschlecht',
                  c(
                    "Alle" = "all",
                    "Weiblich" = "Female",
                    "Männlich" = "Male"
                  )
                ))
            )),
        
      ),
      fluidRow(
        style = "padding-left:2%;padding-right:2%;padding-top:2%",
        box(width = 12,
            status = "primary",
            plotlyOutput("comp_plot")
        )
      )
    )
  ))
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
             
             "<h5> Wir wollen euch mit dieser Web-App dabei unterstützen, Daten und die Darstellung von Daten besser zu verstehen. Das ist nämlich eine wichtige Grundlage für Diagnostik und kann euch dabei helfen, die Inhalte der Vorlesung besser nachzuvollziehen. 
             <br>
             <br>
             Dafür machen wir ein kleines Experiment, bei dem wir echte Daten sammeln.
             <br>
             <br> 
             Wir schreiben nämlich jeden Tag auf:
             <ul>
             <br>
             <li>ob wir selbst
               gekocht oder uns etwas bestellt haben</li>
             <li>wie teuer unser
               Essen war</li>
             <li>wie lange es
               gedauert hat</li>
             <li>wie gut das Essen
               geschmeckt hat</li>
             <li>wie viel Spaß wir
               dabei hatten</li>
             </ul>
             <br>
             <br>
             Diese Daten tragen wir in die Tabelle ein (Unterpunkt 'Daten' im linken Menü).
             <br>
             <br>
             Außerdem könnt ihr unter dem Punkt 'Hypothesen' (im linken Menü) sehen, ob unsere Annahmen stimmen oder nicht. Dafür müsst ihr nur die jeweilige Hypothese auswählen, dann seht ihr ein Balkendiagramm mit jeweils einem Balken für selbstgekocht, bestellt und Essen gegangen.
            <br>
             <br>
             Jeden Tag werden die Grafiken ein bisschen anders aussehen, weil schließlich immer mehr Daten dazukommen. Am Ende können wir dann ein Fazit ziehen. 
             <br>
             <br>
             Wir freuen uns, wenn ihr mitmacht!
             <br>
             <br>
             Ella, Lea und Katrin
             
             <h5>"
             )
           ),
           column(3)),
  
  fluidRow(style = "height:50px;"),
  
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
    # Ella
    column(4,
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "ella.jpeg",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Ella"),
                   tags$h6(
                     tags$i("Psychologin, Projektleiterin und Instagram-Beauftragte")
                   )),
               div(style = "width:10%;"),
               div("Ich bin Ella und bald fertig mit meinem Master in Psychologie. Ich interessiere mich für Unterschiede in der Motivation von verschiedenen Menschen. Derzeit beschäftige ich mich vor allem mit interindividuellen Unterschieden in moralischen Entscheidungsprozessen und was diese beeinflusst. Ich liebe fast alle Arten von Suppen!")
             )
           ),
    # Katrin
    column(4,
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "katrin.jpeg",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Katrin"),
                   tags$h6(
                     tags$i("Mensch-Computer Mensch und kreativer Kopf")
                   )),
               div(style = "height:10%;"),
               div("Ich bin Katrin, promoviere gerade am Lehrstuhl und beschäftige mich größtenteils mit 
               Child-Robot Interaction. Manchmal bin ich auch kreativ und arbeite gerne mit Studierenden zusammen. 
                   Ich liebe Sushi!")
             )
           ),
    # Lea
    column(4,
             div(
               class = "panel-body",
               width = "600px",
               align = "center",
               div(tags$img(
                 src = "lea.jpeg",
                 width = "50px",
                 height = "50px"
               )),
               div(tags$h5("Lea"),
                   tags$h6(
                     tags$i("Informatikerin und verantwortlich für die Shiny-App")
                   )),
               div(style = "height:10%;"),
               div(
                 "Ich bin Lea, habe Informatik studiert und arbeite mittlerweile als wissenschaftliche Mitarbeiterin
                 an der Uni in Essen. Wenn es um die Entwicklung von IT-Lösungen in interdisziplinären Teams geht 
                 bin ich immer gern dabei. Ich liebe koreanisches Essen!"
               )
             ),
           
     column(3)),
    
    fluidRow(),

    
    
    fluidRow(column(3),
             column(
               6,
               shiny::HTML("<br><br><center> <h1>Bereit loszulegen?</h1> </center><br>"),
               shiny::HTML(
                 "<h5>Schaut Euch doch mal die Visualisierungen an. Kann man jetzt schon sehen, ob unsere Annahmen stimmen?</h5>"
               )
             ),
             column(3)),
    fluidRow(style = "height:25px;"),
    fluidRow(column(3),
             column(6,
                    tags$div(
                      align = "center",
                      tags$a("Los geht's!",
                             onclick = "https://everydata.de",
                             class = "btn btn-primary btn-lg")
                    )),
             column(3)),
    fluidRow(style = "height:25px;")
    
  ))
  
  datentab <-
    fluidPage(sidebarLayout(
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
  
  umfrage <- fluidPage(
    fluidRow(box(width = 12, title = "Hypothesen",
        tabsetPanel(
          tabPanel("Geld", br(), "Beim selber kochen spart man Geld.", plotlyOutput("piegeld")),
          tabPanel("Zeit", br(), "Selber kochen dauert länger als sich etwas zum Essen zu bestellen.", plotlyOutput("piezeit")),
          tabPanel("Gesundheit", br(), "Selber kochen ist gesünder als Essen bestellen.",  plotlyOutput("piegesundheit")),
          tabPanel("Geschmack", br(), "Selber kochen schmeckt genauso gut wie Essen bestellen oder gehen.",  plotlyOutput("piegeschmack")),
          tabPanel("Spaß", br(), "Essen gehen macht mehr Spaß als selbst zu kochen.", plotlyOutput("piespass"))
        )))
  )
  
  hypothesen <- fluidPage(
    fluidRow(
      box(width = 12, title = "Hypothesen",
          tabsetPanel(
            tabPanel("Beim selber kochen spart man Geld.", plotOutput("teuer")),
            tabPanel("Essen gehen oder bestellen geht schneller.",  plotOutput("zeit")),
            tabPanel("Selber kochen ist gesünder.",  plotOutput("gesundheit")),
            tabPanel("Selber kochen schmeckt genauso gut wie Essen bestellen oder gehen.",  plotOutput("geschmack")),
            tabPanel("Essen gehen macht mehr Spaß.",  plotOutput("fun"))
          ))
    ),
    
    fluidRow(
      box(
        width = 4,
        h3("Ella"),
        infoBoxOutput("EssenElla", width = 12),
        infoBoxOutput("ZeitElla", width = 12),
        infoBoxOutput("AusgabenElla", width = 12)
      ),
      box(
        width = 4,
        h3("Katrin"),
        infoBoxOutput("EssenKatrin", width=12),
        infoBoxOutput("ZeitKatrin", width = 12),
        infoBoxOutput("AusgabenKatrin", width = 12)
      ),
      box(
        width = 4,
        h3("Lea"),
        infoBoxOutput("EssenLea" , width = 12),
        infoBoxOutput("ZeitLea", width = 12),
        infoBoxOutput("AusgabenLea", width = 12)
      )
  ))
  
  histo <- fluidPage(
    fluidRow(
      box(width=6,title = "", plotOutput("plot1")),
      box(
        title = "Merkmal",
        radioButtons(
          "category",
          "Wähle eine Spalte:",
          choices = c("Kategorie", "Kosten", "Zeit", "Geschmack", "Fun")
        )
      ), 
    ),
    fluidRow(
      box(width=12, title= "Wähle einen Tag",
          sliderInput("dayslide", "Achtung: Die Daten des Dashboards werden auch angepasst!",
                      min = 0, max = day, value = day, step = 1, ticks = TRUE,
          )),
    )
  )
  
  ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(
      title = "EveryData",
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
          text = ("Prof. Heine: angela.heine@uni-due.de"),
          icon = icon("chalkboard-teacher")
        )
      )
    ),
    dashboardSidebar(sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("tachometer-alt")
      ),
      menuItem("Hypothesen", tabName = "hypothesen", icon = icon("lightbulb")),
      menuItem("Eure Umfrage", tabName = "umfrage", icon = icon("poll-h")),
      if(show_histo) menuItem("Entwicklung über die Zeit", tabName = "histo", icon=icon("chart-bar")),
      menuItem("Daten", tabName = "data", icon = icon("table")),
      if(show_normaldist) menuItem("Exkurs: Normalverteilung", tabName = "normalverteilung", icon = icon("balance-scale-left")),
      # menuItem("Übungen", tabName = "exercises", icon = icon("award")),
      menuItem("Info", tabName = "info", icon = icon("question"))
    )),
    dashboardBody(tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        # fluidRow(
        #   box(width=12,
        #       sliderInput("dayslide", "Wähle einen Tag:",
        #                   min = 0, max = day, value = day, step = 1, ticks = TRUE,
        #       )),
        # ),
        fluidRow(
          infoBoxOutput("Essen"),
          infoBoxOutput("Zeit"),
          infoBoxOutput("Ausgaben")
        ),
        
        fluidRow(
          box(title = "Wer hat was gegessen? Klicke auf einen Balken im Diagram um es zu sehen!", plotOutput("essen_overall", click="hist_click")),
          # box(verbatimTextOutput("x_value"),
          #     verbatimTextOutput("selected_rows")),
          box(title= "Hier das leckere Essen:", textOutput("foodname"), plotOutput("foodimg"), align="center")
        )
      ),
      
      tabItem(tabName= "hypothesen", hypothesen),
      tabItem(tabName= "umfrage", umfrage),
      tabItem(tabName="histo", histo),
      # Second tab content
      tabItem(tabName = "data",
              datentab),
      tabItem(tabName = "normalverteilung", normalverteilung),
      
      tabItem(
        tabName = "exercises",
        "Sobald es mit den Übungen losgeht, findet Ihr hier Tips und Tricks... :)"
      ),
      tabItem("info", infotab)
    )))
  
  server <- function(input, output, session) {
    answers = c('Ja', 'Nein')
    
    #######NORMALVERTEILUNG###########
    df <- read.csv("heights_fu.csv", sep = ",")
    df_ude <- read.csv("heights_ude.csv", sep=",")
    df['highlight'] <- 'no'
    df_ude['highlight'] <- 'no'
    
    original <- reactiveValues(df = df[FALSE, ])
    compare <- reactiveValues(df = df[FALSE, ])
    values_original <- reactiveValues(datatable = df[FALSE, ])
    values_comp <- reactiveValues(datatable = df[FALSE, ])
    updated <-
      reactiveValues(dataframe = df[FALSE, ], subset = NULL)
    #################################
    
    count_geld = c(328,29)
    count_zeit = c(194,142)
    count_gesundheit = c(265,64)
    count_geschmack = c(215,105)
    count_spass=c(151,175)
    df_geld = data.frame(answers,count_geld)
    df_zeit = data.frame(answers,count_zeit)
    df_gesundheit = data.frame(answers,count_gesundheit)
    df_geschmack = data.frame(answers,count_geschmack)
    df_spass = data.frame(answers,count_spass)
    
    values <- reactiveValues(datatable = read.csv(paste("food_data/food_",0,".csv", sep=""), sep = ";"))
    
    observeEvent(input$dayslide, {
      values$datatable <- read.csv(paste("food_data/food_",input$dayslide,".csv", sep=""), sep = ";")
    })
    
    output$piegeld <- renderPlotly({
      colors <- c('#a8eeee', '#fde86d')
      plot_ly(df_geld, labels = answers, values = count_geld, type = 'pie', marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1)))
    })
    
    output$piezeit <- renderPlotly({
      colors <- c('#ab84a3', '#1e4d4c')
      plot_ly(df_zeit, labels = answers, values = count_zeit, type = 'pie', marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1)))
    })
    
    output$piegesundheit <- renderPlotly({
      colors <- c('#a5991e', '#f2facc')
      plot_ly(df_gesundheit, labels = answers, values = count_gesundheit, type = 'pie', marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1)))
    })
    
    output$piegeschmack <- renderPlotly({
      colors <- c('#666bc8', '#ac1454')
      plot_ly(df_geschmack, labels = answers, values = count_geschmack, type = 'pie', marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1)))
    })
    
    output$piespass <- renderPlotly({
      colors <- c('#42933d', '#f79830')
      plot_ly(df_spass, labels = answers, values = count_spass, type = 'pie', marker = list(colors = colors,
                                                                                          line = list(color = '#FFFFFF', width = 1)))
    })
    
    output$Essen <-
      renderInfoBox({
        infoBox(
          "Essen",
          nrow(values$datatable),
          icon = icon("hamburger"),
          color = "purple",
          fill = TRUE
        )
      })
    
    output$Zeit <-
      renderInfoBox({
        infoBox("Minuten",
                sum(values$datatable$Zeit),
                icon = icon("clock"),
                fill = TRUE)
      })
    output$Ausgaben <-
      renderInfoBox({
        infoBox(
          "Euro",
          sum(values$datatable$Kosten),
          icon = icon("euro-sign"),
          color = "yellow",
          fill = TRUE
        )
      })
    output$EssenElla <-
      renderInfoBox({
        infoBox(
          "Essen",
          nrow(values$datatable[which(values$datatable$Name == "Ella"), ]),
          icon = icon("hamburger"),
          color = "teal"        )
      })
    
    output$ZeitElla <-
      renderInfoBox({
        infoBox("Minuten",
                sum(values$datatable[which(values$datatable$Name == "Ella"), ]$Zeit),
                icon = icon("clock"),color="teal")
      })
    output$AusgabenElla <-
      renderInfoBox({
        infoBox(
          "Euro",
          sum(values$datatable[which(values$datatable$Name == "Ella"), ]$Kosten),
          icon = icon("euro-sign"),
          color = "teal"
        )
      })
    output$EssenKatrin <-
      renderInfoBox({
        infoBox(
          "Essen",
          nrow(values$datatable[which(values$datatable$Name == "Katrin"), ]),
          icon = icon("hamburger"),
          color = "light-blue",
        )
      })
    
    output$ZeitKatrin <-
      renderInfoBox({
        infoBox("Minuten",
                sum(values$datatable[which(values$datatable$Name == "Katrin"), ]$Zeit),
                icon = icon("clock"), color = "light-blue"
                )
      })
    output$AusgabenKatrin <-
      renderInfoBox({
        infoBox(
          "Euro",
          sum(values$datatable[which(values$datatable$Name == "Katrin"), ]$Kosten),
          icon = icon("euro-sign"),
          color = "light-blue",
        )
      })
    output$EssenLea <-
      renderInfoBox({
        infoBox(
          title = "Essen",
          nrow(values$datatable[which(values$datatable$Name == "Lea"), ]),
          icon = icon("hamburger"),
          color = "navy"
        )
      })
    
    output$ZeitLea <-
      renderInfoBox({
        infoBox("Minuten",
                sum(values$datatable[which(values$datatable$Name == "Lea"), ]$Zeit ),
                icon = icon("clock"),color="navy")
      })
    output$AusgabenLea <-
      renderInfoBox({
        infoBox(
          "Euro",
          sum(values$datatable[which(values$datatable$Name == "Lea"), ]$Kosten ),
          icon = icon("euro-sign"),
          color = "navy"
        )
      })
    output$table <- DT::renderDataTable({
      
      subset(values$datatable, select=-c(Foto,Tag))
    },  rownames = FALSE, options = list(language = de))
    
    # Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
      filename = function() {
        "food_data/food.csv"
      },
      content = function(file) {
        write.csv(values$datatable, file, row.names = FALSE)
      }
    )
    output$plot1 <-  renderPlot({
      if (input$category == "Zeit")  {
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_histogram(bins =
                                                                                    30,
                                                                                  fill = "#098474", color="black")+ylab("Anzahl der Essen")+xlab("Zeit in Minuten")+theme(axis.text=element_text(size=12),
                                                                                                                                                                          axis.title=element_text(size=12))
      }
      else if (input$category == "Kosten")  {
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_histogram(bins =
                                                                                           30,
                                                                                         fill = "#098474", color="black")+ylab("Anzahl der Essen")+xlab("Kosten in €")+theme(axis.text=element_text(size=12),
                                                                                                                                                                             axis.title=element_text(size=12))
      }
      else if (input$category == "Geschmack")  {
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_histogram(bins =
                                                                                           30,
                                                                                         fill = "#098474", color="black")+ylab("Anzahl der Essen")+xlab("Geschmack von 1 bis 10")+theme(axis.text=element_text(size=12),
                                                                                                                                                                                        axis.title=element_text(size=12))
      }
      else if (input$category == "Fun")  {
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_histogram(bins =
                                                                                           30,
                                                                                         fill = "#098474", color="black")+ylab("Anzahl der Essen")+xlab("Fun von 1 bis 10")+theme(axis.text=element_text(size=12),
                                                                                                                                                                                  axis.title=element_text(size=12))
      }
      # else if(input$category == "Name"){ggplot(data = datatable, aes(x = Name, fill= Gericht)) + theme(legend.position = "none") + geom_bar(stat = "count")+ylab("Anzahl")}
      else{
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_bar(stat = "count", fill= "#098474", color="black")+ylab("Anzahl der Essen")+xlab("Kategorie (bestellt, gekocht?)")+theme(axis.text=element_text(size=12),
                                                                                                                                                                                                         axis.title=element_text(size=12))
      }
    })
    output$essen_overall <- renderPlot({
      ggplot(data = values$datatable, aes(x = Name, fill= Gericht)) + theme(legend.position = "none") + geom_bar(stat = "count")+ylab("Anzahl aller Essen")
      
    })
    
    output$pie1 <- renderPlot({
      by_fun <- values$datatable %>% group_by(Fun) %>% summarise(count=n())
      if(input$dayslide > 0){
        ggplot(by_fun, aes(x = "", y=count, fill = as.factor(Fun))) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar(theta = "y", start = 0)+
          theme_void() +
          theme(legend.position = "right") +
          guides(fill = guide_legend(title = "Fun"))
      }
    })
    
    output$teuer <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Kosten, fill= Kategorie))+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("") +
        ylab("Durchschnittliche Kosten") +theme(axis.text=element_text(size=12),
                                                axis.title=element_text(size=12))
    })
    
    output$foodname <- renderText({ 
      if (is.null(input$hist_click$x)) return(NULL)
      if (is.null(input$hist_click$y)) return(NULL)
      
      y <- round(input$hist_click$y-0.5)
      lvls <- levels(factor(values$datatable$Name))
      roundy <- round(input$hist_click$x)
      if (roundy > 3) {
        return(NULL)
      }
      if (roundy < 1) return(NULL)
      x <- lvls[roundy]
      subset_on_x <- subset(values$datatable, Name==x)
      rownames(subset_on_x) <- NULL
      max_row = nrow(subset_on_x) -1
      if (y > max_row) y = max_row
      #print(subset_on_x[y + 1, "Gericht"])
      return (toString(subset_on_x[y + 1, "Gericht"]))
      })
    
    
    output$foodimg <- renderImage({
      if (is.null(input$hist_click$x)) return(list(src = "", width = "auto",
                                                   height = "100%", align = "center"))
      if (is.null(input$hist_click$y)) return(list(src = "", width = "auto",
                                                   height = "100%", align = "center"))
      y <- round(input$hist_click$y-0.5)
      lvls <- levels(factor(values$datatable$Name))
      roundy <-round(input$hist_click$x)
      if (roundy > 3 || roundy < 1) return(list(src = "", width = "auto", height = "100%", align = "center"))
      x <- lvls[roundy]
      subset_on_x <- subset(values$datatable, Name==x)
      max_row = nrow(subset_on_x) -1
      if (y > max_row) y <- max_row 
      food_name <- subset_on_x[y + 1, "Foto"]
    
      #filename <- normalizePath(file.path('./www', food_name))
      list(src = paste("www/",food_name, sep=""), width = "auto",
           height = "100%", align = "center")
    }, deleteFile=FALSE)
    
    
    output$zeit <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Zeit, fill= Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("") +
        ylab("Durchschnittliche Zeit") +theme(axis.text=element_text(size=12),
                                              axis.title=element_text(size=12))
    })
    
    output$gesundheit <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Kalorien, fill= Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("") +
        ylab("Durchschnittliche Kalorien")+theme(axis.text=element_text(size=12),
                                                 axis.title=element_text(size=12))
    })
    
    output$geschmack <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Geschmack, fill= Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("") +
        ylab("Durchschnittlicher Geschmack")+theme(axis.text=element_text(size=12),
                                                   axis.title=element_text(size=12))
    })
    
    output$fun <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Fun, fill=Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean)  + 
        xlab("") +
        ylab("Durchschnittliches Fun-Rating") +theme(axis.text=element_text(size=12),
                                                     axis.title=element_text(size=12))
    })
    
    
    #########################Normalverteilung#########################
    df <- read.csv("heights_fu.csv", sep = ",")
    df_ude <- read.csv("heights_ude.csv", sep = ",")
    df['highlight'] <- 'no'
    df_ude['highlight'] <- 'no'
    
    base <- reactiveValues(df = df[FALSE,])
    comp <- reactiveValues(df = df[FALSE,])
    
    original <- reactiveValues(df = df[FALSE,])
    values_original <- reactiveValues(datatable = df[FALSE,])
    
    updated <-
      reactiveValues(dataframe = df[FALSE,], subset = NULL)
    
    observeEvent(input$uni_base, {
      if (input$uni_base == "fu") {
        base$df <- df
        updateRadioButtons(session, 'gender_base',  selected = "all")
        
      }
      else{
        base$df <- df_ude
        updateRadioButtons(session, 'gender_base',  selected = "all")
      }
    })
    
    observeEvent(input$uni_comp, {
      if (input$uni_comp == "fu") {
        comp$df <- df
        updateRadioButtons(session, 'gender_comp',  selected = "all")
      }
      else{
        comp$df <- df_ude
        updateRadioButtons(session, 'gender_comp',  selected = "all")
      }
    })
    
    observeEvent(input$gender_base, {
      if ((input$gender_base == "all") & (input$uni_base == "fu") ) {
        base$df <- df
      }
      else if ((input$gender_base == "all") & (input$uni_base == "ude") ) {
        base$df <- df_ude
      }
      else if ((input$gender_base != "all") & (input$uni_base == "fu") ) {
        base$df <- filter(df, Geschlecht == input$gender_base)
      }
      else if ((input$gender_base != "all") & (input$uni_base == "ude") ){
        base$df <- filter(df_ude, Geschlecht == input$gender_base)
      }
    })
    
    observeEvent(input$gender_comp, {
      if ((input$gender_comp == "all") & (input$uni_comp == "fu") ) {
        comp$df <- df
      }
      else if ((input$gender_comp == "all") & (input$uni_comp == "ude") ) {
        comp$df <- df_ude
      }
      else if ((input$gender_comp != "all") & (input$uni_comp == "fu") ) {
        comp$df <- filter(df, Geschlecht == input$gender_comp)
      }
      else if ((input$gender_comp != "all") & (input$uni_comp == "ude") ){
        comp$df <- filter(df_ude, Geschlecht == input$gender_comp)
      }
    })
    
    
    observeEvent(input$uni, {
      if (input$uni == "fu") {
        original$df <- df
        values_original$datatable <- original$df
        updateRadioButtons(session, 'gender', selected = "all")
      }
      else{
        original$df <- df_ude
        values_original$datatable <- original$df
        updateRadioButtons(session, 'gender',  selected = "all")
      }
    })
    
    observeEvent(input$gender, {
      updated$subset <- NULL
      if (input$gender == "all") {
        values_original$datatable <- original$df
      }
      else{
        values_original$datatable <-
          filter(original$df, Geschlecht == input$gender)
      }
    })
    
    observeEvent(input$inputslider, {
      updated$dataframe <-
        values_original$datatable[0:input$inputslider,]
    })
    
    observeEvent(input$add, {
      if (input$inputslider < nrow(values_original$datatable) - 1) {
        updateSliderInput(session, "inputslider", value = input$inputslider + 5)
        values_original$datatable['highlight'] = 'no'
        values_original$datatable[(input$inputslider + 1):(input$inputslider + 5),]['highlight'] = 'yes'
        updated$subset <-
          values_original$datatable[(input$inputslider + 1):(input$inputslider + 5),]
      }
    })
    
    observeEvent(input$minus, {
      if (input$inputslider > 0) {
        values_original$datatable['highlight'] = 'no'
        updateSliderInput(session, "inputslider", value = input$inputslider - 5)
        updated$subset <- NULL
      }
    })
    
    output$slider <- renderUI({
      sliderInput(
        "inputslider",
        "Füge Datenpunkte hinzu:",
        min = 0,
        max = nrow(values_original$datatable),
        value = 0,
        step = 10,
        ticks = TRUE
      )
    })
    
    output$main_plot <- renderPlotly({
      if (dim(updated$dataframe)[1] == 0)
        return(NULL)
      gg <-
        ggplot(updated$dataframe, aes(
          x = Größe,
          text = paste0("Größe: ", x, ", Anzahl: ", ..count..)
        )) +
        geom_histogram(
          data = updated$dataframe,
          aes(fill = highlight),
          alpha = 0.5,
          binwidth = 1
        ) +
        geom_density(data = updated$dataframe,
                     aes(y = ..count..),
                     alpha = 0.5) +
        ylab("Anzahl") +
        xlab("Größe in cm")  + theme(legend.title = element_blank()) + theme(legend.position = "none") + ggtitle("Größenverteilung")
      ggplotly(gg, tooltip = "text")
      
    })
    
    output$comp_plot <- renderPlotly({
      gg <-
        ggplot(base$df, aes(
          x = Größe,
          text = paste0("Größe: ", x, ", Anzahl: ", ..count..)
        )) +
        geom_histogram(
          aes(fill='Datenset 1'),
          data = base$df,
          alpha = 0.5,
          binwidth = 1
        ) +
        geom_histogram(
          aes(fill='Datenset 2'),
          data = comp$df,
          alpha = 0.7,
          binwidth = 1
        ) +
        geom_density(data = base$df,
                     aes(y = ..count..),
                     alpha = 0.7, color = '#00798c') +
        geom_density(data = comp$df,
                     aes(y = ..count..),
                     alpha = 0.7, color='#edae49') +
        ylab("Anzahl") +  
        xlab("Größe in cm")  + theme(legend.title = element_blank()) + ggtitle("Größenverteilungen") + scale_fill_manual(values=c('#00798c','#edae49'), name = NULL)
      ggplotly(gg, tooltip = "text")
      
    })
    
    output$dataview <- DT::renderDataTable({
      datatable(
        updated$subset[c("ID", "Größe")],
        options = list(paging = FALSE, searching = FALSE),
        rownames = FALSE
      )
    })
    ############################################
  }
  options(warn = -1) # ignore plotly weird warning
  
  shinyApp(ui, server)
