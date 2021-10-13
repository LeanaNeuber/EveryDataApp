#addResourcePath(prefix = 'www', directoryPath = './www')

library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

#####################
day = 20
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
             
             "<h5> Wir wollen euch mit dieser Web-App dabei unterstützen, Daten und die 
             Darstellung von Daten besser zu verstehen. Das ist nämlich eine
             wichtige Grundlage für Diagnostik und kann euch dabei helfen, die
             Inhalte der Vorlesung besser nachzuvollziehen.
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
             <a href='https://everydata.de'>Hier</a> könnt ihr die Entwicklung der Daten mitverfolgen. 
             Unter „Merkmale“ könnt ihr die jeweiligen Merkmale (Kategorie, Kosten, Zeit, Geschmack, Fun) 
             einzeln auswählen und schauen, wie sich die Datensammlung entwickelt.
             <br>
             <br>
             Wenn ihr Kategorie auswählt, seht ihr, wie viele Gerichte selbst gekocht oder bestellt wurden.
             <br>
             Wenn ihr Kosten auswählt, seht ihr, wie viele Gerichte wie viel gekostet haben. 
             <br>
             Wenn ihr Zeit auswählt, sehr ihr, wie viele Gerichte wie lange gedauert haben. 
             <br>
             Wenn ihr Geschmack auswählt, sehr ihr, wie viele Gerichte wie gut geschmeckt haben.
             <br>
             Wenn ihr Fun auswählt, seht, ihr, wie viele Gerichte wie viel Spaß gemacht haben.
             <br>
             <br>
             Außerdem könnt ihr im linken Menü 'Hypothesen' sehen, ob unsere Annahmen stimmen oder nicht. 
             Jeden Tag werden die Grafiken ein bisschen anders aussehen, weil schließlich immer 
             mehr Daten dazukommen und am Ende können wir dann ein Fazit ziehen. 
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
  
  hypothesen <- fluidPage(
    fluidRow(
      # box(title = "Wie steht's um den Spaß?", plotOutput("pie1")),
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
        icon = icon("chart-bar")
      ),
      menuItem("Hypothesen", tabName = "hypothesen", icon = icon("lightbulb")),
      menuItem("Daten", tabName = "data", icon = icon("table")),
      # menuItem("Übungen", tabName = "exercises", icon = icon("award")),
      menuItem("Info", tabName = "info", icon = icon("question"))
    )),
    dashboardBody(tabItems(
      # First tab content
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(width=12,
              sliderInput("dayslide", "Wähle einen Tag:",
                          min = 0, max = day, value = day, step = 1, ticks = TRUE,
              )),
        ),
        fluidRow(
          infoBoxOutput("Essen"),
          infoBoxOutput("Zeit"),
          infoBoxOutput("Ausgaben")
        ),
        
        fluidRow(
          box(title = "Eine Übersicht", plotOutput("essen_overall", click="hist_click")),
          # box(verbatimTextOutput("x_value"),
          #     verbatimTextOutput("selected_rows")),
          box(title= "Hier das leckere Essen - klicke auf eine der Säulen in der Übersicht: ", textOutput("foodname"), plotOutput("foodimg"), align="center")
        ),
        fluidRow(                                                                         
          box(width=6,title = "Histogram", plotOutput("plot1")),
          box(
            title = "Merkmal",
            radioButtons(
              "category",
              "Wähle eine Spalte:",
              choices = c("Kategorie", "Kosten", "Zeit", "Geschmack", "Fun")
            ),
            conditionalPanel(
              "input.category == 'Zeit' | input.category == 'Kosten'| input.category == 'Geschmack'| input.category == 'Fun'",
              sliderInput(
                inputId = "bins",
                label = "Anzahl der Balken:",
                min = 1,
                max = 30,
                value = 3
              )
            )
          ), 
          box(title = "Wie steht's um den Spaß?", plotOutput("pie1"))
        ),
        fluidRow()
      ),
      
      tabItem(tabName= "hypothesen", hypothesen),
      # Second tab content
      tabItem(tabName = "data",
              datentab),
      
      tabItem(
        tabName = "exercises",
        "Sobald es mit den Übungen losgeht, findet Ihr hier Tips und Tricks... :)"
      ),
      tabItem("info", infotab)
    )))
  
  server <- function(input, output) {
    
    values <- reactiveValues(datatable = read.csv(paste("food_",1,".csv", sep=""), sep = ";"))
    
    observeEvent(input$dayslide, {
      values$datatable <- read.csv(paste("food_",input$dayslide,".csv", sep=""), sep = ";")
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
        "food.csv"
      },
      content = function(file) {
        write.csv(values$datatable, file, row.names = FALSE)
      }
    )
    output$plot1 <-  renderPlot({
      if (input$category == "Zeit" |
          input$category == "Kosten" |
          input$category == "Geschmack" |
          input$category == "Fun")  {
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_histogram(bins =
                                                                                    input$bins,
                                                                                  fill = "#098474")+ylab("Anzahl")
      }
      # else if(input$category == "Name"){ggplot(data = datatable, aes(x = Name, fill= Gericht)) + theme(legend.position = "none") + geom_bar(stat = "count")+ylab("Anzahl")}
      else{
        ggplot(data = values$datatable, aes_string(x = input$category)) + geom_bar(stat = "count", fill= "#098474")+ylab("Anzahl")
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
        xlab("Kategorie") +
        ylab("Durchschnittliche Kosten")
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
      print('--------------')
      print(x)
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
        xlab("Kategorie") +
        ylab("Durchschnittliche Zeit")
    })
    
    output$gesundheit <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Kalorien, fill= Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("Kategorie") +
        ylab("Durchschnittliche Kalorien")
    })
    
    output$geschmack <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Geschmack, fill= Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean) +
        xlab("Kategorie") +
        ylab("Durchschnittlicher Geschmack")
    })
    
    output$fun <- renderPlot({
      ggplot(data = values$datatable, aes(x = Kategorie, y = Fun, fill=Kategorie)) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
        geom_bar(stat = "summary", fun = mean)  + 
        xlab("Kategorie") +
        ylab("Durchschnittliches Fun-Rating") 
    })
  }
  
  shinyApp(ui, server)