# Pfad zur CSV-Datei angeben
pfad_zur_datei <- "C:/Users/phili/Downloads/titanic_data.csv"

# CSV-Datei in ein Datenobjekt laden
titanic_data <- read.csv(pfad_zur_datei)

# Shiny-Paket laden
library(shiny)
library(DT)

# UI erstellen
ui <- fluidPage(
  
  # Seitentitel
  titlePanel("Titanic Überlebenschancen"),
  
  # Eingabe-Widgets
  sidebarLayout(
    sidebarPanel(
      
      # Kabinenklasse auswählen
      selectInput("pclass", "Kabinenklasse:",choices = c("All", "1", "2", "3"),selected = "All"),
      
      # Geschlecht auswählen
      selectInput("gender", "Geschlecht:",
                  choices = c("All", "male", "female"),
                  selected = "All"),
      
      # Alter auswählen
      sliderInput("age_range", "Alter:",
                  min = min(titanic_data$Age, na.rm = TRUE),
                  max = max(titanic_data$Age, na.rm = TRUE),
                  value = c(min(titanic_data$Age, na.rm = TRUE), max(titanic_data$Age, na.rm = TRUE)),
                  step = 1)
    ),
    
    # Ausgabe-Elemente
    mainPanel(
      
      # Anzahl der überlebenden und gestorbenen Passagiere
      textOutput("survived"),
      
      # Histogramm der Überlebenschancen basierend auf dem Alter
      plotOutput("age_histogram"),
      
      # Tabelle mit gefilterten Daten
      dataTableOutput("filtered_table")
    )
  )
)

# Server erstellen
server <- function(input, output) {
  
  # Filtern der Daten basierend auf den Benutzereingaben
  filtered_data <- reactive({
    data <- titanic_data
    
    # Filter nach Kabinenklasse
    if (input$pclass != "All") {
      data <- data[data$Pclass == as.numeric(input$pclass), ]
    }
    
    # Filter nach Geschlecht
    if (input$gender != "All") {
      data <- data[data$Sex == input$gender, ]
    }
    
    # Filter nach Alter
    data <- data[!is.na(data$Age), ]
    data <- data[data$Age >= input$age_range[1] & data$Age <= input$age_range[2], ]
    
    return(data)
  })
  
  # Anzahl der überlebenden und gestorbenen Passagiere berechnen
  output$survived <- renderText({
    data <- filtered_data()
    survived_count <- sum(data$Survived == 1)
    dead_count <- sum(data$Survived == 0)
    
    paste("Überlebt:", survived_count, "Gestorben:", dead_count)
  })
  
  # Histogramm der Überlebenschancen basierend auf dem Alter erstellen
  output$age_histogram <- renderPlot({
    data <- filtered_data()
    age <- data$Age
    survived <- data$Survived
    
    hist(age[survived == 1], breaks = 20, col = "green", main = "Überlebenschancen nach Alter", xlab = "Alter")
    hist(age[survived == 0], breaks = 20, col = "red", add = TRUE)
    legend("topright", c("Überlebt", "Gestorben"), fill = c("green", "red"))
  })
  
  # Tabelle mit gefilterten Daten erstellen
  output$filtered_table <- renderDataTable({
    data <- filtered_data()
    datatable(data)
  })
}

# Shiny-App erstellen
shinyApp(ui = ui, server = server)
