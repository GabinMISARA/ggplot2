# Installer les packages si vous ne les avez pas déjà
# install.packages("shiny")
# install.packages("ggplot2")

# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)

# Définir les données
data <- iris

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Clustering des données sur les iris"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Choisir l'axe X", choices = colnames(data)),
      selectInput("y_axis", "Choisir l'axe Y", choices = colnames(data)),
      numericInput('clusters', 'Nombre de clusters', 3, min = 1, max = 9),
      actionButton("increase_clusters", "+"),
      actionButton("decrease_clusters", "-")
    ),
    
    mainPanel(
      plotOutput('plot1')
    )
  )
)

# Serveur Shiny
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data[, c(input$x_axis, input$y_axis)]
  })
  
  clusters <- reactiveVal()
  
  observe({
    clusters(kmeans(selectedData(), input$clusters))
  })
  
  observeEvent(input$increase_clusters, {
    updateNumericInput(session, "clusters", value = input$clusters + 1)
  })
  
  observeEvent(input$decrease_clusters, {
    updateNumericInput(session, "clusters", value = max(1, input$clusters - 1))
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
