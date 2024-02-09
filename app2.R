# Installer les packages si vous ne les avez pas déjà
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("jpeg")
# install.packages("grid")

# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(jpeg)
library(grid)

# Définir les données
data <- iris

# Charger l'image en dehors de Shiny
iris_image <- readJPEG("iris.jpeg")

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Clustering des données sur les iris"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Choisir l'axe X", choices = colnames(data)),
      selectInput("y_axis", "Choisir l'axe Y", choices = colnames(data)),
      numericInput('clusters', 'Nombre de clusters', 3, min = 1, max = 15),
      actionButton("increase_clusters", "+"),
      actionButton("decrease_clusters", "-")
    ),
    
    mainPanel(
      plotOutput('plot1'),
      textOutput("cluster_warning")
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
    
    # Tracer l'image d'iris en fond 
    ggplot(selectedData(), aes_string(x = input$x_axis, y = input$y_axis)) +
      annotation_custom(grid::rasterGrob(iris_image, interpolate = TRUE), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      geom_point(aes(color = as.factor(clusters()$cluster)), pch = 20, cex = 3) +
      geom_point(data = as.data.frame(clusters()$centers), aes_string(x = input$x_axis, y = input$y_axis), pch = 4, cex = 4, color = "black", stroke = 2) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(
        title = paste("Nuage de points :", input$x_axis, "vs", input$y_axis),
        x = input$x_axis,
        y = input$y_axis
      )
  })
  
  output$cluster_warning <- renderText({
    if (input$clusters > 10) {
      return("Attention : Le nombre de clusters dépasse 10.")
    } else {
      return(NULL)
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

