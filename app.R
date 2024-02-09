# Installer les packages si vous ne les avez pas déjà
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("jpeg")
# install.packages("scales")

# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(jpeg)
library(scales)

# Définir les données
data <- iris

# Charger l'image en dehors de Shiny
iris_image <- readJPEG("iris.jpeg")

# Définir la transparence initiale (par exemple, 0.6 pour 60% de transparence)
transparency <- 0.6

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Clustering des données sur les iris"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Choisir l'axe X", choices = colnames(data)),
      selectInput("y_axis", "Choisir l'axe Y", choices = colnames(data)),
      numericInput('clusters', 'Nombre de clusters', 3, min = 1, max = 15),
      sliderInput('transparency_slider', 'Transparence de l\'image', min = 0, max = 1, value = transparency, step = 0.1),
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
    
    # Tracer l'image d'iris en fond avec la transparence
    ggplot(selectedData(), aes_string(x = input$x_axis, y = input$y_axis)) +
      annotation_custom(rasterGrob(iris_image, interpolate = TRUE, alpha = input$transparency_slider), xmin = -Inf, xmax = Inf, ymin = -Inf, 
                        