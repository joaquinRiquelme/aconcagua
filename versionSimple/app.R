# template de app simple grafico y opciones de filtro
# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(iris), "Species")
bd <- read.csv("bd_clusterizado.csv")
bd$seccion <- gsub(pattern = "_", replacement = ".",bd$seccion)
bd$grupo.etario <- NA
bd$grupo.etario[bd$edad<30] <- "18-29"
bd$grupo.etario[bd$edad>=30 & bd$edad<60] <- "30-59"
bd$grupo.etario[bd$edad>=60] <- "+60"
bd <- bd[order(bd$seccion),]

vars <- names(bd)

ui <- pageWithSidebar(
  headerPanel('Análisis cluster caracterización de agricultores cuenca del Aconcagua'),
  sidebarPanel(
    selectInput('seccion', 'Seccion', unique(bd$seccion), selected = unique(bd$seccion)[1]),
    selectInput('etario', 'Grupo etario',  unique(bd$grupo.etario), selected = unique(bd$grupo.etario)[1]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    bd[, c( "PCA.Axis1","PCA.Axis2","typo")]
  })
  
  clusters <- reactive({
    selectedData()[,c("PCA.Axis1","PCA.Axis2")]
  })
  
  output$plot1 <- renderPlot({
    palette(c("#000000","#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData()[,c("PCA.Axis1","PCA.Axis2")],
         col = as.numeric(as.factor(selectedData()$typo)),
         pch = 20, cex = 3)
    # points(x=clusters()$PCA.Axis1,y=clusters()$PCA.Axis2, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui, server)
