# Preambulo ----
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(nycflights13)
library(histoslider)
library(rlang)
library(htmltools)
library(rmarkdown)
library(leaflet)
library(leaflet.extras)


parametros <- list(
  color = "#68B47D",
  round_digits = 1,
  font_family = "Raleway",
  font_family_code = "Source Code Pro",
  tabla_datos = "estaciones_datos",
  tabla_estaciones = "estaciones",
  paleta = c("#730000","#E60000","#FFAA00","#FFD37F","#FFFF00","#FFFFFF",
             "#8CCDEF","#00BFFF","#1D90FF","#4169E1","#0000FF"),
  etiquetas = c("Sequía excepcional", "Sequía extrema", "Sequía severa",
                "Sequía moderada", "Anormalmente seco","Normal",
                "Anormalmente húmedo","Moderadamente húmedo","Severamente húmedo",
                "Extramademente húmedo", "Excepcionalmente húmedo")
)

paleta.odes <- c("#68B47D", "#7EC07F", "#03674E", "#04559F", "#048ABF",
                 "#A63E26", "#AB4924", "#F2A74B", "#962C0C", "#F2EDD0",
                 "#9A692E", "#49A671", "#E8AF47", "#048ABF", "#F7ECC9")

paleta.sensores <- c("#9A692E","#F2A74B","#04559F", "#048ABF","#03674E","#7EC07F")
paleta.sensores.humedad <- c("#9A692E","#04559F", "#03674E")
paleta.socioeconomico <- c("1"="#04559F","2"="#03674E","3"="#9A692E","4"="#A63E26")


theme_odes <- bs_theme(#bs_theme(),
  version = 5,
  # bg = "#fff", fg = "#000",
  primary = "#7EC07F",
  secondary="#1F0E68",
  # navbar-bg = secondary,
  # parametros$color,
  base_font = font_google(parametros$font_family),
  code_font = font_google(parametros$font_family_code)
)

# Datos ----
## Biodiversidad ----
data.inicial <- read.csv("Biodiversidad-Topografia.csv")
data.inicial$Longitud <- round(data.inicial$Longitud,2)
data.inicial$Latitud <- round(data.inicial$Latitud,2)
biodiversidad <- tibble(data.inicial[,c("Sitio","Riqueza","Shannon","Simpson")])
Sitio.b <- tibble(data.inicial[,c("Sitio","Longitud","Latitud")])
Topografica <- tibble(data.inicial[c("Sitio","Elevación","Pendiente.Porcentaje","Exposición")])
PRIMARY <- "#68B47D"
biodiversidad <- biodiversidad %>%
  left_join(Sitio.b) %>%
  left_join(Topografica)

# print(biodiversidad)
# print(summary(biodiversidad))
plot(biodiversidad[,c("Riqueza","Shannon","Simpson","Elevación","Pendiente.Porcentaje","Exposición")])


## Sensores ----
load("sensores.RData")
names(sensores)
sensores <- sensores %>% group_by(Categoria, Fecha, Sitio, Latitud,Longitud) %>% 
  summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
            Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
            Humedad.suelo = mean(Humedad.suelo, na.rm = T))

sensores.dia <- sensores %>% group_by(Categoria, Fecha) %>% 
  summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
            Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
            Humedad.suelo = mean(Humedad.suelo, na.rm = T))
summary(sensores.dia)

# sensores <- full_join(biodiversidad, sensores, by="Sitio")
# sensores$Latitud <- sensores$Latitud.x
# sensores$Longitud <- sensores$Longitud.x
# sensores$Categoria <- sensores$Categoria.x
# biodiversidad <- sensores

## Socieconomico ----
clusterizado <- read.csv("bd_clusterizado.csv")
clusterizado$seccion2 <- NA
clusterizado$seccion2[clusterizado$seccion=="primera_seccion"] <- "Alta"
clusterizado$seccion2[clusterizado$seccion=="segunda_seccion"] <- "Media"
clusterizado$seccion2[clusterizado$seccion=="tercera_seccion"] <- "Baja"
clusterizado$seccion2 <- factor(x=clusterizado$seccion2, levels = c("Baja","Media","Alta"))
# head(clusterizado)
var.socio <- read.csv("variables_socioeconomico.csv")

cuenca <- sf::st_read("cuencaAconcagua.shp")
plot(cuenca)

# ui ----
ui <-page_navbar(
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "horizontal_SB_blanco.png", height = "30px", style = "margin-top: -5px"),
      href = "https://odes-chile.org/"
    ),
    "Aconcagua"
  ),
  id = "nav",
  lang = "es",
  theme = theme_odes,
  fillable = TRUE,
  fillable_mobile = TRUE,
  ## sidebar -----
  sidebar = sidebar(
    width = 400,
  #   selectInput("unidad", tags$small("Sitio"), opt_unidad),
  #   selectInput("macrozona", tags$small("Macrozona"), opt_macrozona, multiple = FALSE), # selected = "zona central",
  #   selectInput("variable", tags$small("Variable"), opt_variable, selected = "pre"),
  #   sliderTextInput("fecha", tags$small("Fecha"), opt_fecha, selected = c(tail(opt_fecha, 12 * 10)[1], tail(opt_fecha, 1))),
  #   
  #   conditionalPanel(
  #     "input.showchart",
  #     # "hchart va en 2do contitaion panel",
  #     highchartOutput("chart", width = "100%", height = "250px"),
  #     div(
  #       style="display:inline-block;float:right",
  #       downloadButton("descargar_datos_mini", "Descargar datos", class = "btn-primary btn-sm")
  #     )
  #     # tags$br(),
  #   ),
  #   conditionalPanel(
  #     "false",
  #     checkboxInput("showchart", "Mostrar información histórica"),
  #   ),
  #   # actionButton("guidess", "Guide")
  # )
  accordion(
    # open = "Sensores",
    # open = "Biodiversidad",
    open = "Socioeconómico",
    
    ### sensores ----
  accordion_panel(
    "Sensores", icon = icon("temperature-half"),
    # open = c("Sitio"),
    uiOutput("sensores-group"),

    accordion_panel(
      "Categoría", icon = icon("sun-plant-wilt"),
      uiOutput("categoria_reset"),
      checkboxGroupInput(
        "categoria", NULL,
        choices = sort(unique(sensores$Categoria)),
        inline = FALSE,
        selected = sort(unique(sensores$Categoria))
      )
    ),
    accordion_panel(
      "Sitio", icon = icon("location-dot"),
      uiOutput("sitio_reset"),
      checkboxGroupInput(
        "sitio", NULL,
        choices = sort(unique(sensores$Sitio)),
        inline = TRUE,
        selected = sort(unique(sensores$Sitio))
      )
    ),    
    accordion_panel(
      "Rango de fechas", icon = icon("calendar-days"),
      dateRangeInput(
        inputId = "fechas",
        label = NULL,
        language = "es",
        separator = " hasta ",
        start = min(sensores$Fecha, na.rm = T),#"2018-04-15",
        end =  max(sensores$Fecha, na.rm = T),#"2018-04-16",
        min = min(sensores$Fecha, na.rm = T),#"2018-04-10",
        max = max(sensores$Fecha, na.rm = T)),#"2018-04-20"),
      # sliderInput("fecha", "Fecha:",
      # min = min(sensores$datetime2, na.rm = T), max = max(sensores$datetime2, na.rm = T), 
      # value = seq(min(sensores$datetime2, na.rm = T), max(sensores$datetime2, na.rm = T),1)),
      
      
    )),
    
    
    ### biodiversidad ----
    accordion_panel(
      "Biodiversidad", icon = bsicons::bs_icon("flower2"),
      input_histoslider(
        "diversidad_riqueza", "Riqueza taxonómica",
        biodiversidad$Riqueza, height = 150,
        breaks=seq(1,round(max(biodiversidad$Riqueza, na.rm = T),0)+1,1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      ),
      input_histoslider(
        "diversidad_shannon", "Índice de Shannon",
        biodiversidad$Shannon, height = 150,
        breaks=seq(1,round(max(biodiversidad$Shannon, na.rm = T),0),1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      ),
      input_histoslider(
        "diversidad_simpson", "Índice de Simpson",
        biodiversidad$Simpson, height = 150, #breaks = "months",
        breaks=seq(1,5,1),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      ),
    #### topografia ----
      accordion_panel(
        "Topografía", icon = icon("mountain"),
        input_histoslider(
          "topografica_elevacion", "Elevación (msnm)",
          biodiversidad$Elevación,
          height = 150,
          options = list(     handleLabelFormat = "0d",
                              selectedColor = PRIMARY)
        ),
        input_histoslider(
          "topografica_pendiente", "Pendiente (%)",
          biodiversidad$Pendiente.Porcentaje, height = 150,
          breaks=seq(0,360,30),
          options = list( handleLabelFormat = "0d",
                          selectedColor = PRIMARY)
        ),
        input_histoslider(
          "topografica_exposición", "Exposición",
          biodiversidad$Exposición, height = 150,
          options = list( handleLabelFormat = "0d",
                          selectedColor = PRIMARY)
        )
      )
    ),
  
 

    ### Socioeconomico ----
    accordion_panel(
      "Socioeconómico", icon = bsicons::bs_icon("people"),
      uiOutput("socioeconomico"),
      accordion_panel(
      "Sección de la cuenca", icon = icon("section"),
      uiOutput("seccion_reset"),
      checkboxGroupInput(
        "seccion", NULL,
        choices = sort(unique(clusterizado$seccion2)),
        inline = TRUE,
        selected = sort(unique(clusterizado$seccion2))
      )
    ),
   
     accordion_panel(
      "Grupo etario", icon = icon("id-card"),
      uiOutput("getario_reset"),
      
      input_histoslider(
        "edad", NULL,
        clusterizado$edad, height = 150,
        # breaks=seq(15,100,5),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      )

    ),
    
    accordion_panel(
      "Superficie (ha)", icon = icon("chart-area"),
      uiOutput("superficie_reset"),
      
      input_histoslider(
        "superficie", NULL,
        clusterizado$area_total, height = 150,
        # breaks=seq(15,100,5),
        options = list(
          handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      )
      ),
    
      accordion_panel(
        "% de sup. destinada a frutales", icon = icon("apple-whole"),
        uiOutput("fruta_reset"),
        
      sliderInput(
        "fruta", label = NULL,
        min = 0, max = 100, value = c(0,100), step = 10, round = 0
        # ,height = 150,
        # breaks=seq(15,100,5),
        # options = list(
          # handleLabelFormat = "0d",
          # selectedColor = PRIMARY
        # )
      )
    ),
    accordion_panel(
      "Porción destinada a hortalizas (%)", icon = icon("carrot"),
      uiOutput("hortaliza_reset"),
      
      sliderInput(
      # input_histoslider(
        "hortalizas", NULL,
        min = 0, max = 100, value = c(0,100), step = 10, round = 0
        # clusterizado$hort_share, height = 150,
        # breaks=seq(15,100,5),
        # options = list(
          # handleLabelFormat = "0d",
          # selectedColor = PRIMARY
        # )
      )
    ),
    accordion_panel(
      "Porcentaje destinado a especies forrajeras", icon = icon("seedling"),
      uiOutput("forraje_reset"),
      
      sliderInput(
      # input_histoslider(
        "forraje", NULL,
        # clusterizado$forraj_share, height = 150,
        min = 0, max = 100, value = c(0,100), step = 10, round = 0
        # breaks=seq(15,100,5),
        # options = list(
          # handleLabelFormat = "0d",
          # selectedColor = PRIMARY
        # )
      )
    ),
    accordion_panel(
      "Utiliza agua de fuentes subterráneas", icon = icon("arrow-up-from-ground-water"),
      uiOutput("rgosubt_reset"),
      
      checkboxGroupInput(
        "subt", NULL,
        choices = sort(unique(clusterizado$rgo_agua_subt)),
        inline = TRUE,
        selected = sort(unique(clusterizado$rgo_agua_subt))
      )
    ),
    accordion_panel(
      "eficiencia", icon = icon("faucet-drip"),
      uiOutput("eficiencia_reset"),
      
      input_histoslider(
      # sliderInput(
        "eficiencia", NULL,
        # min = 0, max = 100, value = 50, step = 10, round = 0
        clusterizado$eficiencia_prom, height = 150,
        # breaks=seq(15,100,5),
        options = list(
          # handleLabelFormat = "0d",
          selectedColor = PRIMARY
        )
      )
    ),
    accordion_panel(
      "Exposicion", icon = icon("sun-plant-wilt"),
      uiOutput("exposicions_reset"),
      
      input_histoslider(
      # sliderInput(
        "exposicions", NULL,
        # min = 0, max = 100, value = 50, step = 10, round = 0
        clusterizado$exposure_sub_index, height = 150,
        # breaks=seq(15,100,5),
        options = list(
          # handleLabelFormat = "N",
          selectedColor = PRIMARY
        )
      )
    ),
    accordion_panel(
      "Indice de vulnerabilidad", icon = icon("user-shield"),
      uiOutput("ives_reset"),
      
      input_histoslider(
        "ives", NULL,
        # min = 0, max = 100, value = 50, step = 10, round = 0
        clusterizado$ives, height = 150,
        breaks=seq(-0.6,0.6,0.05),
        options = list(
        # handleLabelFormat = "2d",
        selectedColor = PRIMARY
        )
      )
    )
    
    
    
    
    )
    
  ))
  
  ,
  ## Main Panel -----
  
  
  nav_panel(
    title = "Aplicación",
      icon  = icon("map-location-dot"),
    theme = theme_odes,
    
      tags$head(
        tags$link(href = "Isotip_gradiente_azul.png", rel = "icon"),
        tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
        tags$script(src = "js/ga.js"),
        includeCSS("www/css/styles.css")),
    # "Delays",
    ### Tarjetas ----
     uiOutput("tarjetas"),
    
    layout_column_wrap(
      width = 1/2,
      class = "my-3",
      # HTML("<p><strong>&nbsp; &nbsp; &nbsp; &nbsp;Mapa de la cuenca del río Aconcagua</strong></p>"),
      ### Mapa ----
      navset_card_pill(
        title = "Mapas",
        full_screen = FALSE,
        nav_panel("Sensores",
                  leafletOutput("mapaSensores")),
        nav_panel("Biodiversidad",
                  leafletOutput("mapaBiodiversidad")),
        nav_panel("Socioeconómico",
                  leafletOutput("mapaSocioeconomico"))
        ),#,height = 300),
      
      ### Sensores ----
      navset_card_pill(
        title = "Sensores",
        full_screen = FALSE,
        nav_panel(
          "Temperatura",
          plotOutput("plot.sensores.t",click = "plot_clicka")
        ),
        nav_panel(
          "Humedad",
          plotOutput("plot.sensores.h",click = "plot_clicks"))
      )
      ), 
    layout_column_wrap(
      width = 1/2 , class = "my-3",
      ### Biodiversidad ----
    navset_card_pill(
      title = "Biodiversidad",
      full_screen = FALSE,
      nav_panel(
        "Riqueza taxonómica",
        plotOutput("plot.riqueza",click = "plot_click1")
      ),
      nav_panel(
        "Índice de Shannon",
        plotOutput("plot.shannon",click = "plot_click2")),
      nav_panel(
        "Índice de Simpson",
        plotOutput("plot.simpson",click = "plot_click2"))
      ),
      ### Socioeconomico ----
    navset_card_pill(
      title = "Socioeconómico",
      full_screen=FALSE,
      nav_panel(
        title=NULL,
        plotOutput("plot.socioeconomico",click = "plot_click2"))
    ))
    
    )
  ,
  

  
      ### ayuda ----
  bslib::nav_panel(
    title = "Ayuda",
    icon  = icon("question"),
    layout_column_wrap(
      width = 1,
      navset_card_tab(
        # height = 450,
        # full_screen = TRUE,
        # title = "HTML Widgets",
        nav_panel(
          "Aplicación",
          includeMarkdown("md/ayuda.md")
        ),
        nav_panel(
          "Indicadores",
          includeMarkdown("md/indicadores2.md")
        )
      )
    )
  )
  
)


# Server ----
server <- function(input, output, session) {
  
  # 
  # biodiversidad <- reactive({
  #   biodiversidad[ is.element(biodiversidad()["Sitio"], sitio=updateSelectInput), ]
  # })
  # observe({
    # Obtener la selección de la entrada 1
    # categoria.i <- unique(input$categoria)
    # output$sensores <- subset(sensores, is.element(Categoria, categoria.i))
    
    # Actualizar las opciones disponibles en la entrada 2
    # Basado en la selección de la entrada 1
    
  # })  
  
  # tarjetas ----
  output$tarjetas <- renderUI({
    
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    # print(fechas.i)
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & 
                           is.element(Categoria, categoria.i) & 
                           Fecha >= fechas.i[1] &
                           Fecha <= fechas.i[2]
                           )
    
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, 
                              # is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    
    seccion.i <- unique(input$seccion)
    edad.i <- summary(input$edad)
    area.i <- summary(input$superficie)
    fruta.i <- summary(input$fruta)
    hort.i <- summary(input$hortalizas)
    forr.i <- summary(input$forraje)
    agusubt.i <- unique(input$subt)
    eficiencia.i <- summary(input$eficiencia)
    exposicions.i <- summary(input$exposicions)
    ives.i <- summary(input$ives)
    
    clusterizado.i <- subset(clusterizado,
                             is.element(seccion2, seccion.i) & 
                               edad >= edad.i["Min."] & edad <= edad.i["Max."] & 
                               area_total >= area.i["Min."] & area_total <= area.i["Max."] &
                               frut_share >= fruta.i["Min."]/100 & frut_share <= fruta.i["Max."]/100 &
                               hort_share >= hort.i["Min."]/100 & hort_share <= hort.i["Max."]/100 &
                               forraj_share >= forr.i["Min."]/100 & forraj_share <= forr.i["Max."]/100 &
                               is.element(rgo_agua_subt, agusubt.i) &
                               eficiencia_prom >= eficiencia.i["Min."] & eficiencia_prom <= eficiencia.i["Max."] &
                               exposure_sub_index >= exposicions.i["Min."] & exposure_sub_index <= exposicions.i["Max."] &
                               ives >= ives.i["Min."] & ives <= ives.i["Max."])                        
    
    n_sensores <- value_box( 
      # height = "200px",
      # theme_color = "primary",
      # "Un total de",
      paste(nrow(sensores.i), "registros provenientes de"),
      paste(length(unique(sensores.i$Sitio)),
        "sensores"
      ),
     
      showcase = bsicons::bs_icon("thermometer-half")
    )
    
    n_flora <- value_box(
      "Observaciones correspondientes a"
      ,
      paste(length(unique(biodiversidad.i$Sitio)),
            "sitios"),
      showcase = bsicons::bs_icon("flower2")
    )
    
    n_socioeconomico <- value_box(
      "Encuesta aplicada a ",
      paste(nrow(clusterizado.i),
        "personas"
      ),
      # paste0(
        # round(100 * sum(d$arr_delay > 0, na.rm = TRUE) / nrow(d), 1),
        # "% of flights arrive late"
      # ),
      showcase = bsicons::bs_icon("people")
    )
    
    layout_column_wrap(height = "125px", width = 1/3, n_sensores, n_flora, n_socioeconomico)
  })
  
  
  # actualizacion de sitio en base a clase de categoria ----
  
  observeEvent(input$categoria,{
    categorias.i <- unique(input$categoria)
    # sitios.actualizados <- unique(sensores$Sitio[sensores$Categoria %in% categorias.i])
    sitios.actualizados <- 
      # sort(
      unique(
        subset(sensores, is.element(Categoria, categorias.i))$Sitio
        
        )
    
    # print(categorias.i)
    # print(sitios.actualizados)
    updateCheckboxGroupInput(session, "sitio", 
                             choices = sort(unique(sensores$Sitio)),
                             inline = TRUE,
                             selected = sort(unique(sitios.actualizados)))
    },
    ignoreNULL = FALSE
  )
  
  # grafico sensores ----
  ## temperatura ----
  output$plot.sensores.t <- renderPlot({
    
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & 
                           is.element(Categoria, categoria.i)& 
                           Fecha >= fechas.i[1] &
                           Fecha <= fechas.i[2])
    
    sensores.i <- sensores.i %>% group_by(Categoria, Fecha) %>% 
      summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
                Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
                Humedad.suelo = mean(Humedad.suelo, na.rm = T))
    
    
    sensores.i.aire <- sensores.i
    sensores.i.aire$temp <- sensores.i.aire$Temp.aire.15
    sensores.i.aire$Temp.aire.15 <- NULL
    sensores.i.aire$Categoria <- paste(sensores.i.aire$Categoria, "aire",sep="-")
    
    sensores.i.suelo <- sensores.i
    sensores.i.suelo$temp <- sensores.i.suelo$Temp.suelo.8
    sensores.i.suelo$Temp.suelo.8 <- NULL
    sensores.i.suelo$Categoria <- paste(sensores.i.suelo$Categoria, "suelo",sep="-")
  
    sensores.ii <- rbind(sensores.i.aire, sensores.i.suelo)
    
    
    
    ggplot(sensores.ii, aes(x=Fecha, y=temp, color=Categoria))+geom_line(linetype = 1)+#geom_line(aes(y=Temp.suelo.8))+
      xlim(fechas.i)+
      theme_minimal()+
      ylab('Temperatura (\u00baC)')+
      xlab('Fecha')+
      labs(color="Categoría")+
      # scale_x_date(date_breaks='1 month')+
      scale_x_date(date_labels = "%m - %y")+
      scale_colour_manual(values=paleta.sensores)+
      # ggtitle("Temperatura del aire a 15 cm y de suelo a 8cm") +
      ylim(summary(c(sensores.dia$Temp.aire.15,sensores.dia$Temp.suelo.8))[c("Min.","Max.")])
    
  }, res = 100, height = 230)
  ## humedad ----
  output$plot.sensores.h <- renderPlot({
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & 
                           is.element(Categoria, categoria.i)& 
                           Fecha >= fechas.i[1] &
                           Fecha <= fechas.i[2])
    
    sensores.i <- sensores.i %>% group_by(Categoria, Fecha) %>% 
      summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
                Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
                Humedad.suelo = mean(Humedad.suelo, na.rm = T))
    
    
    # sensores.i.aire <- sensores.i
    # sensores.i.aire$temp <- sensores.i.aire$Temp.aire.15
    # sensores.i.aire$Temp.aire.15 <- NULL
    # sensores.i.aire$Categoria <- paste(sensores.i.aire$Categoria, "aire",sep="-")
    # 
    # sensores.i.suelo <- sensores.i
    # sensores.i.suelo$temp <- sensores.i.suelo$Temp.suelo.8
    # sensores.i.suelo$Temp.suelo.8 <- NULL
    # sensores.i.suelo$Categoria <- paste(sensores.i.suelo$Categoria, "suelo",sep="-")
    # 
    # sensores.ii <- rbind(sensores.i.aire, sensores.i.suelo)
    sensores.ii <- sensores.i  
    
    
    ggplot(sensores.ii, aes(x=Fecha, y=Humedad.suelo, color=Categoria))+geom_line(linetype = 1)+#geom_line(aes(y=Temp.suelo.8))+
      xlim(fechas.i)+
      theme_minimal()+
      ylab('Humedad del suelo (%)')+
      xlab('Fecha')+
      labs(color="Categoría")+
      scale_x_date(date_labels = "%m - %y")+ 
      scale_colour_manual(values=paleta.sensores.humedad)+
      # ggtitle("Temperatura del aire a 15 cm y de suelo a 8cm") +
      ylim(summary(sensores.dia$Humedad.suelo)[c("Min.","Max.")])
    
  }, res = 100, height = 230)
  
  
  # grafico biodiversidad ----
  ## grafico riqueza taxonomica ----
  output$plot.riqueza <- renderPlot({
    sitios.i <- unique(input$sitio)
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, 
                              # is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    ggplot(biodiversidad.i, aes(Elevación, Riqueza)) + geom_point()+
      xlim (0, 2500)+
      ylim(0,12.5)+
      # ggtitle("Riqueza taxonómica vs elevación") +
      theme_minimal()+
      ylab('Riqueza taxonómica (N\u00b0 spp)') +
      xlab('Elevación (msnm)')
  }, res = 100, height = 230)
  
  ## grafico Shannon ----
  output$plot.shannon <- renderPlot({
    sitios.i <- unique(input$sitio)
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    ggplot(biodiversidad.i, aes(Elevación, Shannon)) + geom_point()+
      xlim (0, 2500)+
      ylim(0,8)+
      # ggtitle("Riqueza taxonómica vs elevación") +
      theme_minimal()+
      ylab('Índice de Shannon ') +
      xlab('Elevación (msnm)')
  }, res = 100, height = 230)
  
  ## grafico Simspon ----
  output$plot.simpson <- renderPlot({
    sitios.i <- unique(input$sitio)
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad, is.element(Sitio, sitios.i) & 
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    ggplot(biodiversidad.i, aes(Elevación, Simpson)) + geom_point()+
      xlim (0, 2500)+
      ylim(0,8)+
      # ggtitle("Riqueza taxonómica vs elevación") +
      theme_minimal()+
      ylab('Índice de Simpson') +
      xlab('Elevación (msnm)')
  }, res = 100, height = 230)
  
  # grafico socioeconomico ----
  output$plot.socioeconomico <- renderPlot({
    
    seccion.i <- unique(input$seccion)
    edad.i <- summary(input$edad)
    area.i <- summary(input$superficie)
    fruta.i <- summary(input$fruta)
    hort.i <- summary(input$hortalizas)
    forr.i <- summary(input$forraje)
    agusubt.i <- unique(input$subt)
    eficiencia.i <- summary(input$eficiencia)
    exposicions.i <- summary(input$exposicions)
    ives.i <- summary(input$ives)
    
    clusterizado.i <- subset(clusterizado,
                             is.element(seccion2, seccion.i) & 
                               edad >= edad.i["Min."] & edad <= edad.i["Max."] & 
                               area_total >= area.i["Min."] & area_total <= area.i["Max."] &
                               frut_share >= fruta.i["Min."]/100 & frut_share <= fruta.i["Max."]/100 &
                               hort_share >= hort.i["Min."]/100 & hort_share <= hort.i["Max."]/100 &
                               forraj_share >= forr.i["Min."]/100 & forraj_share <= forr.i["Max."]/100 &
                               is.element(rgo_agua_subt, agusubt.i) &
                               eficiencia_prom >= eficiencia.i["Min."] & eficiencia_prom <= eficiencia.i["Max."] &
                               exposure_sub_index >= exposicions.i["Min."] & exposure_sub_index <= exposicions.i["Max."] &
                               ives >= ives.i["Min."] & ives <= ives.i["Max."]
                             
                                )
    # clusterizado.i$typo <- factor(clusterizado.i$typo)
    
    # clusterizado.i <- rbind(clusterizado.i, data.frame(typo=c("2"),PCA.Axis1=-10,PCA.Axis2=-10))
    
    
    ggplot(clusterizado.i, aes(PCA.Axis1, PCA.Axis2)) + 
      geom_point(data=clusterizado, color="lightgray")+  
      geom_point(aes(color=factor(typo)))+
      xlim (-3, 2)+
      ylim(-3,3)+
      # ggtitle("Análisis cluster") +
      scale_colour_manual(values=paleta.socioeconomico)+
      
      labs(color="Tipología")+
      theme_minimal()+
      ylab('ACP 2') +
      xlab('ACP 1')
  }, res = 100, height = 250)
  
  # Mapas ----
  ## Mapa sensores ----
  output$mapaSensores <-  renderLeaflet({
    categoria.i <- unique(input$categoria)
    sitios.i <- unique(input$sitio)
    fechas.i <- input$fechas
    
    
    sensores.i <- subset(sensores, is.element(Sitio, sitios.i) & 
                           is.element(Categoria, categoria.i)& 
                           Fecha >= fechas.i[1] &
                           Fecha <= fechas.i[2])
    
    # # sensores.i <- sensores.i %>% group_by(Categoria, Fecha, Longitud, Latitud) %>% 
    #   summarise(Temp.aire.15 = mean(Temp.aire.15, na.rm = T),
                # Temp.suelo.8 = mean(Temp.suelo.8, na.rm = T),
                # Humedad.suelo = mean(Humedad.suelo, na.rm = T))
    # raster.i <- 
    
    sensores.i <- unique(sensores.i[,c("Categoria", "Longitud", "Latitud")])
    leaflet() %>% addTiles() %>%
      # addRasterImage(raster.i, colors = pal, opacity = 0.8) %>%
      # addLegend(pal = pal, values = values(r),
                # title = "Surface temp") %>%
    addCircles(data = sensores.i, lat = ~Latitud, lng = ~Longitud, color="red", radius=15)%>% 
      addPolygons(data=cuenca, color = "#444444"#, weight = 1, smoothFactor = 0.5,
                  #opacity = 1.0, fillOpacity = 0.5
    )
    
    
  })
  
  
  ## Mapa biodiveridad ----
  output$mapaBiodiversidad <-  renderLeaflet({
    riqueza.i <- summary(input$diversidad_riqueza)
    shannon.i <- summary(input$diversidad_shannon)
    simpson.i <- summary(input$diversidad_simpson)
    elevacion.i <- summary(input$topografica_elevacion)
    pendiente.i <- summary(input$topografica_pendiente)
    exposicion.i <- summary(input$topografica_exposición)
    
    biodiversidad.i <- subset(biodiversidad,
                                Riqueza>=riqueza.i["Min."] & Riqueza<=riqueza.i["Max."]& 
                                Shannon>=shannon.i["Min."] & Shannon<=shannon.i["Max."]& 
                                Simpson>=simpson.i["Min."] & Simpson<=simpson.i["Max."]& 
                                Elevación>=elevacion.i["Min."] & Elevación<=elevacion.i["Max."]& 
                                Pendiente.Porcentaje>=pendiente.i["Min."] & Pendiente.Porcentaje<=pendiente.i["Max."]& 
                                Exposición>=exposicion.i["Min."] & Exposición<=exposicion.i["Max."])
    
    # raster.i <- 
    leaflet() %>% addTiles() %>%
      # addRasterImage(raster.i, colors = pal, opacity = 0.8) %>%
      # addLegend(pal = pal, values = values(r),
      # title = "Surface temp") %>%
      addCircles(data = biodiversidad.i, lat = ~Latitud, lng = ~Longitud, color="black")%>% 
      addPolygons(data=cuenca, color = "#444444"#, weight = 1, smoothFactor = 0.5,
                  #opacity = 1.0, fillOpacity = 0.5
      )
    
    
  })
  
  ## Mapa socieconomico ----
  output$mapaSocioeconomico <-  renderLeaflet({
    seccion.i <- unique(input$seccion)
    edad.i <- summary(input$edad)
    area.i <- summary(input$superficie)
    fruta.i <- summary(input$fruta)
    hort.i <- summary(input$hortalizas)
    forr.i <- summary(input$forraje)
    agusubt.i <- unique(input$subt)
    eficiencia.i <- summary(input$eficiencia)
    exposicions.i <- summary(input$exposicions)
    ives.i <- summary(input$ives)
    
    clusterizado.i <- subset(clusterizado,
                             is.element(seccion2, seccion.i) & 
                               edad >= edad.i["Min."] & edad <= edad.i["Max."] & 
                               area_total >= area.i["Min."] & area_total <= area.i["Max."] &
                               frut_share >= fruta.i["Min."]/100 & frut_share <= fruta.i["Max."]/100 &
                               hort_share >= hort.i["Min."]/100 & hort_share <= hort.i["Max."]/100 &
                               forraj_share >= forr.i["Min."]/100 & forraj_share <= forr.i["Max."]/100 &
                               is.element(rgo_agua_subt, agusubt.i) &
                               eficiencia_prom >= eficiencia.i["Min."] & eficiencia_prom <= eficiencia.i["Max."] &
                               exposure_sub_index >= exposicions.i["Min."] & exposure_sub_index <= exposicions.i["Max."] &
                               ives >= ives.i["Min."] & ives <= ives.i["Max."])
    
    if(nrow(clusterizado.i)<4){clusterizado.i <- NA;
    stop("El número de encuestados es muy bajo para generar el mapa")}else{nofiltro=1}
        # raster.i <- 
    leaflet() %>% addTiles() %>%
      # addRasterImage(raster.i, colors = pal, opacity = 0.8) %>%
      # addLegend(pal = pal, values = values(r),
      # title = "Surface temp") %>%
      addHeatmap(data = clusterizado.i, lat = ~location_latitude, lng = ~location_longitude, radius=10)%>% 
      addPolygons(data=cuenca, color = "#444444"#, weight = 1, smoothFactor = 0.5,
                  #opacity = 1.0, fillOpacity = 0.5
      )
    
    
  })
  

  output$data <- renderTable({
    # req(input$plot_click)
    nearPoints(biodiversidad[,c("Sitio","Riqueza","Shannon", "Simpson","Elevación","Pendiente.Porcentaje","Exposición")], input$plot_click)
  })
}


shinyApp(ui, server)

  