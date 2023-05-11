
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)


ui<-navbarPage(theme = '2css.css',
               "App Master Ciencia de Datos 💻 📊", 
               tabPanel("Selección de máquina 🤖",
                        sidebarLayout(
                          sidebarPanel('MÁQUINA',
                                       fileInput("DatosFichero", "Selecciona el fichero de datos", accept = NULL),
                                       tableOutput("contents"),
                                       uiOutput("selectMaquina")
                                      ),

                          mainPanel(h5("Probabilidad de orden"),
                                    plotOutput("probsPlot"))
                        )),

               navbarMenu("Estado de la maquina 🔛",
                          tabPanel("Evolución Temporal alarmas",
                                   sidebarLayout(
                                     sidebarPanel('ALARMAS radiobuttons',
                                                  textOutput('alarmas_desact'),
                                                  uiOutput("radioButtons_alarms")),

                                     mainPanel(h5("🕒🚨 Evolución temporal Alarmas 🚨🕒"),
                                               plotlyOutput('plotAlarms'))
                                     )),

                          tabPanel("Registros de la máquina",
                                   sidebarLayout(
                                     sidebarPanel('ALARMAS checkbox', 
                                                  textOutput('alarmas_desact2'), 
                                                  uiOutput("checkBoxes_alarms")),

                                     mainPanel(h5("Registros de la máquina seleccionada"),
                                               dataTableOutput('tabla_alarmas'))
               ))),
               tabPanel("Estadísticas Globales Temporales 📈🕒",
                        sidebarLayout(
                          sidebarPanel('PERIODO Y ESTADÍSTICAS',                             
                              uiOutput('data_range'),
                                'HISTOGRAMA 📊',
                              uiOutput('selectAlarma'),
                              uiOutput('slider_bins'),
                                'BOXPLOT',
                              uiOutput('elegir_grafico'), 
                              conditionalPanel(
                                condition = "input.elegir_grafico == 'Boxplot'",
                                uiOutput('checkBoxplots')
                              )

                          ),
                          mainPanel(h5("Histograma de la alarma seleccionada"),
                                      # plotOutput('plotHist'),
                                      plotOutput('boxplot')
                                   
                                    )
                        )),
               
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$DatosFichero)
    load(input$DatosFichero$datapath)
    get(ls()[1])
  })
  
  output$contents <- renderTable({
    file <- input$DatosFichero
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "Rdata", "El archivo que has seleccionado no es un .Rdata, más concretamente 'PrediccionesMaquina.Rdata'. Selecciona ese archivo para continuar"))
    # Código para cargar y mostrar el contenido del archivo .Rdata
  })
  
  
  output$selectMaquina <- renderUI({
    req(data())
    selectInput("select", "️➡️ 🤖 Selecciona máquina:", choices = unique(data()[,"matricula"]))
  })
  
  
  output$probsPlot <- renderPlot({
    req(data(), input$select)
    attach(data())
    df <- data()[data()[,"matricula"] == input$select,]
    #ggplotly( 
      ggplot(df, aes(x = dia, y = p_orden, color = p_orden)) +
               geom_line() + geom_point() + 
      scale_color_gradient(low = "darkblue", high = "red") +
      labs(title = "", x = 'Día', y = 'P. orden', color = "p_orden") #)
  })
  
  dataButtons <- reactive({
    req(input$DatosFichero)
    load(input$DatosFichero$datapath)
    df <- as.data.frame(get(ls()[1]))
    df <- na.omit(df)
    df <- df[df[,"matricula"] == input$select,]
    cols_originales <- names(df)
    df <- df %>% select(matches("^a\\d+$"))
    df <- df %>% select(-setdiff(names(df), names(df)[colSums(df) != 0.0000]))
    cols_eliminadas <- setdiff(cols_originales, names(df))
    colnames(df)
  })
  
  cols_elim <- reactive({
    req(input$DatosFichero)
    load(input$DatosFichero$datapath)
    df <- as.data.frame(get(ls()[1]))
    df <- na.omit(df)
    df <- df[df[,"matricula"] == input$select,]
    df <- df %>% select(matches("^a\\d+$"))
    cols_originales <- names(df)
    df <- df %>% select(-setdiff(names(df), names(df)[colSums(df) != 0.0000]))
    cols_eliminadas <- setdiff(cols_originales, names(df))
    cols_eliminadas
  })
  
  
  output$radioButtons_alarms <- renderUI({
    radioButtons("variable", "➡️ 🚨 Selecciona la alarma a visualizar:",
                 choices = dataButtons())
  })
  
  output$alarmas_desact <- renderText({
    req(cols_elim(), input$select)
    desac <- paste('Las alarmas ➡️', paste(cols_elim(), collapse = ', '), ' no se muestran porque están desactivadas para la máquina ', input$select)
    desac
   })
  
  output$alarmas_desact2 <- renderText({
    req(cols_elim(), input$select)
    desac <- paste('Las alarmas ➡️', paste(cols_elim(), collapse = ', '), ' no se muestran porque están desactivadas para la máquina ', input$select)
    desac
  })
  
  output$plotAlarms <- renderPlotly({
    req(data(), input$select, input$variable)
    df <- data()[data()[,"matricula"] == input$select,]
    ggplotly(ggplot(df, aes(x = dia, y = !!sym(input$variable))) +
      geom_line(color = 'darkcyan') + geom_point(color = 'darkcyan') + 
      #scale_color_gradient(low = "darkblue", high = "red") +
      labs(title = "", x = 'Día', y = input$variable))
  })

  output$checkBoxes_alarms <- renderUI({
    checkboxGroupInput("check_variable", "➡️ 🚨 Selecciona las alarmas para ver en la tabla",
                 choices = dataButtons())
  })

  output$tabla_alarmas <- renderDataTable({
    req(data(), input$check_variable)
    df <- data()
    df <- df %>% select( matricula, dia, input$check_variable, p_orden)
    df <- df[df[,"matricula"] == input$select,]
    
    df <- na.omit(df)  # Eliminar filas con NAs
    datatable(df, style = 'bootstrap4',options = list(pageLength = 25))
  })
  
  output$selectAlarma <- renderUI({
    req(dataButtons())
    selectInput("selectAlarma", "Selecciona máquina", choices = dataButtons())
  })

  output$slider_bins <- renderUI({
    sliderInput("slider_bins", "Ancho de bin del histograma 《⇹》",
                min= 1, max = 50, value = 20, step= 1)
  })
  
  output$data_range <- renderUI({
    req(data())
    df <- data()
    dateRangeInput("data_range", "Selecciona el periodo 📅",
                   start = min(df$dia), end = max(df$dia), weekstart = 1, separator= "a", language = 'es')
  })
  
  output$elegir_grafico <- renderUI({
    radioButtons("elegir_grafico", "➡️ Eleccion de gráfico",
                 choices = c('Histograma', 'Boxplot'))
  })
  
  output$checkBoxplots <- renderUI({
    checkboxInput("checkBoxplots", "Todas las máquinas")
  })
  
    output$boxplot <- renderPlot({
      data() 
      input$selectAlarma
      input$data_range 
      input$select
      input$checkBoxplots
      input$elegir_grafico

      if (input$elegir_grafico == 'Histograma') {
        req(data(), input$selectAlarma, input$slider_bins, input$data_range, input$select)
        df <- na.omit(data())
        df <- df[df[,"matricula"] == input$select,]
        df <- df %>% filter(dia >= input$data_range[1] & dia <= input$data_range[2])
        ggplot(df, aes(x= !!sym(input$selectAlarma))) +
          geom_histogram(color = 'darkcyan', fill = 'darkslategray', binwidth=input$slider_bins)
        
        
      } else if (input$elegir_grafico == 'Boxplot') {
        if (input$checkBoxplots == FALSE){
          df <- na.omit(data())
          df <- df %>% select(matricula, dia, matches(paste0("^", input$selectAlarma)))
          df <- df %>% filter(dia >= input$data_range[1] & dia <= input$data_range[2])
          df <- df[df[,"matricula"] == input$select,]
        
          ggplot(df, aes(x = matricula, y = !!sym(input$selectAlarma))) +
          geom_boxplot(color = 'darkcyan', fill = 'darkslategray') 
      
        
      } else if (input$checkBoxplots == TRUE) {
        df <- na.omit(data())
        df <- df %>% select(matricula, dia, matches(paste0("^", input$selectAlarma)))
        df <- df %>% filter(dia >= input$data_range[1] & dia <= input$data_range[2])
        df$matricula <- as.factor(df$matricula)

        ggplot(df, aes(x =matricula, y = !!sym(input$selectAlarma))) +
          geom_boxplot(color = 'darkcyan', fill = 'darkslategray') 
        
      }} 
        

   })
}

shinyApp(ui, server)






