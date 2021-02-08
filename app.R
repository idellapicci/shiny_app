#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Created on Feb 27 2020
# Altered on March 19 2020
# Author: Isabela_Piccinini
# Status: {dev_test}

library(shiny)
library(gridExtra)
library(shinythemes)
library(ggpubr)
library(rsconnect)

options(shiny.maxRequestSize = 3000*1024^2)

# Include credentials to aplication as configured 
#rsconnect::setAccountInfo(name='myappisa', token='', secret='')
#rsconnect::deployApp(server = 'shinyapps.io', 
#                     appFiles = '',
#                     appId = )

ui <- shinyUI(
    
    # --------------------------------------------------------CONFIGURE PAGE THEME AND LOGO-------------------------------------------------------------------------------
    fluidPage(
    theme = shinytheme("superhero"),
    headerPanel(
        list("Analitica Descriptiva",
             HTML('<img src="https://www.analyticsvidhya.com/wp-content/uploads/2016/10/shiny.png", height="100px",    
                  style="float:right"/>','<p style="color:black"></p>'))
    ),
    
    sidebarLayout(position = "left",
                  
                  # -------------------------------------------CONFIGURE SLIDER BAR ON LEFT-------------------------------------------------------------------------------
                  sidebarPanel("",
                               # Input: Select a file ----
                               fileInput("file", "Selecciona el archivo csv:",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               # Horizontal line ----
                               tags$hr(),
                               
                               # Selection Variables ---- 
                               selectInput(inputId = "var1", label = strong("Seleccione la primera variable:"), choices = c()),
                               selectInput(inputId = "var2", label = strong("Seleccione la segunda variable:"), choices = c()),
                               
                               # Horizontal line ----
                               tags$hr(),
                               
                               # Input: Specification of date range within an interval ----
                               uiOutput("mSlider"),
                               width = 3),
                  
                  
                  # -------------------------------------------CONFIGURE PLOT AREA ON RIGHT-------------------------------------------------------------------------------
                  mainPanel("",
                            tabsetPanel(
                                
                                # -----------------------------CONFIGURE TAB 1 - CORRELATION------------------------------------------------------------------------------
                                tabPanel("Analisis de correlacion",
                                    column(12, tags$hr()),
                                    
                                    # Scatter Plot
                                    column(12,plotOutput(outputId="dataplot", width="1200",height="500px",
                                                         dblclick = "dataplot_dblclick",
                                                         brush = brushOpts(id = "dataplot_brush", resetOnNew = TRUE)
                                                         )),
                                    column(12, tags$hr()),
                                    
                                    # Histogram Variable 1
                                    column(10,plotOutput(outputId="hist1", width="1000px",height="500px")),
                                    
                                    # Table Stats Variable 1
                                    column(2,tableOutput("quant1")),
                                    column(12, tags$hr()),
                                    
                                    # Histogram Variable 2
                                    column(10,plotOutput(outputId="hist2", width="1000px",height="500px")),
                                    
                                    # Table Stats Variable 2
                                    column(2,tableOutput("quant2"))
                                ),
                                
                                # -----------------------------CONFIGURE TAB 2 - TIME SERIES------------------------------------------------------------------------------
                                tabPanel("Series de tiempo",
                                     column(12, tags$hr()),
                                     
                                     # Time Series Variable 1
                                     column(12,plotOutput(outputId="timeseries1", width="1200",height="600px")),
                                     column(12, tags$hr()),
                                     
                                     # Time Series Variable 2
                                     column(12,plotOutput(outputId="timeseries2", width="1200",height="600px")),
                                     column(12, tags$hr())
                                ))
                  ))))


server = function(input, output, session){
    
    
    
    # -----------------------------------------------------------IMPORT AND PREPARE DATA----------------------------------------------------------------------------------
    
    # Import data
    data = reactive({
        
        # Import dataset (first columns MUST be dates)
        req(input$file)
        inFile = input$file
        df = read.csv2(inFile$datapath,
                       header = TRUE,
                       sep=",",
                       dec=".",
                       stringsAsFactors = FALSE,
                       encoding = 'utf-8')
        
        df[, 1] = as.Date(df[, 1])
        
        # Update inputs
        checkbox = names(df)[-1]
        updateSelectInput(session, 
                          inputId = "var1", 
                          choices = checkbox, 
                          selected = checkbox[1])
        updateSelectInput(session, 
                          inputId = "var2", 
                          choices = checkbox, 
                          selected = checkbox[2])
        
        return(df)
    })
    
    
    # Get selected dates in slider bar to filter data
    output$mSlider <- renderUI({
        
        df = data()
        
        Date = df[, 1]
        
        # Generate the slider
        sliderInput("DateRange", "Seleccione el periodo deseado:",
                    #min = as.Date("2018-01-01"), max = as.Date("2020-02-29"),
                    min = min(Date), max = max(Date),
                    value = c(min(Date), max(Date)),
                    timeFormat = "%Y-%m-%d",
                    round = FALSE)
    })
    
    
    # Filter dates selected
    data_filter = reactive({
        
        df = data()
        
        # Filter dates
        Date = df[, 1]
        df = df[Date >= input$DateRange[1] & Date <= input$DateRange[2],]
        
        return(df)
    })
    
    
    # Return date column
    date_col = reactive({
        
        df = data_filter()
        
        # Creates date format
        Date = df[, 1]
        
        return(Date)
    })
    
    
    # Single zoomable plot (Scatter Plot)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$dataplot_dblclick, {
        brush <- input$dataplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    
    # Filter brush ranges for Variable 1
    data_filter_hist1 = reactive({
        
        df = get(input$var1, data_filter())
        
        # Filter Brushes
        if(!is.null(ranges$x)){
            df = df[df>=ranges$x[1] & df<=ranges$x[2]]
        }
        
        return(df)
    })
    
    
    # Filter brush ranges for Variable 2
    data_filter_hist2 = reactive({
        
        df = get(input$var2, data_filter())
        
        # Filter Brushes
        if(!is.null(ranges$y)){
            df = df[df>=ranges$y[1] & df<=ranges$y[2]]
        }
        
        return(df)
    })
    
    
    
    # -----------------------------------------------------------CONFIGURE PLOTS TAB 1 - CORRELATION----------------------------------------------------------------------

    # Scatter Plot
    output$dataplot = renderPlot({
        
        if(is.factor(get(input$var1, data_filter())) || is.factor(get(input$var1, data_filter()))){
            barplot(table(get(input$var1, data_filter())),
                    main = paste('Distribution for categoric variable 1: ', isolate({input$var1})), 
                    col = "dodgerblue4",
                    xlab = isolate({input$var1}), 
                    ylab = isolate({input$var2}))
        }else{
            ggplot(data_filter(),
                   aes(get(input$var1, data_filter()), get(input$var2, data_filter()))) + 
                ggtitle("Analisis de correlacion") +
                xlab(isolate({input$var1})) +
                ylab(isolate({input$var2})) +
                geom_point(col = "dodgerblue4") +
                stat_smooth(col = "lightskyblue") + 
                stat_cor(method = "pearson", label.y.npc = "top", label.x.npc = "left", size = 6, color = "red") +
                theme(
                    plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=12, face="plain"),
                    axis.title.y = element_text(color="black", size=12, face="plain")) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
            
            
        }})
    
    
    # Histogram Variable 1
    output$hist1 = renderPlot({
        
        if(!is.factor(get(input$var1, data_filter()))){
            hist(data_filter_hist1(),
                 col = 'deepskyblue', border = 'lightskyblue', 
                 main = paste('Distribucion para la variable: ', isolate({input$var1})),
                 xlab = isolate({input$var1}),
                 ylab = 'Frecuencia',
                 labels = TRUE) +
                theme(
                    plot.title = element_text(color="black", size=18, face="plain", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=12, face="plain"),
                    axis.title.y = element_text(color="black", size=12, face="plain"))
            
        }})
    
    
    # Histogram Variable 2
    output$hist2 = renderPlot({
        
        if(!is.factor(get(input$var2, data_filter()))){
            hist(data_filter_hist2(),
                 col = 'deepskyblue', border = 'lightskyblue2', 
                 main = paste('Distribucion para la variable: ', isolate({input$var2})),
                 xlab = isolate({input$var2}),
                 ylab = 'Frecuencia',
                 labels = TRUE) +
                theme(
                    plot.title = element_text(color="black", size=18, face="plain", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=12, face="plain"),
                    axis.title.y = element_text(color="black", size=12, face="plain"))
            
        }})
    
    
    # Table Stats Variable 1
    output$quant1 = renderTable({
        
        txt = quantile(data_filter_hist1(),
                       probs = c(0.01, 0.1, 0.25, 0.50, 0.75, 0.90, 0.99),
                       na.rm = TRUE)
        statistics = c(as.vector(names(txt)), 'Avg.', 'Std.')
        values = c(as.vector(txt), mean(get(input$var1, data_filter()), na.rm = TRUE), sd(get(input$var1, data_filter()), na.rm = TRUE))
        txt = data.frame(statistics, values)
        colnames(txt) = c('Estadisticas', 'Valor')
        print(txt)
        
    })
    
    
    # Table Stats Variable 2
    output$quant2 = renderTable({
        
        txt = quantile(data_filter_hist2(),
                       probs = c(0.01, 0.1, 0.25, 0.50, 0.75, 0.90, 0.99),
                       na.rm = TRUE)
        statistics = c(as.vector(names(txt)), 'Avg.', 'Std.')
        values = c(as.vector(txt), mean(get(input$var2, data_filter()), na.rm = TRUE), sd(get(input$var2, data_filter()), na.rm = TRUE))
        txt = data.frame(statistics, values)
        colnames(txt) = c('Estadisticas', 'Valor')
        print(txt)
        
    })
    
    
    
    # -----------------------------------------------------------CONFIGURE PLOTS TAB 2 - TIME SERIES----------------------------------------------------------------------
    
    # Time Series Plot Variable 1
    output$timeseries1 = renderPlot({
        
        if(!(is.factor(get(input$var1, data_filter())))){
            ggplot(data_filter(),
                   aes(date_col(), get(input$var1, data_filter()))) +
                ggtitle(paste0("Time Series: ", isolate({input$var1}))) +
                xlab('Fecha') +
                ylab(isolate({input$var1})) +
                geom_point(col = "dodgerblue4") +
                theme(
                    plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=12, face="plain"),
                    axis.title.y = element_text(color="black", size=12, face="plain"))
            
        }})
    
    
    # Time Series Plot Variable 2
    output$timeseries2 = renderPlot({
        
        if(!(is.factor(get(input$var2, data_filter())))){
            ggplot(data_filter(),
                   aes(date_col(), get(input$var2, data_filter()))) +
                ggtitle(paste0("Time Series: ", isolate({input$var2}))) +
                xlab('Fecha') +
                ylab(isolate({input$var2})) +
                geom_point(col = "dodgerblue4") +
                theme(
                    plot.title = element_text(color="black", size=18, face="bold", hjust = 0.5),
                    axis.title.x = element_text(color="black", size=12, face="plain"),
                    axis.title.y = element_text(color="black", size=12, face="plain"))
            
        }})
    
}



shinyApp(ui, server)