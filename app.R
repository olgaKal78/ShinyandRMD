#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
 #
    # This is a Shiny web application. You can run the application by clicking
    # the 'Run App' button above.
    #
    # Find out more about building applications with Shiny here:
    #
    #    http://shiny.rstudio.com/
    #
    
 library(shiny)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(knitr)
library(rgdal)
library(pander)
library(kableExtra)
library(leaflet)
library(leaflet.extras)
library(mapedit)
library(readr)
library(ncdf4)
library(ggplot2)
library(rasterVis)
library(sf)
library(raster)
library(sp)
library(knitr)
library(htmltools)
library(plyr)
library(dplyr)

map <- leaflet() %>%
    addTiles()%>% setView(lng=-12,lat=53,zoom=6)%>%
    addDrawToolbar(polylineOptions = FALSE,circleOptions = FALSE,
                   markerOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   editOptions = editToolbarOptions()
    )%>%
    addWMSTiles("https://gis.ices.dk/gis/services/ICES_reference_layers/ICES_Areas/MapServer/WMSServer?",
                layers = "0",
                options = WMSTileOptions(format = "image/png", transparent = TRUE, crs = "EPSG:4326"))

button_color_css <- "
 #DivCompClear, #FinderClear, #EnterTimes{
 /* Change the background color of the update button
 to blue. */
 background: DodgerBlue;
 /* Change the text size to 15 pixels. */
 font-size: 15px;
}"
# Define UI for application that draws a histogram
ui = fluidPage(theme = shinytheme("lumen"),
               tags$head(tags$style(".shiny-notification{
                 position: fixed;
                 top: 33%;
                 left: 33%;
                 right: 33%;
                 background-color: lightgrey;
                  border: 1px solid #4CAF50;
                 
               }")),
               titlePanel("Use Case: Linking Ecosystem and Fisheries Data"),
               navbarPage(title = div(img(src="Niamh.png", height = 35,
                                          width = 500)),
                          
                          tabPanel("Data selection and Report Generation", fluid = TRUE,
                                   # Sidebar layout with a input and output definitions
                                   sidebarLayout(
                                       sidebarPanel(
                                           h4(HTML("<u> Step 1: Select Area of Interest </u>"))
                                           ,
                                           selectInput("select", "Select Coordinates:", choices = c("","interactively","manually")),
                                           
                                           conditionalPanel(condition = "input.select == 'manually'",
                                                            
                                                            textInput('xcoord', 'Enter x coordinates (separated by comma)'),
                                                            textInput('ycoord', 'Enter y coordinates (separated by comma)')
                                                            ,
                                                            actionBttn(
                                                                inputId = "viewP",
                                                                label = "Validate selected polygon",
                                                                color = "success"
                                                            )),
                                           conditionalPanel(condition = "input.select == 'interactively'",
                                                            textInput('xcoord2', 'Enter x coordinates (separated by comma)'),
                                                            textInput('ycoord2', 'Enter y coordinates (separated by comma)')
                                           ), h4(HTML("<u> Step 2: Data Sources </u>")),
                                           awesomeCheckboxGroup(
                                               inputId = "Data2", label="",
                                               choices = c(
                                                   "Bathymetry"
                                               )),
                                           
                                           conditionalPanel(condition = "input.Data2.includes('Bathymetry')",
                                                            prettyCheckboxGroup(inputId = "DS2",
                                                                                label = "", icon = icon("check"),
                                                                                choices = c("height_above_reference_ellipsoid"),
                                                                                selected="height_above_reference_ellipsoid",
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           
                                           awesomeCheckboxGroup(
                                               inputId = "Data4", label="",
                                               choices = c(
                                                   "Copernicus Ocean Physics"
                                               )),
                                           conditionalPanel(condition = "input.Data4.includes('Copernicus Ocean Physics')",
                                                            sliderInput("slider2", label = "Year", min = 1993, 
                                                                        max = 2019, value = c(2016, 2019),sep = ""),
                                                            selectizeInput('month2', 'Month', month.name, multiple=TRUE,selected = "January", options = list(maxItems = 3) ),
                                                            prettyCheckboxGroup(inputId = "DS4",
                                                                                label = "", icon = icon("check"),
                                                                                choices = dput( as.vector(read.csv("Data/PhysDesc.csv")[[2]])),
                                                                                animation = "tada", status = "default",inline = TRUE)),
                                           h4(HTML("<u> Step 3: Generate Report </u>")),
                                           #useShinyjs(),  # Set up shinyjs
                                           actionBttn(
                                               inputId = "report",
                                               label = "View and Save report",
                                               #color = "success",
                                               style = "fill",
                                               icon = icon("file-alt"),
                                               block = TRUE
                                           ),  h4(HTML("<u> Step 4: Download Results </u>")),
                                           downloadButton("downloadResults", label = "Download")
                                           ),mainPanel(tabsetPanel( id="inTabset",selected ="Data Description",
                                                                    tabPanel("Data Description",uiOutput("dataD")),
                                                                    tabPanel("Area Selection Explorer",uiOutput("area"),uiOutput("action")),                      
                                                                    tabPanel("Report",uiOutput("test"))))))))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    output$action<-renderUI({
        if(input$select=="interactively"){
            actionBttn(
                inputId = "save",
                label = "Show selected coordinates",
                color = "success"
            )
        }
        else {}
    })
    
    observeEvent(input$viewP,{
        
        updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
        xcoord<-as.numeric(unlist(strsplit(input$xcoord, ",")))
        ycoord<-as.numeric(unlist(strsplit(input$ycoord, ",")))
        
        
        output$summary <- renderText({
            
            return(paste(xcoord,ycoord))
            
        })
        
        output$polP <- renderPlot({
            
            xym <- cbind(xcoord, ycoord)
            p = Polygon(xym)
            ps = Polygons(list(p),1)
            sps = SpatialPolygons(list(ps))
            proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
            plot(sps,axes=T)
            
        })
        output$area <- renderUI({
            
            list(verbatimTextOutput("summary"),plotOutput("polP"))
            
            
            
        })
    })
    
    
    
    observeEvent(input$select, { 
        updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
        if(input$select=="interactively"){
            
            edits <- callModule(
                editMod,
                leafmap = map,
                id = "map"
            )
            observeEvent(input$save, {
                updateTabsetPanel(session, "inTabset",selected ="Area Selection Explorer")
                x_geom <- edits()$finished$geometry[[1]][[1]][,1]
                y_geom<-edits()$finished$geometry[[1]][[1]][,2]
                updateTextInput(session, "xcoord2", value = x_geom)
                updateTextInput(session, "ycoord2", value = y_geom)
                if (!is.null(x_geom)) {
                    output$xcoord<-  renderText({x_geom})}
                if (!is.null(y_geom)) {
                    output$ycoord<-  renderText({y_geom})} 
                
                output$note<- renderText({ "!!!After every selection click Recycling Bin to clear selected layer"})
                
            })
            output$area<-renderUI(
                list( editModUI("map"),
                      "x coordinates:",br(),
                      verbatimTextOutput("xcoord"),
                      "y coordinates:",br(),
                      verbatimTextOutput("ycoord"),
                      
                      verbatimTextOutput("note")
                ))}
        else{ 
            
            output$selectedX<-renderText(input$xcoord)
            output$selectedY<-renderText(input$ycoord)
            
            output$area <- renderUI({
                
                list(
                    "x coordinates:",br(),
                    textOutput("selectedX"),
                    "y coordinates:",br(),
                    textOutput("selectedY"))
                
                
                
                
            })}
    })
    
    
    observeEvent(input$report, {
        updateTabsetPanel(session, "inTabset",selected = "Report")
        ####################0.Select area of interest####
        #################################################
        
        if(input$select=="interactively"){
            params <- list(xcoord = input$xcoord2,
                           ycoord = input$ycoord2,
                           Data2=input$Data2,
                           DS2=input$DS2,
                           Data4=input$Data4,
                           DS4=input$DS4,
                           y2=input$slider2,
                           m2=input$month2,
                           rendered_by_shiny = TRUE
            )
        }
        else if (input$select=="manually"){
            params <- list(xcoord = input$xcoord,
                           ycoord = input$ycoord,
                           Data2=input$Data2,
                           DS2=input$DS2,
                           Data4=input$Data4,
                           DS4=input$DS4,
                           y2=input$slider2,
                           m2=input$month2,
                           rendered_by_shiny = TRUE
                           
            )  
        }
        
        
        output$test <- renderUI({
            list(#paste("Saved Report Location: ",paste0(getwd(),"/results")),
                 withProgress(message = 'Generating Report',includeHTML(rmarkdown::render("report.Rmd",
                                                                                          params = params,output_dir = paste0(getwd(),"/results")
                 ))))
            
        })
    })
    # output$dataD <- renderUI({
    #     updateTabsetPanel(session, "inTabset",selected = "Data Description")
    #     includeHTML("Data-Sources.html")})
    
    
   
    output$downloadResults <- downloadHandler(
        filename <- function() {
            paste("output", "zip", sep=".")
        },
        
        content <- function(file) {
            zip(file, "results/")
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
