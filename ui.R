rm(list = ls())
# Required Packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readxl)
library(shinyalert)
library(xfun)
library(shinyjs)
library(lubridate)
library(ggplot2)
library(plotly)
library(NbClust)
library(highcharter)
library(mapproj)
library(leaflet)
library(viridis)

ui <- 
  
  navbarPage("Super Store Analytics",id = "inTabset",selected ="Data",collapsible=TRUE,
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
             ),
             # First Tab
             tabPanel("Data",
                      sidebarLayout(position ="left",
                                    sidebarPanel(id="tabs1",
                                                 selectInput("Selectdata", label = "Select the data", 
                                                             choices = c("Choose File","Super Store") 
                                                 ),
                                                 fileInput('file1',label = 'Choose a xls File'),
                                                 helpText("Note: Kindly Upload the xls format file"),
                                                 actionButton("Submit","Submit"),
                                                
                                    
                                    ),
                                    mainPanel(
                                      fluidRow(
                                        h3("Super Store Data"),
                                        tableOutput('maintable'),
                                        conditionalPanel(condition = "output.maintable",actionButton("Next","Continue analysis "))
                                        
                                        
                                      )
                                    ))
             ),
             # Second Tab
             tabPanel("Visualization",
                      includeCSS("style.css"),
                      sidebarLayout(position ="left",
                                    sidebarPanel(id="tabs2",width = 3,
                                                 selectInput("Cat", label = "Select the category", 
                                                             choices = c("All Categories","Furniture","Office Supplies","Technology")
                                                 ),
                                                 selectInput("ye", label = "Select Year", 
                                                             choices = c("All years",2016,2017,2018,2019)
                                                 ),
                                                 tags$br(),
                                                
                                                 tags$br(),
                                    ),
                                    mainPanel(
                                      HTML('<div class="row">'),
                                      frow1 <- fluidRow(
                                        conditionalPanel(condition = "output.maintable",
                                        valueBoxOutput("value1")
                                        ,valueBoxOutput("value2")
                                        ,valueBoxOutput("value3")
                                        ,HTML('</div>')
                                      )),
                                      frow2 <- fluidRow(
                                        conditionalPanel(condition = "output.maintable",
                                                         box(
                                                           downloadButton('downloadData', 'DownloadData'),
                                                           width=12,
                                                           title = "Category Sales Trend"
                                                           ,status = "primary"
                                                           ,solidHeader = TRUE 
                                                           ,collapsible = TRUE 
                                                           ,plotlyOutput("catSalesTrend") 
                                                         ))),
                                        
                                        frow3 <- fluidRow(
                                          conditionalPanel(condition = "output.maintable",
                                                           box(
                                                             downloadButton('downloadData1', 'DownloadData'),
                                                             width=12,
                                                             title = "Sales by Sub-category"
                                                             ,status = "primary"
                                                             ,solidHeader = TRUE 
                                                             ,collapsible = TRUE 
                                                             ,plotlyOutput("Subcategorysales")
                                                           ))),
                                          
                                        frow4 <- fluidRow(
                                          conditionalPanel(condition = "output.maintable",
                                                           box(
                                                             downloadButton('downloadData2', 'DownloadData'),
                                                             width=12,
                                                             title = "Top 5 product Sales"
                                                             ,status = "primary"
                                                             ,solidHeader = TRUE 
                                                             ,collapsible = TRUE 
                                                             ,plotOutput("Productsales")
                                                           ))),
                                          
                                        frow5 <- fluidRow(
                                          conditionalPanel(condition = "output.maintable",
                                                           box(
                                                             downloadButton('downloadData3', 'DownloadData'),
                                                             title = "Sales by State"
                                                             ,status = "primary"
                                                             ,solidHeader = TRUE 
                                                             ,collapsible = TRUE 
                                                             ,highchartOutput("stateMap", height = "300px")
                                                           ),
                                                           box(
                                                             downloadButton('downloadData4', 'DownloadData'),
                                                             title = "Sales by Region"
                                                             ,status = "primary"
                                                             ,solidHeader = TRUE 
                                                             ,collapsible = TRUE 
                                                             ,plotlyOutput("regionpie")
                                                           ),
                                          actionButton("Previous","Previous Page"),
                                          actionButton("Continue","Start Analysis"),
                                          )),
                                                           
                                          
                                      )
                      )),
                                    
             # Third Tab
             tabPanel("K Means Clustering",
                      sidebarLayout(position ="left",
                                    sidebarPanel(id="tabs3",width = 3,
                                                 checkboxGroupInput("Check", "Choose the variables", 
                                                                    choices  = c("Sales","Quantity","Discount","Profit"),
                                                                    selected = c("Sales","Profit")
                                                                    ),
                                                 
                                    ),
                                    mainPanel(
                                      
                                      conditionalPanel(condition = "output.maintable",
                                                       downloadButton('downloadData5', 'DownloadFullSegmenteddData'),
                                                       h3("Customer Segmentation"),
                                                       tableOutput("Kmeans"),
                                      actionButton("Previous1","Previous Page"),
                                      actionButton("Previous2","Home Page"),
                                        
                                      )
                                    )))
                                    
                      
                      
             
             
  )

