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
library(ggthemes)
server <- function(input,output,session){
  
#################################  Data Preparation ##################################
  shinyalert("Thank You!","Welcome to the dashboard", type = "success",immediate=TRUE)
  df <- eventReactive(input$Submit,{
    if(input$Selectdata=="Super Store"){
    
    if (is.null(input$file1)==TRUE){
      Val<<-1
      shinyalert("Oops!","Please choose the file", type = "error")
    } else {
      file <- input$file1
      ext <- file_ext(file$datapath)
      if (ext=='xls'){
        Val <<-0
        dat <- read_excel(file$datapath, sheet = 1)
      }
      else{
        Val <<-1
        dat <- data.frame(0,0,0,0,0,0,0,0,0,0,0,0)
        shinyalert("Oops!", "Please upload the correct file", type = "error")
      }
    }
    }
    else{
      Val <<-1
      shinyalert("Oops!", "Choose the data", type = "error")
    }
    #return(dat)
  })
  tab2data <- function(){
    if (is.null(input$file1)==FALSE) {
      file <- df()
      if (input$Cat=="All Categories" && input$ye=="All years"){
        catSalesTrendFil <- file
      }
      if(input$Cat=="All Categories" && input$ye!="All years")
      {
        catSalesTrendFil <-file
        catSalesTrendFil$Year <- year(catSalesTrendFil$`Order Date`)
        catSalesTrendFil <-catSalesTrendFil %>% filter(Year==input$ye)
      }
      if(input$Cat!="All Categories" && input$ye=="All years")
      {
        catSalesTrendFil <-file
        catSalesTrendFil <-file %>% filter(Category==input$Cat)
        catSalesTrendFil$Year <- year(catSalesTrendFil$`Order Date`)
        
      }
      if(input$Cat!="All Categories" && input$ye!="All years")
      {
        catSalesTrendFil <-file
        catSalesTrendFil <-file %>% filter(Category==input$Cat)
        catSalesTrendFil$Year <- year(catSalesTrendFil$`Order Date`)
        catSalesTrendFil <-catSalesTrendFil %>% filter(Year==input$ye)
      }
      return(catSalesTrendFil)
      
      
    }
  }
  tab3data <- function(){
    if (is.null(input$file1)==FALSE) {
      file <- df()
      data <- file 
      data$Year <- year(data$`Order Date`)
      data <- data %>% filter(Year==2018|Year==2019)
      data <- data %>% select(input$Check)
      return(data)
    }
    
  }
 #################################  Data Display ##################################
  output$maintable <- renderTable({
      file <- df()
      if (Val!=1){
        if (nrow(file)>1){
          dattt <- file[1:100,]
        }
        
      }
  },align = 'c',width="100%",bordered=TRUE)
  

#################################  Visualizations ##################################
## Value Boxes
  output$value1 <- renderValueBox({
    
    if(Val!=1){
      file <- tab2data()
      d <- round(sum(file$Sales),0)/1000
      
    }
    else{
      d<-0
    }
    valueBox( "Total Sales", paste("$",d,"K"),width = 3)
  })
  output$value2 <- renderValueBox({
    if(Val!=1){
      file <- tab2data()
      d <- sum(file$Quantity)
      agg_data_Value<-prettyNum(d,big.mark=",", preserve.width="individual")
    }
    else{
      agg_data_Value<-0
    }
    valueBox( "Total Quantity", width = 3,agg_data_Value)
  })
  output$value3 <- renderValueBox({
    if(Val!=1){
      file <- tab2data()
      d <- round(sum(file$Profit),0)/1000
      
    }
    else{
      d<-0
    }
    valueBox("Total Profit", paste("$",d,"K"),width = 3)
  })

## Sales Trend by month
  output$catSalesTrend <- renderPlotly({
    if(is.null(input$file1)==FALSE) {
      catSalesTrendFil <- tab2data()
      catSalesTrendFil$Month <- month(catSalesTrendFil$`Order Date`)
      catSalesTrendFil$Month <- as.factor(catSalesTrendFil$Month)
      catSalesTrendFil <- aggregate(catSalesTrendFil["Sales"],by=catSalesTrendFil[c('Category','Month')],FUN=sum)
      catdata <<-catSalesTrendFil
      p <- ggplot(catSalesTrendFil, aes(x = Month, y = Sales)) + 
        geom_line(aes(color=Category,group=Category)) + labs(x = "Month", y="Total Sales")
      
      p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

    }
    
  })
  
  
## Sales By sub-category
  output$Subcategorysales <- renderPlotly({
    if(is.null(input$file1)==FALSE) {
      catSalesTrendFil <- tab2data()
      catSalesTrendFil <- aggregate(catSalesTrendFil["Sales"],by=catSalesTrendFil[c('Category','Sub-Category')],FUN=sum)
      subcatdat <<-catSalesTrendFil
      colnames(catSalesTrendFil) <- c("Category","Sub.Category","Sales")
      p <- ggplot(catSalesTrendFil, aes(x = Sub.Category, y = Sales)) +
        geom_col(aes(fill = Category), width = 0.7) + labs(x = "Sub-Category", y="Total Sales")+
        theme(axis.text.x = element_text(angle = 60, size = 15))
      p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    
    
  })
  
## Sales by products
  output$Productsales <- renderPlot({
    if(is.null(input$file1)==FALSE) {
      catSalesTrendFil <- tab2data()
      catSalesTrendFil <- aggregate(catSalesTrendFil["Sales"],by=catSalesTrendFil[c('Category','Product Name')],FUN=sum)
      colnames(catSalesTrendFil) <- c("Category","Product.Name","Sales")
      catSalesTrendFil <- catSalesTrendFil[order(catSalesTrendFil$Sales),]
      catSalesTrendFil <- tail(catSalesTrendFil,5)
      proddat <<-catSalesTrendFil
      p <- ggplot(catSalesTrendFil, aes(x = Product.Name, y = Sales)) +
        geom_col(aes(fill = Category),width = 0.7)+coord_flip() + labs(x = "Sales", y="Products")
      
      
      #theme(axis.text.x = element_text(angle = 60, size = 15))
      p+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
      p
    }
    
  })
  
    
## Sales on map
    
  output$stateMap <- renderHighchart({
    if(is.null(input$file1)==FALSE) {
      n <- 4
      colstops <- data.frame(
        q = 0:n/n,
        c = substring(viridis(n + 1), 0, 7)) %>%
        list_parse2()
      thm <- 
        hc_theme(
          colors = c("#1a6ecc", "#434348", "#90ed7d"),
          chart = list(
            backgroundColor = "transparent",
            style = list(fontFamily = "Source Sans Pro")
          ),
          xAxis = list(
            gridLineWidth = 1
          )
        )
      
      catSalesTrendFil <- tab2data()
      catSalesTrendFil <- catSalesTrendFil %>% group_by(State) %>% 
        summarise_at(vars(Sales),list(Sales = sum))
      statedat <<-catSalesTrendFil
      
      highchart() %>%
        hc_add_series_map(usgeojson, catSalesTrendFil, name = "Sales",
                          value = "Sales", joinBy = c("woename", "State"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>%
        hc_colorAxis(stops = colstops) %>%
        hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_add_theme(thm)  
    }
    
  })
  
    
## Sales by Region
  output$regionpie <- renderPlotly({
    if(is.null(input$file1)==FALSE) {
      catSalesTrendFil <- tab2data()
      catSalesTrendFil <- catSalesTrendFil %>% group_by(Region) %>% 
        summarise_at(vars(Sales),list(Sales = sum))
      regiondat <<-catSalesTrendFil
      fig <- plot_ly(catSalesTrendFil, labels = ~Region, values = ~Sales, type = 'pie')
      fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                            
        
      
      fig
    }
    
    
  })
  
  
    
## Customer Segmentation
  output$Kmeans<- renderTable({
    if(is.null(input$file1)==FALSE) {
      data_clust <- tab3data()
      data_clust1 <- df()
      data_clust1$Year <- year(data_clust1$`Order Date`)
      data_clust1 <- data_clust1 %>% filter(Year==2018|Year==2019)
      data_clust <- scale(data_clust)
      data<-data_clust1
      
      clu <-kmeans(data_clust,3)
      Cluster_val <-clu$cluster
      data <- cbind(data,Cluster_val)
      clustered_data <<-data
      data <- data %>% select("Order ID","Customer Name","Sales","Quantity","Cluster_val")
      colnames(data)<-c("Order.ID","Customer.Name","Sales","Quantity",'Cluster_val')
      dat_cust <- data %>% select("Order.ID","Customer.Name","Cluster_val")
      dat_cust <- dat_cust %>% distinct(Order.ID,Customer.Name,.keep_all = TRUE)
      dat_cust <- dat_cust %>% group_by(Cluster_val) %>%
        summarize(Noofcustomers=n()) %>% as.data.frame()
      dat_order <- data %>% select("Order.ID","Customer.Name","Cluster_val")
      
      dat_order <- dat_order %>% group_by(Cluster_val) %>% 
        summarize(NoOfOrders=length(Order.ID))%>% as.data.frame()
      dat_order <- merge(dat_cust,dat_order,by.x="Cluster_val")
      
      data_rema <- data %>% group_by(Cluster_val) %>% 
        summarize(Totalsale=sum(Sales),TotalQnty = sum(Quantity))%>% as.data.frame()
      
      data_Final <- merge(dat_order,data_rema,by.x="Cluster_val")
      data_Final$"Sales By customer" <- round(data_Final$Totalsale/data_Final$Noofcustomers,2)
      colnames(data_Final) <- c("Cluster Number","No Of Customers","No Of Orders","Sales","Quantity","Sales By Customer")
      data_Final
    }
  },align = 'c',width="100%",bordered=TRUE)
  
  
## Download Handlers
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('Category wise data by monthly', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(catdata, con,row.names = FALSE)
       }
     )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('Sub category wise sales', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(subcatdat, con,row.names = FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('Top 5 products by sales', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(proddat, con,row.names = FALSE)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste('Slaes By State', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(statedat, con,row.names = FALSE)
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste('Slaes By Region', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(regiondat, con,row.names = FALSE)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste('Clustered Data', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(clustered_data, con,row.names = FALSE)
    }
  )
  
  
## Action Buttons navigations
  
  observeEvent(input$Previous, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data")
  })

  observeEvent(input$Next, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Visualization")
  })
  observeEvent(input$Continue, {
    updateTabsetPanel(session, "inTabset",
                      selected = "K Means Clustering")
  })
  observeEvent(input$Previous1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Visualization")
  })
  observeEvent(input$Previous2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Data")
  })
  
}
