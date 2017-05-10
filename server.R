# http://deanattali.com/blog/building-shiny-apps-tutorial/#4-load-the-dataset
library(shiny)
library(zoo)
library(grnn)
library(ggplot2)
library(reshape2)
library(imputeTS)
require(grt)
require(foreach)
library(shinyjs)
require(lubridate)
library(plotly)

# load IVA_scripts
source("na_spline_cli.R")
source("na_grnn_cli.R")
source("na_kalman_cli.R")
source("global-write.R")
source("global-plot.R")
source("global-read.R")

#require(compiler)
#enableJIT(3)

read.year <- function(fileName) {
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  return(cli_dataset[1, 3])
}

createDF <- function(fileName){
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4] 
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]
  days <- c(1:dim(cli_dataset)[1])
  return(data.frame(days=days, prec=prec_vector, temp=temp_vector))
}
## read global data.frame from disk
file_name <- '1977.cli'; krest1977 <- createDF(file_name)
file_name <- '1967.cli'; krest1967 <- createDF(file_name)
file_name <- '1969.cli'; krest1969 <- createDF(file_name)

YEAR <- read.year(file_name)
##
shinyServer(
  function(input, output, session) {
  # https://github.com/daattali/shinyjs/blob/master/inst/examples/basic/app.R
  # http://deanattali.com/shinyjs/
  
  onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))  
    
  observe({ # input$vscli,
    #http://stackoverflow.com/questions/34733147/unable-to-disable-a-shiny-app-radio-button-using-shinyjs
    if(input$vscli == TRUE) {
      shinyjs::disable(selector = "[type=radio][value='VS-R Shiny format .cli']")
      shinyjs::disable(selector = "[type=radio][value='VS-Fortran 5 Classic format .CLI']")
      shinyjs::disable(selector = "[type=radio][value='VS-Fortran 5 China format .CLI']")
      shinyjs::disable(selector = "[type=radio][value='aisori.meteo.ru/ClimateR']")
    }
    else {
      shinyjs::enable(selector = "[type=radio][value='VS-R Shiny format .cli']")
      shinyjs::enable(selector = "[type=radio][value='VS-Fortran 5 Classic format .CLI']")
      shinyjs::enable(selector = "[type=radio][value='VS-Fortran 5 China format .CLI']")
      shinyjs::enable(selector = "[type=radio][value='aisori.meteo.ru/ClimateR']")
    }
  })  
  
  observe({
    #if (input$showlegendOne)
    if (input$showlegendOne)
      layout$showlegend <- TRUE
    else
      layout$showlegend <- FALSE  
    plotlyPrecTemp(trace1, trace2, layout, input$showrangesliderOne)
  })
    
  observeEvent(input$button, {
    toggle("countTest")
  })
 
  observeEvent(input$manualSigma, {
    
    
    if(input$manualSigma == TRUE){
      shinyjs::hide("countPrecTest")
      shinyjs::hide("countTempTest")
      shinyjs::show("sigmaPrecGRNN")
      shinyjs::show("sigmaTempGRNN")
    }else{
      shinyjs::hide("sigmaPrecGRNN")
      shinyjs::hide("sigmaTempGRNN")
      shinyjs::show("countPrecTest")
      shinyjs::show("countTempTest")
    }
  })
  #
  values <- reactiveValues()
  values$sigmaShow <- FALSE
  # 
  methodNA <- reactive({
    
  })
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "krest1977" = krest1977,
           "krest1967" = krest1967,
           "krest1969" = krest1969)
    
  })
  
  datasetInputFileName <- reactive({
    switch(input$dataset,
           "krest1977" = '1977.cli',
           "krest1967" = '1967.cli',
           "krest1969" = '1969.cli')
    
  })
  #
  currentFileInput <- reactive({
    inFile <- input$filecli
    
    if (is.null(inFile)){
      datasetInput()
      #return(NULL)
    }else{
      if(input$vscli){
        createDF(inFile$datapath)
      }else{
        read.csv(inFile$datapath, header=input$header, sep=input$sep,
                 quote=input$quote)
      }
    }
  })
  
  ##############################
  ### getFileYear
  ##############################
  
  getFileYear <- reactive ({
    inFile <- input$filecli
    if (is.null(inFile)){
      cat("Preloads year", read.year(getNameFileCli()), "\n")
    }else{
      cat("Uploads year", read.year(getNameFileCli()), "\n")
    }
  })
  #
  getFileCli <- reactive({
    inFile <- input$filecli
    if (is.null(inFile)){
      cat("Preloads .CLI", input$dataset, "\n")
    }else{
      cat("Uploads .CLI\n"); cat(str(inFile), "\n")
    }
  })
  #
  getNameFileCli <- reactive({
    inFile <- input$filecli
    if (is.null(inFile)){
      datasetInputFileName()
      #input$dataset
    }else{
      inFile$name
    }
  })
  #
  output$summaryCli <- renderPrint({
    cat(getFileCli(), getFileYear(), "***\n")
    #cat(getFileCli(), "***\n")
    str(currentFileInput())
    cat("***\n")
    summary(currentFileInput())
  })
  
  ##############################
  ### precNA 
  ##############################
  
  output$precNA <-renderPlot({
    if(input$replaceNA == "GRNN"){
      if(input$manualSigma == TRUE){
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countPrecTest, input$sigmaPrecGRNN, FALSE, FALSE)
      }else{
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countPrecTest, 0.01, TRUE, FALSE)
      }
      #par(mfrow=c(2,1))
      plotPrecFilled(currentFileInput(), na.na.grnn, 
                     paste0(getNameFileCli(), " - GRNN"))
      #
      #na.na <- na.spline.cli(currentFileInput())
      #plotTempFilled(currentFileInput(), na.na, getNameFileCli())
    }else if(input$replaceNA == "Spline"){
      na.na <- na.spline.cli(currentFileInput())
      plotPrecFilled(currentFileInput(), na.na, paste0(getNameFileCli(), "Spline"))
    }
  })
  
  output$precNAInfo <- renderPrint({
    cat("***\n")
    cat("Method inputation= ", input$replaceNA, "\n")
    cat("***\n")
    summary(na.spline.cli(currentFileInput()))
    
    })
  #
  output$tempNA <-renderPlot({
    if(input$replaceNA == "GRNN"){
      if(input$manualSigma == TRUE){
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countTempTest, input$sigmaTempGRNN, FALSE, FALSE)
      }else{
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countTempTest, 0.01, TRUE, FALSE)
      }
      
      #par(mfrow=c(2,1))
      plotTempFilled(currentFileInput(), na.na.grnn, 
                     paste0(getNameFileCli(), " - GRNN"))
      #
      #na.na <- na.spline.cli(currentFileInput())
      #plotTempFilled(currentFileInput(), na.na, getNameFileCli())
    }else if(input$replaceNA == "Spline"){
      na.na <- na.spline.cli(currentFileInput())
      plotTempFilled(currentFileInput(), na.na, paste0(getNameFileCli(), " - Spline"))
    }
  })
  
  output$tempNAInfo <- renderPrint({
    cat("***\n")
    cat("Method inputation= ", input$replaceNA, "\n")
    cat("***\n")
    summary(na.spline.cli(currentFileInput()))
  })
  #################################
  
  output$contentsPlot <- renderPlot({
    # ggvis: Similar to ggplot2, but the plots are focused on being web-based and are more interactive
    # todo plot 2 
    # Plotting two graphs, one below the other, in shiny panel
    # http://stackoverflow.com/questions/33204243/plotting-two-graphs-one-below-the-other-in-shiny-panel
    #plot(datasetInput(), col="red")
    plot(datasetInput(), col="blue")
  })
  
  ##############################
  # plotPrecTemp
  ##############################
  
  output$plotPrecTemp <- renderPlot({
    #http://stackoverflow.com/questions/27350243/ggplot-line-graph-with-different-line-styles-and-markers
    #How can I plot with 2 different y-axes?
    #http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
    
    ## set up some fake test data
    #time <- seq(0,72,12)
   # betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
    #cell.density <- c(0,1000,2000,3000,4000,5000,6000)
    
    time <- currentFileInput()[,1]
    betagal.abs <- currentFileInput()[,2]; precVect <- currentFileInput()[,2]
    cell.density  <- currentFileInput()[,3]; tempVect <- currentFileInput()[,3]
      
    ## add extra space to right margin of plot within frame
    par(mar=c(5, 4, 4, 6) + 0.1)
    
    if (input$colorPrecTemp) { 
      colTemp <-  '#FF7F0E' #'darkred'
      colPrec <-  '#1F77B4' #"darkblue"
    }
    else {
      colTemp <-  'darkred'
      colPrec <-  "darkblue"
    }
    
    
    ## calculation of minimum maximum value of a vector
    ylimMinMAx <- c(min(tempVect, na.rm=TRUE), max(tempVect, na.rm=TRUE))
    namePlot <- getNameFileCli()
    ## Plot first set of data and draw its axis
    plot(time, tempVect, pch=16, axes=FALSE, ylim=ylimMinMAx, xlab="", ylab="", 
         type="b",col=colTemp , main=paste0("Climatic data ", namePlot)) # !!!!!
    axis(2, ylim=ylimMinMAx, col=colTemp , col.axis=colTemp , las=1)  ## las=1 makes horizontal labels
    mtext(" Temp(c)*10",side=2,line=2.5, col=colTemp )
    box()
    
    ## Allow a second plot on the same graph
    par(new=TRUE)
    
    ## calculation of minimum maximum value of a vector
    ylimMinMAx <- c(min(precVect, na.rm=TRUE), max(precVect, na.rm=TRUE))
   
    ## Plot the second plot and put axis scale on right
    plot(time, precVect, pch=15,  xlab="", ylab="", ylim=ylimMinMAx, 
         axes=FALSE, type="b", col=colPrec)
    ## a little farther out (line=4) to make room for labels
    mtext("Prec(mm)*10",side=4,col=colPrec,line=4) 
    axis(4, ylim=ylimMinMAx, col=colPrec,col.axis=colPrec,las=1)
    
    ## Draw the time axis
    axis(1,pretty(range(time),10))
    mtext("Time (days)",side=1,col="black",line=2.5)  
    
    ## Add Legend
    legend("topleft",legend=c("Prec","Temp"),
           text.col=c(colPrec,colTemp ),pch=c(16,15),col=c(colPrec,colTemp ))
  })
  
  ##############################
  ### plotlyPrecTemp
  ##############################
  
  output$plotlyPrecTemp <- renderPlotly({ 
    if (input$showlegendOne){
      layout$showlegend <- TRUE
      shinyjs::reset('showrangesliderOne')  
    }
    else
      layout$showlegend <- FALSE
    
    if (input$hovermodeOne)
      layout$hovermode <- 'x'
    else
      layout$hovermode <- 'closest'
    
    time <- currentFileInput()[,1]
    precVect <- currentFileInput()[,2]
    tempVect <- currentFileInput()[,3]
    ## calculation of minimum maximum value of a vector
    yTemplimMinMAx <- c(min(tempVect, na.rm=TRUE-10), max(tempVect, na.rm=TRUE)+10)
    yPreclimMinMAx <- c(min(precVect, na.rm=TRUE-10), max(precVect, na.rm=TRUE)+10)
    
    trace1$x <- time
    trace1$y <- tempVect
    trace2$x <- time
    trace2$y <- precVect
    
    layout$yaxis2$range <- yPreclimMinMAx
    
    plotlyPrecTemp(trace1, trace2, layout, input$showrangesliderOne)
  })
  
  output$contentsPlotPrec <- renderPlot({
    plot(datasetInput()[,1],datasetInput()[,2], col="red")
  })
  
  # 
  output$tableCli <- renderDataTable({
    currentFileInput()
    #datasetInput()
  }, options = list(lengthMenu = c(16, 30, 50), pageLength = 16))
  
  output$downloadData2 <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  ) 
  
  ##############################
  ### DownloadsData
  ##############################

  output$downloadData <- downloadHandler(
    filename = function() { # create file name
      paste0(getNameFileCli(), '.filled')
    },
    content = function(file) {
      filled.grnn.cli <- na.grnn.cli(currentFileInput(), 108, 
                                     input$sigmaPrecPlotly, FALSE, FALSE)
      ymd.df <- as.data.frame.ymd(read.year(getNameFileCli()))
      filled.cli <- data.frame(ymd.df, filled.grnn.cli$prec, filled.grnn.cli$temp)
      # 
      write.table(filled.cli, file, col.names=FALSE,row.names=FALSE, sep="\t")
    } 
  ) # downloadData <- downloadHandler(
  
  ##############################
  ### downloadDataPlotly
  ##############################
  
  output$downloadDataPlotly <- downloadHandler(
    filename = function() { # create file name
      paste0(getNameFileCli(), '.filled')
    },
    content = function(file) {
      filled.grnn.cli <- na.grnn.cli(currentFileInput(), 108, 
                           input$sigmaPrecPlotly, FALSE, FALSE)
      ymd.df <- as.data.frame.ymd(read.year(getNameFileCli()))
      filled.cli <- data.frame(ymd.df, filled.grnn.cli$prec, filled.grnn.cli$temp)
      # 
      write.table(filled.cli, file, col.names=FALSE,row.names=FALSE, sep="\t")
    }  
  ) # downloadDataPlotly <- downloadHandler(
  
  ##############################
  ### frameContactUS
  ##############################
  
  output$frameContactUS <- renderUI({ # 
    view_frameContactUS <- tags$iframe(src="https://trace2017.typeform.com/to/ZurLTf", height=410, width=600)
    print(view_frameContactUS)
    view_frameContactUS
  })
  
  ##############################
  ### frameShinyJS
  ##############################
  
  output$frameShinyJS <- renderUI({ # https://shiny.rstudio.com/articles/tag-glossary.html
    demoShinyJS <- tags$iframe(src="https://daattali.com/shiny/shinyjs-mini-demo/", height=600, width=900)
    print(demoShinyJS)
    demoShinyJS
  })
  
  output$yearImpute <- renderPrint({ 
    cat(getNameFileCli(), read.year(getNameFileCli()), input$mode.edom)
  })
  
  ###############################
  ### Plotly
  ###############################
  output$PlotlyPrec <- renderPlotly({
    #plotlyMarkersLines()
    grnn.impute <- na.grnn.cli(currentFileInput(), 106, input$sigmaPrecPlotly, FALSE, FALSE)
    plotlyVect(currentFileInput()$prec, grnn.impute$prec,
               paste0(getNameFileCli(), " Prec - GRNN-R"), 
               "Precipitation in millimeters", "Days", 
               mode_line = input$mode.edom, mode_slider = input$plotlyShowrangesI)
  }) # output$PlotlyPrec
  
  output$PlotlyTemp <- renderPlotly({
    #plotlyNAMarkersLines()
    grnn.impute <- na.grnn.cli(currentFileInput(), 106, input$sigmaTempPlotly, FALSE, FALSE)
    plotlyVect(currentFileInput()$temp, grnn.impute$temp,
               paste0(getNameFileCli(), " Temp - GRNN-R"), 
               "Temperature in degrees Celsius", "Days", 
               mode_line = input$mode.edom, mode_slider = input$plotlyShowrangesI)
  }) # output$PlotlyTemp
  
  })
#

