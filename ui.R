#IVA 18.4.17 / 5.5.17

# install.packages("shinythemes")
# install.packages('shinythemes', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("shinyjs")
# 

library(shiny)
library(shinythemes)
library(shinyjs)
require(lubridate)
library(dplyr)
library(plotly)

library(zoo)
library(grnn)
library(ggplot2)
library(reshape2)
library(imputeTS)
require(grt)
require(foreach)
require(lubridate)

#require(compiler)
#enableJIT(3)

shinyUI(
  tagList(
    navbarPage(
      "",
      #"GRNNCLI",
      tabPanel("HOME",
      # theme = "cerulean",  # <--- To use a theme, uncomment this "shinythemes",
      fluidPage( theme = shinytheme("cerulean"), #flatly #darkly #spacelab #united # paper
      useShinyjs(),
      #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
    sidebarLayout(
      sidebarPanel(
        fileInput('filecli', 'Choose upload .CLI File', # object in which NAs are to be replaced
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv', '.cli', '.CLI')),
        tags$div("Here is a sample",
               a("data file,",
                 href = "https://gist.github.com/anonymous/282df1e0d1c1c6ce9c02e1de347b2128"),
               "describing weather observations"),
        checkboxInput('vscli', 'VS-Oscilloscope Pascal format .CLI', TRUE), 
        tags$hr(),
        radioButtons ( "inRadioButtonsCliFormats" , "Other climatic data representation formats" ,
                       c ( "VS-R Shiny format .cli" , 
                           "VS-Fortran 5 Classic format .CLI" 
                           #,
                           #"VS-Fortran 5 China format .CLI",
                           #"aisori.meteo.ru/ClimateR"
                           )),
        tags$hr(),
        # https://github.com/daattali/shinyjs/blob/master/inst/examples/basic/app.R
        a(id = "toggleAdvanced", "Show/hide advanced info"),
        hidden(
          div(id = "advanced",
              column(12, checkboxInput('header', 'Header', FALSE)),
              column(6, 
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'), ';')),
              column(6,
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"'))
          ) # div
        ), # hidden
        
        tags$hr(),
        selectInput("dataset", "Choose a .CLI dataset:", 
                  choices = c("krest1977", "krest1967", "krest1969"), selected = "krest1967"),
      # R shiny; how to use multiple inputs from selectInput to pass onto 'select' option in dplyr?
      # http://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
      # selectInput("Columns","Columns", names(mtcars), multiple = TRUE),
        tags$hr(),
      # *********************************************************************
        checkboxInput('manualSigma', 'Manual modification of Sigma', TRUE),
      
        #https://rdrr.io/cran/imputeTS/man/na.kalman.html
        selectInput("replaceNA", "Replace NA by:", 
                  choices = c("GRNN", "PSO-GRNN", "Kalman Smoothing", "Spline"), 
                  selected = "GRNN"),
        tags$hr(),
      # https://gist.github.com/aagarw30/9c60b87e839db05b8dcc
        downloadButton('downloadData', 'Save Results to File', class = "butt"),
        tags$hr(),
        tags$div("Created by Iljin Victor, 2017", style = "color:green")#,
      #tags$h1('LORA')
    ),
    ##########################################
    mainPanel( 
      tabsetPanel( # http://rstudio.github.io/shiny/tutorial/#tabsets
        tabPanel("Overview", 
                 #plotOutput('contentsPlot'),
                 plotlyOutput('plotlyPrecTemp'), #, height = "720px", width = "920"),
                 #plotOutput('plotPrecTemp'),
                 tags$br(),
                 div(id = "centralPlot",
                 column(4, checkboxInput('showlegendOne', 'legend', TRUE)),
                 column(4, checkboxInput('showrangesliderOne', 'rangeslider', FALSE)),
                 column(4, checkboxInput('hovermodeOne', 'hovermode', TRUE))
                 ),
                 hr(),
                 #plotOutput('contentsPlotPrec'),
                 verbatimTextOutput("summaryCli"), hr()),
        tabPanel("Table", dataTableOutput('tableCli')),
        tabPanel("Prec NAs replaced", 
                 plotOutput('precNA'), tags$hr(),
                 sliderInput("sigmaPrecGRNN", label = "Changing the value of Sigma - Prec", 
                             min = 0.01, 
                             max = 0.9, value = 0.16),
                 numericInput('countPrecTest', 'The size of the test sample - Prec', 69,
                              min = 21, max = 191),
                 verbatimTextOutput("precNAInfo")
                 ),
        tabPanel("Temp NAs replaced", 
                 plotOutput('tempNA'), tags$hr(), 
                 # http://stackoverflow.com/questions/35136029/hide-show-outputs-shiny-r
                 #conditionalPanel("output.sigmaShow", 
                 #                 sliderInput("sigmaGRNN", label = h3("Slider"), min = 0.1, 
                 #                             max = 0.9, value = 0.56)),
                 sliderInput("sigmaTempGRNN", label = "Changing the value of Sigma - Temp", 
                             min = 0.01, 
                                                          max = 0.9, value = 0.24),
                 numericInput('countTempTest', 'The size of the test sample - Temp', 69,
                              min = 21, max = 191),
                 verbatimTextOutput("tempNAInfo")
                 )
        #tabPanel("PlotUpload",
        #         plotOutput('contentsPlotUpload'),
        #         plotOutput('contentsPlotUpload2')),
        #tabPanel("SummaryUpload",verbatimTextOutput("summaryUpload")),
        #tabPanel("TableUpload", dataTableOutput('contentsUpload'))
      )
    )
  )
)),
############################
### imputeCli::Plotly    ###
############################
tabPanel("imputeCli",
         sidebarLayout(
           sidebarPanel(
             tags$div("Here is a sample",
                      a("data file,",
                        href = "https://gist.github.com/anonymous/282df1e0d1c1c6ce9c02e1de347b2128"),
                      "describing weather observations"),
             tags$hr(),
             sliderInput("sigmaPrecPlotly", label = "Changing the value of Sigma - Prec", 
                         min = 0.01, 
                         max = 0.9, value = 0.27),
             tags$hr(),
             sliderInput("sigmaTempPlotly", label = "Changing the value of Sigma - Temp", 
                         min = 0.01, 
                         max = 0.9, value = 0.16),
             checkboxInput('mode.edom', tags$b('PlotLy lines+markers'), TRUE),
             checkboxInput('plotlyShowrangesI', tags$b('PlotLy showranges'), FALSE),
             tags$hr(),
             verbatimTextOutput("yearImpute"),
             tags$hr(),
             div("TODO:", tags$br(), tags$br(), tags$ul( 
                 tags$li(" Interactively change the color graphics "),
                 tags$li(" Interactively change the color graphics ")
             )),
             tags$hr(),
             downloadButton('downloadDataPlotly', 'Download Plot Data', class = "butt"),
             tags$hr(),
             tags$div(a("Created by Iljin Victor, 2017"))
           ), # sidebarPanel
         mainPanel(
           fluidRow(
             # https://plot.ly/r/shiny-coupled-events/
             plotlyOutput("PlotlyPrec"), #, height = "600px", width = "640"
             tags$hr(),
             plotlyOutput("PlotlyTemp") #height = "640px", width = "920")
           )), # mainPanel
         position = c("right"),
         fluid = TRUE) # sidebarLayout
         ),
############################
###  VS-Shiny            ###
############################
tabPanel("About"),
tabPanel("ContactUS", #http://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
         mainPanel(fluidRow(
           tags$h1("ContactUS"),
           tags$p("General questions, comments, requests, please, send to", a("vilin@sfu-kras.ru") ,"(Victor)."),
           tags$hr(),
           htmlOutput("frameContactUS"),
           tags$hr()
         )
         ) # DEMO ShinyJS
)
)))
#

