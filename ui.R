## app.R ##

##Libraries #################
library(shinydashboard)
library(shiny)
library(corrgram)
library(datasets)
library(xlsx)
library(readr)
library(dplyr)
library(ggvis)
library(tree)
library(BH)
library(tigerstats)
library(vcd)
library(cramer)
library(lsr)
library(psych)
library(tree)
library(randomForest)
library(DT)
library(rpart)
library(car)
library(lattice)
library(effsize)
#library(scatterplot3d)
#library(rgl)



##Beginning of Shiny Code #####################################################

##UI Code ####
ui <- dashboardPage(skin = "blue",
                    # skin = "green",
                    dashboardHeader(title = "Data Analysis"),
                    dashboardSidebar( 
                      sidebarMenu(
                        ##Tab Item One #####
                        menuItem("File Upload",tabName = "file",icon = icon("file-excel-o")),
                        ##Tab Item Two
                        menuItem("Plot", tabName = "plot1", icon = icon("line-chart")),
                        ##Tab Item Three
                        # menuItem("Table", tabName = "table", icon = icon("table")),
                        ##Testing Tab
                        menuItem("Table", tabName = "tabletwo", icon = icon("table")),
                        ## Subsetting Tab
                        menuItem("Data Subset",tabName = "subset",icon = icon("gears")),
                        ##Tab Item Four
                        menuItem("Cross Tabulation",tabName = "crosstab", icon = icon("table")),
                        ##Tab Item Five
                        menuItem("Summary",tabName = "summary",icon = icon("list-alt")),
                        ##Tab Item Six
                        menuItem("Correlation",tabName = "correlation",icon = icon("gears")),
                        ##Tab Item Seven
                        menuItem("Correlation Matrix",tabName = "correlationm",icon = icon("th")),
                        ##Tab Item Eight
                        menuItem("Correlogram",tabName = "correlogram",icon = icon("picture-o")),
                        ##Tab Item Fourteen
                        menuItem("Chi-Square",tabName = "chisquare",icon = icon("gears")),
                        ##Tab Item Nine
                        menuItem("Simple Linear Regression",tabName = "linearregression",icon = icon("line-chart")),
                        ##Tab Item Fifteen
                        menuItem("Simple Linear Regression Plot",tabName = "linearregressionplot",icon = icon("line-chart")),
                        ##Tab Item Ten
                        menuItem("Multiple Linear Regression",tabName = "linearregressionm",icon = icon("line-chart")),
                        ##Tab Item Eleven
                        menuItem("Histogram", tabName = "histogram",icon = icon("bar-chart")),
                        ##Tab Item Twelve
                        menuItem("Interactive Plot",tabName = "ggvis",icon = icon("bar-chart")),
                        ##Tab Item Thirteen
                        menuItem("Lattice Plot",tabName = "lattice",icon = icon("bar-chart")),
                        ##Tab Item Thirteen
                        menuItem("3D Plot",tabName = "plot3d",icon = icon("gears")),
                        #
                        menuItem("3D Spinning Plot",tabName = "plotrgdal3d",icon = icon("gears"))
                        
                      )),
                    ## Beginning of Dashoboard Bodys #########
                    dashboardBody(
                      tabItems(
                        ##File Tab ##############
                        tabItem(tabName = "file",
                                fileInput("file", label = h3("File input: CSV/Text",multiple = FALSE,accept = NULL,width=NULL),
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.xlsx')),
                                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                                radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                                # radioButtons(inputId = 'fileext',label = 'File Extension',choices = c('read.csv','read.xlsx')),
                                checkboxInput(inputId = "factor",label = "Strings as Factors",value = FALSE),
                                checkboxInput("header", "Header", TRUE),
                                downloadButton('downloadData', 'Download')),
                        ## Plot Tab ###############
                        tabItem(
                          tabName = "plot1",solidHeader = TRUE,
                          fluidRow(box(title = "Plot Controls",
                                       selectInput(inputId = "download1",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                       selectInput("xcol", "X Variable",choices = names(df)),
                                       selectInput("ycol", "Y Variable", choices = names(df)),
                                       sliderInput("point","Point Type",min=0, max=25,value = 19),
                                       numericInput("size","Point size",3,min=0.5,max = 10),
                                       selectInput(inputId = "line",label = "Line Type",choices = list("Line"="l","Points"="p","Stairs"="s"),selected = "p"),
                                       selectInput(inputId = 'color',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),collapsible = TRUE,collapsed = TRUE)),
                          # downloadButton('download', 'Download Plot')),
                          # downloadButton('download', 'Download Plot'),
                          # box(title = "Download",radioButtons(inputId = "download1",label = "Choose Format",choices = list("png","pdf")),
                          # downloadButton('download', 'Download Plot'),collapsible = TRUE,collapsed = TRUE),
                          box(title = "Data Plot",
                              downloadButton('download', 'Download Plot'),
                              br(),
                              plotOutput("plot1"),width = "auto",height = "auto",collapsible = TRUE,collapsed = TRUE)
                        ),
                        ##DT Table Tab ###############
                        tabItem(tabName = "table",         
                                fluidRow(
                                  tableOutput("table"))), 
                        tabItem(tabName = "tabletwo",DT::dataTableOutput('tbl')),
                        ##Summary Tab ##########
                        tabItem(tabName = "summary",
                                fluidRow(
                                  box(title = "Data Summary",solidHeader = TRUE,background = "light-blue",
                                      verbatimTextOutput("summary"),width = "auto",height = "auto")),
                                # radioButtons(inputId = "download2",label = "Choose Format",choices = list("txt","csv")),
                                downloadButton('downloadtwo', 'Download Summary')),
                        ##Correlation Tab ################
                        tabItem(tabName = "correlation",
                                box(
                                  fluidRow(box(title = "Correlation: Select Variables Below",
                                               selectInput("xcol7", "X Variable",choices = names(df)),
                                               selectInput("ycol7", "Y Variable", choices = names(df)),
                                               selectInput("corrtype","Correlation Method",choices = list("pearson","spearman","kendall"),selected = "pearson")
                                  )),
                                  # actionButton(inputId = "gocorrelaton",label = "Run Correlation"),
                                  box(title = paste("Correlation:"),
                                      verbatimTextOutput("correlation")),width = "auto",height = "auto"),
                                downloadButton('downloadcorrelation', 'Download Correlation')), ##Tab Item Six
                        tabItem(tabName = "correlationm",
                                actionButton("gomx", "Create Matrix"),
                                box(title = "Correlation Matrix",solidHeader = TRUE,background = "light-blue",
                                    verbatimTextOutput("correlationm"),width = "auto",height = "auto"),
                                downloadButton('downloadcorrmatrix', 'Download Correlation Matrix')),
                        ##Chi-Sqaure TAb ###########
                        tabItem(tabName = "chisquare",
                                selectInput("chisq1", "X Variable",choices = names(df)),
                                selectInput("chisq2", "Y Variable", choices = names(df)),
                                # numericInput("phix","Chi Square Value",value = 200),
                                numericInput("observations","Number of Observations",value = 100),
                                box(downloadButton('downloadchisquare','Download Chi Square')),
                                box(title = "Chi-Square Test",solidHeader = TRUE,background = "light-blue",
                                    actionButton(inputId = "chisquarerun","Run Chi Square"),
                                    br(),
                                    br(),
                                    verbatimTextOutput("chisquare"),width = "auto",height = "auto"),
                                box(title = "Cramer's V",solidHeader = TRUE,background = "light-blue",
                                    actionButton(inputId = "cramersv","Run Cramer's V"),
                                    br(),
                                    br(),
                                    verbatimTextOutput("cramersvtest"),width = "auto",height = "auto"),
                                br(),
                                br(),
                                box(title = "Cohen's D",solidHeader = TRUE,background = "light-blue",
                                    actionButton(inputId = "cohensd","Run Cohen's D"),
                                    br(),
                                    br(),
                                    verbatimTextOutput("cohensdtest"),width = "auto",height = "auto"),
                                box(title = "Phi",solidHeader = TRUE,background = "light-blue",footer = "Use for 2 X 2 Matrix",
                                    actionButton(inputId = "phitest","Run Phi"),
                                    br(),
                                    br(),
                                    verbatimTextOutput("phitest"),width = "auto",height = "auto"),
                                box(title = "Chi-Square Critical Value Table",solidHeader = TRUE,background = "light-blue",
                                    img(src='chisquare.png',height="auto",width="auto"),width = "300",collapsible = TRUE,collapsed = TRUE),
                                box(title = "Which Test to Use When:",solidHeader = TRUE,background = "light-blue",
                                    img(src='measuresofassociation.png',height="300",width="auto"),width = "300",collapsible = TRUE,collapsed = TRUE)
                                
                        ),
                        ##Correlogram Tab ######################
                        tabItem(tabName = "correlogram",
                                actionButton("gocorrelogram", "Create Correlogram"),
                                selectInput(inputId = "panel",label = "Correlogram Type",choices = list("Bar"="panel.bar","Conf"="panel.conf","panel.corr","Density"="panel.density","Ellipse"="panel.ellipse","MinMax"="panel.minmax","Pie"="panel.pie","Points"="panel.pts","Shade"="panel.shade","Text"="panel.text"),selected = "panel.shade"),
                                selectInput(inputId = "downloadc1",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                
                                box(title = "Correlogram",solidHeader = TRUE,
                                    downloadButton('downloadcorrgram', 'Download Correlogram'),
                                    plotOutput("corr"),width = "auto",height = "auto",collapsible = TRUE,collapsed = FALSE)),
                        # downloadButton('downloadcorrgram', 'ownload Correlogram')),
                        ##Linear Regression Tab ##################
                        tabItem(tabName = "linearregression",
                                selectInput("xcol1", "X Variable",choices = names(df)),
                                selectInput("ycol2", "Y Variable", choices = names(df)),
                                box(title = "Simple Linear Regression Output:",solidHeader = TRUE,background = "light-blue",
                                    verbatimTextOutput("summarylm"),width = 300)),
                        tabItem(tabName = "linearregressionplot",
                                selectInput("xcol14", "X Variable",choices = names(df)),
                                selectInput("ycol14", "Y Variable", choices = names(df)),
                                selectInput(inputId = 'colorl',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey")),
                                box(title = "Linear Regression Plot:",
                                    plotOutput("linearplot1"),width = "auto",height = "auto",collapsed = TRUE,collapsible = TRUE)),
                        ##Multiple Linear Regression Tab ###########
                        tabItem(tabName = "linearregressionm",
                                selectInput("xcol3", "Predictor One",choices = names(df)),
                                selectInput("xcol4", "Predictor Two",choices = names(df)),
                                selectInput("xcol5", "Predictor Three",choices = names(df)),
                                selectInput("xcol6", "Predictor Four",choices = names(df)),
                                selectInput("ycol3", "Dependent", choices = names(df)),
                                box(title = "Multiple Linear Regression Output:",solidHeader = TRUE,
                                    verbatimTextOutput("summarylmmulti"),width = 300)),
                        ##Histogram Tab #########################
                        tabItem(tabName = "histogram",
                                fluidPage(
                                  box(solidHeader = TRUE,background = "light-blue",
                                      selectInput("xcol8", "Histogram Variable",choices = names(df)),
                                      selectInput(inputId = 'colortwo',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                  selected = "LightBlue"),
                                      numericInput("bins","Number of Bins",10,min=1,max = 50),
                                      checkboxInput(inputId = "labelh",label = "Bar Labels",value = FALSE),
                                      checkboxInput(inputId = "histf",label = "Frequencies",value = TRUE),
                                      selectInput(inputId = "download3",label = "Choose Format",choices = list("png","pdf","bmp","jpeg"))),
                                  # downloadButton('downloadthree', 'Download Histogram')), 
                                  box(title = "Histogram",
                                      downloadButton('downloadthree', 'Download Histogram'),
                                      solidHeader = TRUE,background = "light-blue",
                                      plotOutput("hist"),collapsible = TRUE)
                                )),
                        ##GGVIS Tab #################
                        tabItem(tabName = "ggvis",
                                fluidPage(
                                  selectInput('x', 'x:' ,'x'),
                                  selectInput('y', 'y:', 'y'),
                                  sliderInput("size1","Point size",100,min=100,max = 400),
                                  selectInput(inputId = 'color1',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                              selected = "lightblue"),
                                  uiOutput("plot_ui"),
                                  box(
                                    ggvisOutput("plot"),width = 400))),
                        ##Random Forest Tab ####################
                        tabItem(tabName = "randomforest",
                                fluidPage(
                                  actionButton("go", "Plot"),
                                  selectInput("ycolf1", "Random Forest:",choices = names(df)),
                                  selectInput("xcolf2", "Random Forest Predictor 1:",choices = names(df)),
                                  selectInput("xcolf3", "Random Forest Predictor 2:",choices = names(df)),
                                  selectInput("xcolf4", "Random Forest Predictor 3:",choices = names(df)),
                                  box(plotOutput("randomforest"))
                                )),
                        ##Cross Tabulation Tab###################
                        tabItem(tabName = "crosstab",
                                fluidPage(
                                  selectInput(inputId = "tab1",label = "Columns",choices = names(df)),
                                  selectInput(inputId = "tab2",label = "Rows",choices = names(df)),
                                  box(title = "Cross Tabulation",verbatimTextOutput("crosstab"),
                                      downloadButton('downloadcrosstab', 'Download Cross Tabulation')),
                                  box(title = "Column Percentages:",verbatimTextOutput("crosstabcolperc"),
                                      collapsible = TRUE,collapsed = TRUE),
                                  box(title = "Row Percentages:",verbatimTextOutput("crosstabrowperc"),
                                      collapsible = TRUE,collapsed = TRUE)
                                )),
                        ##Subset Tab ###################
                        tabItem(tabName = "subset",
                                numericInput(inputId = "subset1",label =  "Columns",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "subset2",label =  "Rows",min = 1,max = 50000,step = 1,value = 10),
                                numericInput(inputId = "subset3",label =  " From Row:",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "subset4",label =  "To Row:",min = 1,max = 50000,step = 1,value = 10),
                                numericInput(inputId = "subset5",label =  "From Column:",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "subset6",label =  "To Column:",min = 1,max = 50000,step = 1,value = 10),
                                
                                
                                box(title = "Data Single Subset:",solidHeader = TRUE,
                                    verbatimTextOutput("subset"),width = 300),
                                box(title = "Data Multiple Subset:",solidHeader = TRUE,
                                    actionButton("subset", "Create Subset"),
                                    downloadButton('downloadsubset', 'Download Subset'),
                                    verbatimTextOutput("subsettwo"),width = 300)),
                        
                        
                        ##Lattice Plot Tab #################
                        tabItem(
                          tabName = "lattice",solidHeader = TRUE,
                          fluidRow(box(title = "Lattice Plot Controls",
                          selectInput(inputId = "downloadlattice",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                          selectInput("xlattice", "X Variable",choices = names(df)),
                          selectInput("ylattice", "Y Variable", choices = names(df)),
                          selectInput("zlattice", "Grouping Variable", choices = names(df)),
                          textInput(inputId = "text1",label = "Label One",value = ""),
                          textInput(inputId = "text2",label = "Label Two",value = ""),
                          sliderInput("point1","Point Type",min=0, max=25,value = 1),
                          sliderInput("point2","Point Type",min=0, max=25,value = 20),
                          selectInput(inputId = 'colorlattice1',label =  'Grouping Variable Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                      selected = "Black"),collapsible = TRUE,collapsed = TRUE),
                          box(title = "Lattice Plot",
                              # downloadButton('downloadlattice2', 'Download Lattice Plot'),
                              plotOutput("plotlattice"),width = "auto",height = "auto",
                              collapsible = TRUE,collapsed = TRUE)
                        )),
                        
                        
                        ##Plot 3D Tab Non-Moving ###############
                        tabItem(
                          tabName = "plot3d",solidHeader = TRUE,
                          fluidRow(box(title = "3D Plot Controls",
                                       selectInput("x3d", "X Variable",choices = names(df)),
                                       selectInput("y3d", "Y Variable", choices = names(df)),
                                       selectInput("z3d", "Z Variable", choices = names(df)),
                                       textInput(inputId = "title",label = "Title",value = "3D Plot"),
                                       sliderInput("point3d","Point Type",min=0, max=25,value = 19),
                                       numericInput("numeric","Angle",min=0,max = 360,value = 120),
                                       numericInput("size3d","Point size",min=0.5,max = 10,value = 3),
                                       numericInput("cexaxis","Axis Label Size",min=0.5, max=25,value = 1),
                                       numericInput("cexlab","Name Label Size",min=0.5,max = 10,value = 1.5),
                                       selectInput(inputId = 'colaxis',label =  'Axis Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),
                                       selectInput(inputId = 'colgrid',label =  'Grid Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),
                                       selectInput(inputId = 'color3d',label =  'Point Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),
                                       checkboxInput(inputId = "grid",label = "Grid Lines",value = TRUE),
                                       checkboxInput(inputId = "box",label = "Box",value = TRUE),
                                       checkboxInput(inputId = "highlight",label = "Highlight 3D",value = FALSE),collapsible = TRUE,collapsed = TRUE)
                                   
                          ),
                          
                          box(title = "3D Plot",
                              br(),
                              plotOutput("plot3d"),width = "auto",height = "auto",collapsible = TRUE,collapsed = TRUE)),
                        
                        ##Plot 3D Moving ##########################
                        tabItem(
                          tabName = "plotrgdal3d",solidHeader = TRUE,
                          fluidRow(box(title = "3D Plot Controls",
                                       selectInput("x3dr", "X Variable",choices = names(df)),
                                       selectInput("y3dr", "Y Variable", choices = names(df)),
                                       selectInput("z3dr", "Z Variable", choices = names(df)),
                                       textInput(inputId = "title",label = "Title",value = "3D Plot"),
                                       selectInput(inputId = "type3d",label = "Line Type",choices = c("Points"="p","Bars"="h"),selected = "p"),
                                       # sliderInput("point3dm","Point Type",min=0, max=25,value = 19),
                                       numericInput("sizem","Point size",min=0.5,max = 10,value = 3),
                                       numericInput("lwd","Line Width",min=2, max=25,value = 4),
                                       # numericInput("cexlab","Name Label Size",min=0.5,max = 10,value = 1.5),
                                       selectInput(inputId = 'color3dm',label =  'Point Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),
                                       checkboxInput(inputId = "aspect",label = "Increase Aspect Ratio",value = TRUE)
                                       ,collapsible = TRUE,collapsed = TRUE),
                          box(title = "3D Plot Moving",
                              # actionButton("runplot","Render 3D Plot"),
                              br(),
                              br(),
                              plotOutput("plotrgdl3d"),width = "auto",height = "auto",collapsible = TRUE,collapsed = TRUE)
                          ))
                        
                        
                        
                        
                        
                        
                        
                      )))


