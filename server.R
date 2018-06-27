##Server Code ####

server <- function(input, output,session) {
  
  
  shinyApp(ui, server, enableBookmarking = "url")
  options(shiny.maxRequestSize = 25*1024^2)
  ## First File Reader ##############
  data <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote,stringsAsFactors = input$factor)
    
    
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    
    
    
    
    return(df)
    
  })
  
  ##Second Reactive for Simple Linear Regression ####
  dataset <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ##Render Table Data Reactive ############
  
  
  datatable <- reactive({ 
    
    withProgress(message = 'Creating Table',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    return(df)
    
  })
  
  ##Data Subset Reactive ####  
  
  datasubset <- reactive({ 
    
    withProgress(message = 'Creating Subset of Data',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    updateNumericInput(session, inputId = 'subset1', label = 'Columns',value = 1)
    updateNumericInput(session, inputId = 'subset2', label = 'Rows',value = 1)
    updateNumericInput(session, inputId = 'subset3', label = 'From Row:',value = 1)
    updateNumericInput(session, inputId = 'subset4', label = 'To Row:',value = 10)
    updateNumericInput(session, inputId = 'subset5', label = 'From Column:',value = 1)
    updateNumericInput(session, inputId = 'subset6', label = 'To Column:',value = 10)
    
    
    return(df)
    
  })
  output$tbl <- DT::renderDataTable(
    datatable(), options = list(lengthChange = TRUE,autoWidth = TRUE,scrollX = TRUE),filter='top',
    class = "cell-border stripe")
  
  
  
  ##Reactive for Linear Regression Plot #########
  datasetlplot <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol14', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol14', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ## 3DPlot Non-Moving Reactive ##################
  dataplot3d <- reactive({
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'x3d', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'y3d', label = 'Y Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'z3d', label = 'Z Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ## 3DPlot Moving Reactive ###############
  datargl3d <- reactive({
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'x3dr', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'y3dr', label = 'Y Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'z3dr', label = 'Z Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ## Third Reactive for Multiple Linear Regression ####
  datamultiple <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol3', label = 'Predictor One',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol4', label = 'Predictor Two',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol5', label = 'Predictor Three',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol6', label = 'Predictor Four',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol3', label = 'Dependent',
                      choices = names(df))
    
    
    return(df)
    
  })
  ## Fourth Reactive for Correlation
  
  datasetcor <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol7', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol7', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ## Chi-Square Reactive
  datasetchisquare <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'chisq1', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'chisq2', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  datalattice <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xlattice', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ylattice', label = 'Y Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'zlattice', label = 'Grouping Variable',
                      choices = names(df),"")
    
    
    return(df)
    
  })
  
  ##Histogram Reactive Code 
  datasethist <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol8', label = 'Histogram Variable',
                      choices = names(df))
    
    
    
    return(df)
    
  })
  
  
  ##Random Forest File Reader ##############
  datarandom <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    
    updateSelectInput(session, inputId = 'ycolf1', label = 'Random Forest',choices = names(df))
    updateSelectInput(session, inputId = 'xcolf2', label = 'Random Forest Predictor 1',choices = names(df))
    updateSelectInput(session, inputId = 'xcolf3', label = 'Random Forest Predictor 2',choices = names(df))
    updateSelectInput(session, inputId = 'xcolf4', label = 'Random Forest Predictor 3',choices = names(df))
    
    
    
    
    # selectInput("ycolf1", "Random Forest:",choices = names(df)),
    # selectInput("xcolf2", "Random Forest Predictor 1:",choices = names(df)),
    # selectInput("xcolf3", "Random Forest Predictor 2:",choices = names(df)),
    # selectInput("xcolf4", "Random Forest Predictor 3:",choices = names(df)),
    
    
    
    return(df)
    
  })
  
  ## Lattice Reactive Reactive
  datalattice <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xlattice', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ylattice', label = 'Y Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'zlattice', label = 'Grouping Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  #Lattice Plot Code ########################
  output$plotlattice <- renderPlot({
    withProgress(message = 'Creating Lattice Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    group.labels <- c(input$text1,input$text2)
    group.symbols <- c(input$point1,input$point2)
    group.symbols.size <- c(2,2.75)
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    x<-datalattice()[,input$xlattice]
    y<-datalattice()[,input$ylattice]
    z<-datalattice()[,input$zlattice]
    
    bwplot(y ~ x, data = df, groups = z, 
           xlab = toupper(input$xlattice),main= toupper(input$zlattice),
           ylab = toupper(input$ylattice),
           panel = function(x, y, groups, subscripts, ...) 
           {panel.grid(h = (length(levels(y)) - 1), v = -1)
             panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
                             cex = group.symbols.size, pch = group.symbols, col = input$colorlattice1)
           },
           key = list(space = "top", 
                      text = list(group.labels,col = "black"),
                      points = list(pch = group.symbols, cex = group.symbols.size, 
                                    col = input$colorlattice1)))
    
    
  })
  
  #3D Plot Code ########################
  output$plot3d <- renderPlot({
    withProgress(message = 'Rendering 3D Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    x<-dataplot3d()[,input$x3d]
    y<-dataplot3d()[,input$y3d]
    z<-dataplot3d()[,input$z3d]
    
    scatterplot3d(x, y, z, highlight.3d = input$highlight, angle = input$numeric,size=input$size3d,color = input$color3d,
                  col.axis = input$colaxis, col.grid = input$colgrid,cex.axis = input$cexaxis,cex.lab = input$cexlab,
                  main = input$title, pch = input$point3d,xlab = toupper(input$x3d),ylab = toupper(input$y3d),zlab = toupper(input$z3d),
                  grid = input$grid,box = input$box)
    
    
    
  })
  
  #3D Moving Plot Code ########################
  output$plotrgdl3d <- renderPlot({
    withProgress(message = 'Rendering 3D Spinning Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    x<-datargl3d()[,input$x3dr]
    y<-datargl3d()[,input$y3dr]
    z<-datargl3d()[,input$z3dr]
    
    plot3d(x, y, z )
    
    
    
  })
  
  ## Reactive Summary Data #######
  summarydata <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    summary(df)
    
    
    return(df)
    
  })
  
  
  ## Plot output code ##########
  output$plot1 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 0, 1))
    withProgress(message = 'Creating Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    
    
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x,col=input$color,pch = input$point,type = input$line,cex=input$size)
  })
  
  ## Linear Model Plot ###########
  output$linearplot1 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Linear Regression Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    par(mar = c(5.1, 4.1, 0, 1))
    
    linearmodelab<- read.csv(inFile$datapath, header = input$header)
    
    x<-datasetlplot()[,input$xcol14]
    y<-datasetlplot()[,input$ycol14]
    lmone<-lm(as.formula(paste0(input$ycol14,"~",input$xcol14)),data = linearmodelab)
    
    x <- data()[, c(input$xcol14, input$ycol14)]
    plot(x,col=input$color,pch = input$point,type = input$line,cex=input$size)
    abline(lmone,col=input$colorl)
  })
  
  ##Render Table Data Code ############
  output$table <- renderTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Table',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    # datasetInput()
    read.csv(inFile$datapath, header = input$header)
    
  })
  
  #Lattice Plot Code ########################
  latticeplot <- renderPlot({
    
    withProgress(message = 'Creating Lattice Plot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    group.labels <- c(input$text1,input$text2)
    group.symbols <- c(input$point1,input$point2)
    group.symbols.size <- c(2,2.75)
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    x<-datalattice()[,input$xlattice]
    y<-datalattice()[,input$ylattice]
    z<-datalattice()[,input$zlattice]
    
    bwplot(y ~ x, data = df, groups = z, 
           xlab = x,main="Lattice Plot",
           ylab = y,
           panel = function(x, y, groups, subscripts, ...) 
           {panel.grid(h = (length(levels(y)) - 1), v = -1)
             panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
                             cex = group.symbols.size, pch = group.symbols, col = input$colorlattice1)
           },
           key = list(space = "top", 
                      text = list(group.labels,col = "black"),
                      points = list(pch = group.symbols, cex = group.symbols.size, 
                                    col = input$colorlattice1)))
    
    
  })
  
  ##File Summary Code
  output$summary <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Summary',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    yowsa<- read.csv(inFile$datapath, header = input$header)
    summary(yowsa)
  })
  
  
  
  ##Data Subset Output Code ####
  output$subset <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Summary',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    datasubset()[c(input$subset1),c(input$subset2)]
    
    # return(datasubset)
  })
  
  ##Data Subset Output Code ####
  observeEvent(input$subset,{output$subsettwo <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Summary',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    datasubset()[c(input$subset3:input$subset4),c(input$subset5:input$subset6)]
    
    # return(datasubset)
  })
  })
  
  ##Observe function used for Cross Tabulation ########
  observe({
    updateSelectInput(session,inputId = "tab1",label =  "Columns", choices = sort(as.character(colnames(data()))))
    updateSelectInput(session, inputId = "tab2",label = "Rows", choices = sort(as.character(colnames(data()))))
    
  })
  
  # observe({
  #   updateSelectInput(session, inputId = "tab2",label = "Rows", choices = sort(as.character(colnames(data()))))
  # })
  
  
  ## Cross Tabulation Output Code #############
  output$crosstab <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    with(data(), table(get(input$tab2),get(input$tab1)))
    # xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data())
  })
  
  ## Cross Tab Column Percentage Output Code #########
  output$crosstabcolperc <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    colPerc(with(data(), table(get(input$tab2),get(input$tab1))))
    # colPerc(xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data()))
  })
  
  ## Cross Tab Row Percentage Output Code ##########
  output$crosstabrowperc <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    rowPerc(with(data(), table(get(input$tab2),get(input$tab1))))
  })
  
  
  ## Additional Reactive Datasets   
  
  ## Summary Reactive Datasets ##########
  summarytwo <- reactive({
    
    inFile <- input$file
    yowsa<- read.csv(inFile$datapath, header = input$header)
    as.data.frame.matrix(summary(yowsa))
  })
  
  ## Correlation Matrix Reactive Code ########
  corrmatrixd <- reactive({
    
    inFile <- input$file
    yowsa<- read.csv(inFile$datapath, header = input$header)
    as.data.frame.matrix(cor((yowsa)))
  })
  
  ## Correlation Variable Reactive ########
  
  corrvariable <- reactive({
    
    inFile <- input$file
    yowsa<- read.csv(inFile$datapath, header = input$header)
    correlation<- yowsa[,c(input$xcol7,input$ycol7)]
    correlate(correlation,corr.method = input$corrtype)
  })
  
  ### Correlogram Reactive Output Code ########
  corrfiledown <- reactive({
    # par(mar = c(20, 20, 0, 1))
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    corrfile<- read.csv(inFile$datapath, header = input$header)
    corrgram(corrfile,panel = input$panel,order = TRUE)
    
  })
  
  ## Cross Tabulation Reactive Code #######
  
  crossout <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    # ## Needed to hold table until data is updated in it
    # validate(need(input$tab2,''),
    #          need(input$tab1,''))
    xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data())
    # with(data(), table(get(input$tab2),get(input$tab1)))
    
  })
  
  ## Chisquare Print Code ############
  
  chisquaredown<- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    chisquaretest<-datasetchisquare()[,c(input$chisq1,input$chisq2)]
    chisq.test(chisquaretest)
  })
  
  ### Data subset down Code #######
  datasubsetdown <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    incProgress(message = 'Subsetting Data',
                value = 0, {
                  for (i in 1:3) {
                    incProgress(1/2)
                    Sys.sleep(0.25)
                  }
                },env = parent.frame())
    # ## Needed to hold table until data is updated in it
    # validate(need(input$tab2,''),
    #          need(input$tab1,''))
    datasubset()
    # with(data(), table(get(input$tab2),get(input$tab1)))
    
  })
  
  ## Download Buttons ################################################################################################  
  
  ##Download Button Code for downloading csv file
  
  ## Plot Download Code ###########
  
  output$download <- downloadHandler(
    filename = function() { paste("Plot ",input$xcol," by ",input$ycol,input$download1,sep = ".") },
    content = function(file) {
      if(input$download1=="png")
        png(file)
      else if (input$download1=="jpeg")
        jpeg(file)
      else if (input$download1=="bmp")
        bmp(file)
      else if (input$download1=="pdf")
        pdf(file)
      plot(data()[, c(input$xcol, input$ycol)],col=input$color,pch = input$type,type = input$line,cex=input$size,main = paste(input$xcol," by ",input$ycol))
      dev.off()
    })
  
  ## Correlogram Download Code ######
  
  output$downloadcorrgram <- downloadHandler(
    filename = function() { paste("Correlogram ",input$file,input$downloadc1,sep = ".") },
    content = function(file) {
      if(input$downloadc1=="png")
        png(file)
      else if (input$downloadc1=="jpeg")
        jpeg(file)
      else if (input$downloadc1=="bmp")
        bmp(file)
      else if (input$downloadc1=="pdf")
        pdf(file)
      # plot(data()[, c(input$xcol, input$ycol)],col=input$color,pch = input$type,type = input$line,cex=input$size,main = paste(input$xcol," by ",input$ycol))
      corrgram(corrfiledown(),panel = input$panel,order = TRUE,main="Correlogram")
      dev.off()
    })
  
  ## Histogram Dowload Code ##############
  
  output$downloadthree <- downloadHandler(
    filename = function() { paste("Histogram",input$xcol8,input$download3,sep = ".") },
    content = function(file) {
      if(input$download3=="png")
        png(file)
      else if (input$download3=="jpeg")
        jpeg(file)
      else if (input$download3=="bmp")
        bmp(file)
      else if (input$download3=="pdf")
        pdf(file)
      hist(as.numeric(datasethist()[,input$xcol8]), col=input$colortwo,breaks = input$bins,xlab = input$xcol,main = input$xcol,border = input$bordercolor,
           freq = TRUE)
      dev.off()
    })
  
  ## Original File CSV Download Code ##############
  
  output$down <- downloadHandler(
    filename = function() { paste(input$file, sep='') },
    content = function(file) {
      write.csv(data(), file)
      
    })
  
  ## Summary Download Code ##########
  
  output$downloadtwo <- downloadHandler(
    filename = function() { paste("Summary",input$file, sep='.') },
    content = function(file) {
      write.csv(summarytwo(), file)
    })
  
  ## Crosstab Download Code ############
  output$downloadcrosstab <- downloadHandler(
    filename = function() { paste("Crosstab",input$chisq1,"BY",input$chisq2,input$file, sep='.') },
    content = function(file) {
      # x <- (table((input$tab2),(input$tab1),data()))
      write.csv(crossout(),file,col.names = TRUE,row.names = TRUE)
      
    })
  
  ## Correlation Matrix Download Code ############
  output$downloadcorrmatrix <- downloadHandler(
    withProgress(message = 'Downloading Correlation Matrix',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1)),
    filename = function() { paste("CorrelationMatrix",input$file, sep='.') },
    content = function(file) {
      # x <- (table((input$tab2),(input$tab1),data()))
      write.csv(corrmatrixd(),file,col.names = TRUE,row.names = TRUE)
      
    })
  
  ## Correlation Download Code ############
  output$downloadcorrelation <- downloadHandler(
    filename = function() { paste("Correlation",input$xcol7,"by",input$ycol7,input$file, sep='.') },
    content = function(file) {
      # x <- (table((input$tab2),(input$tab1),data()))
      write.csv(corrvariable(),file,col.names = TRUE,row.names = TRUE)
      
    })
  
  ## Chi-Square Download Code ############
  output$downloadchisquare <- downloadHandler(
    filename = function() { paste("Chi_Square",input$tab1,input$tab2,input$file, sep='.') },
    content = function(file) {
      # x <- (table((input$tab2),(input$tab1),data()))
      write.csv(chisquaredown(),file,col.names = TRUE,row.names = TRUE)
      
    })
  
  ## Subset Download Code #########
  output$downloadsubset <- downloadHandler(
    filename = function() { paste(input$file, sep='') },
    content = function(file) {
      x <-  datasubset()[c(input$subset3:input$subset4),c(input$subset5:input$subset6)]
      
      write.csv(x, file)
      
    })
  
  ## Download Buttons #####  
  
  
  ## Statistics Code ######################################################
  ##Correlation Code ######################
  
  # observeEvent(input$gocorrelation,{

  output$correlation <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    correlation<- datasetcor()[,c(input$xcol7,input$ycol7)]
    correlate(correlation,corr.method = input$corrtype)
  })
  
  ##ChiSquare Code Output ###########
  
  output$chisquare<- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    chisquaretest<-datasetchisquare()[,c(input$chisq1,input$chisq2)]
    chisq.test(chisquaretest)
  })
  
  ## Cramers V Output Code #########
  observeEvent(input$cramersv,{output$cramersvtest<- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    chisquaretest<-datasetchisquare()[,c(input$chisq1,input$chisq2)]
    cramersV(chisquaretest)
    
  })})
  
  ## Phi Coefficient Output Code ###########
  output$phitest<- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    chisquaretest<-datasetchisquare()[,c(input$chisq1,input$chisq2)]
    
    phi(chisquaretest)
    
  })
  
  ## Cohens D Output Code 
  observeEvent(input$cohensd,{output$cohensdtest<- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Calculating Cohens D',
                 value = 0, {
                   for (i in 1:2) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    
    x <- datasetchisquare()[,c(input$chisq1)]
    y <- datasetchisquare()[,c(input$chisq2)]
    
    cohen.d(x,y)
    
    
  })})
  ##Correlation Code
  observeEvent(input$gomx,{output$correlationm <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    correlation<- data()
    cor(correlation)
  })
  })
  
  ##Simple Linear Regression Code
  output$summarylm <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Running Simple Linear Regression',
                 value = 0, {
                   for (i in 1:2) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    linearmodel<- read.csv(inFile$datapath, header = input$header)
    
    x<-dataset()[,input$xcol1]
    y<-dataset()[,input$ycol2]
    lmone<-lm(as.formula(paste0(input$ycol2,"~",input$xcol1)),data = linearmodel)
    print(paste0("Formula:",input$ycol2," ~ ",input$xcol1," data: "))
    # # print(input$xcol1)
    # print(paste0("Y =",input$ycol2))
    # # print(input$ycol2)
    summary(lmone)
    # summary(lm(as.formula(paste0(input$ycol2,"~",input$xcol1)),data=linearmodel))
    # summary(lm(as.formula(paste0("~",input$ycol2,input$xcol1)),data=linearmodel))
    
  })
  
  ### Correlogram Code
  observeEvent(input$gocorrelogram,{output$corr <- renderPlot({
    # par(mar = c(20, 20, 0, 1))
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Correlogram',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    corrfile<- read.csv(inFile$datapath, header = input$header)
    corrgram(corrfile,panel = input$panel,order = TRUE)
    
  })
  })
  ##Multiple Linear Regression Code
  output$summarylmmulti <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Running Multiple Linear Regression',
                 value = 0, {
                   for (i in 1:2) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    linearmodelmulti<- read.csv(inFile$datapath, header = input$header)
    
    p1<-datamultiple()[,input$xcol3]
    p2<-datamultiple()[,input$xcol4]
    p3<-datamultiple()[,input$xcol5]
    p4<-datamultiple()[,input$xcol6]
    y<-datamultiple()[,input$ycol3]
    print(paste0("Formula:",input$ycol3," ~ ",input$xcol3," + ",input$xcol4," + ",input$xcol5," + ",input$xcol6," data: "))
    # lmonemulti<-lm(y~p1+p2+p3+p4,data = linearmodelmulti)
    # print("Predictor One =")
    # print(input$xcol3)
    # print("Predictor Two =")
    # print(input$xcol4)
    # print("Predictor Three =")
    # print(input$xcol5)
    # print("Predictor Four =")
    # print(input$xcol6)
    # print("Dependent =")
    # print(input$ycol3)
    # summary(lm(y~p1+p2+p3+p4,data = linearmodelmulti))
    summary(lm(as.formula(paste0(input$ycol3,"~",input$xcol3,"+",input$xcol4,"+",input$xcol5,"+",input$xcol6)),data=linearmodelmulti))
    
  })
  
  
  ##Histogram Code 
  output$hist <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating Histogram',
                 value = 0, {
                   for (i in 1:5) {
                     incProgress(1/5)
                     Sys.sleep(0.25)
                   }
                 })
    x<-as.numeric(datasethist()[,input$xcol8])
    hist(x, col=input$colortwo,breaks = input$bins,xlab = input$xcol8,main = input$xcol8,border = input$bordercolor,
         freq = input$histf,labels = input$labelh,ann=TRUE)
    
    
  })
  
  #load the data when the user inputs a file
  theData <- reactive({
    infile <- input$file        
    if(is.null(infile))
      return(NULL)        
    d <- read.csv(infile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    d        
  })
  
  
  
  # dynamic variable names
  observe({
    data<-theData()
    updateSelectInput(session, 'x', label = 'x:',choices = names(data))
    updateSelectInput(session, 'y', label = 'y:',choices = names(data))
    
  }) # end observe
  
  #gets the y variable name, will be used to change the plot legends
  yVarName<-reactive({
    input$y
  })
  
  #gets the x variable name, will be used to change the plot legends
  xVarName<-reactive({
    input$x
  })
  
  #make the filteredData frame
  
  filteredData<-reactive({
    data<-isolate(theData())
    #if there is no input, make a dummy dataframe
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x=0,y=0)
      }
    }else{
      data<-theData()[,c(input$x,input$y)]
      names(data)<-c("x","y")
    }
    data
  })
  
  input_size <- reactive(input$size1)
  input_color <- reactive(input$color1)
  
  #plot the ggvis plot in a reactive block so that it changes with filteredData
  vis<-reactive({
    plotData<-filteredData()
    # z <- paste0(filteredData()[,input$y])
    plotData %>%
      ggvis(~x, ~y,fill=~y,size := input_size,fill := input_color) %>%
      layer_points() %>%
      add_axis("y", title = yVarName()) %>%
      add_axis("x", title = xVarName()) 
    # add_tooltip(function(df) data()[,input$y[]])
  })
  vis%>%bind_shiny("plot", "plot_ui")
  
  
  
  # # A reactive expression with the ggvis plot
  # vis <- reactive({
  # 
  #   inFile <- input$file
  #   # if (is.null(inFile))
  #   #   return("Please Upload File")
  #   # datanew<- read.csv(inFile$datapath, header = input$header)
  # 
  #   xvar <- prop("x", as.symbol(input$xcol12))
  #   yvar <- prop("y", as.symbol(input$ycol12))
  # 
  # 
  #   # xvar <- datavis()[,input$xcol12]
  #   # yvar <- datavis()[,input$ycol12]
  # 
  #   data %>%
  #     ggvis(x = xvar,y = yvar) %>%
  #     layer_points()
  #     # set_options(width = 500, height = 500)
  # })
  # #
  # vis %>% bind_shiny("plot2")
  # input_size <- reactive(input$size)
  
  # mtcars %>% 
  #   ggvis(~disp, ~mpg, size := input_size) %>%
  #   layer_points() %>%
  #   bind_shiny("ggvis", "ggvis_ui")
  
  
  # #3D Moving Plot Code ########################
  # output$plotrgdl3d <- renderPlot({
  #   withProgress(message = 'Rendering 3D Plot',
  #                value = 0, {
  #                  for (i in 1:3) {
  #                    incProgress(1/2)
  #                    Sys.sleep(0.25)
  #                  }
  #                },env = parent.frame(n=1))
  #   
  #   inFile <- input$file 
  #   
  #   
  #   df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
  #   
  #   x<-datargl3d()[,input$x3dr]
  #   y<-datargl3d()[,input$y3dr]
  #   z<-datargl3d()[,input$z3dr]
  #   
  #   plot3d(x, y, z,col = "blue",xlab = toupper(input$x3dr),ylab = toupper(input$y3dr),
  #          zlab = toupper(input$z3dr),type = input$type3d,aspect = input$aspect,size = input$sizem,lwd = input$lwd
  #         )
  #   # pch3d(x, y, z, pch = i, bg = "green")
  #   
  #   
  #   # help(pch3d)
  # })
  ## Random Forest Approach ############################################
  # ## Student Performance Random Forest #############################################
  # 
  # 
  # output$randomforest <- renderPlot({
  # 

  
  # ## Random Forest Output Code #########
  # observeEvent(input$go,{ output$randomforest <- reactive({
  #   incProgress(message = 'Creating Random Forest',
  #               value = 0, {
  #                 for (i in 1:3) {
  #                   incProgress(1/2)
  #                   Sys.sleep(0.25)
  #                 }
  #               },env = parent.frame())
  #   
  #   inFile <- input$file
  #   yowsa<- read.csv(inFile$datapath, header = input$header)
  #   
  #   t1 <- datarandom()[,input$ycolf1]
  #   t2 <- datarandom()[,input$xcolf2]
  #   t3 <- datarandom()[,input$xcolf3]
  #   t4 <- datarandom()[,input$xcolf4]
  #   
  #   fit <- rpart(t1 ~ ., method = "anova", data = yowsa)
  #   
  #   plot(fit, uniform=TRUE, 
  #        main="Regression Tree for Mileage ")
  #   text(fit, use.n=TRUE, all=TRUE, cex=.8)
  #   
  # })
  # })
  # 
  ## End of Shiny App Code #######################
  
}



# ?validColors

# selectInput("ycolf1", "Random Forest:",choices = names(df)),
# selectInput("xcolf2", "Random Forest Predictor 1:",choices = names(df)),
# selectInput("xcolf3", "Random Forest Predictor 2:",choices = names(df)),
# selectInput("xcolf4", "Random Forest Predictor 3:",choices = names(df)),
# box(plotOutput("randomforest"))
shinyApp(ui, server)
