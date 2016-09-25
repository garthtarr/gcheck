# 
# gcheck app
# 
# Garth Tarr garth.tarr@gmail.com
# 
# if you can see this, to run
# the app all you need to do is 
# click the Run App button in the top
# right of this window pane.
# 
# 
# 
# 
# 
# 
# Do not touch anything below this line
# 
require(readr)
require(shiny)
require(ggplot2)
require(grid)
require(tools)
require(reshape2)
require(gridExtra)
require(magrittr)
require(markdown)
plot.fn = function(data,title){
  names(data) = c("grader","raw","resid")
  data = na.omit(data)
  plot1 = ggplot(data, aes(as.factor(grader),raw)) + 
    geom_violin(aes(fill=as.factor(grader))) + 
    ylab("Original data")+ xlab("Grader ID") + 
    theme_bw(base_size = 14) + guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  plot2 = ggplot(data, aes(as.factor(grader),resid)) + 
    geom_violin(aes(fill=as.factor(grader))) + 
    ylab("Residuals") + xlab("Grader ID") +
    theme_bw(base_size = 14) + guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  grid.arrange(plot1, plot2, ncol = 2, 
               top =textGrob(title, gp=gpar(fontsize=16)))
}

plot.monthly.fn = function(data,title){
  names(data) = c("grader","raw","resid","month")
  data = na.omit(data)
  data$month = ordered(data$month, month.name)
  plot1 = ggplot(data, aes(as.factor(grader),raw)) + 
    geom_violin(aes(fill=as.factor(grader))) + 
    facet_wrap(~month, ncol=2) +
    ylab("Original data")+ xlab("Grader ID") + 
    theme_bw(base_size = 14) + guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  plot2 = ggplot(data, aes(as.factor(grader),resid)) + 
    geom_violin(aes(fill=as.factor(grader))) + 
    facet_wrap(~month, ncol=2) +
    ylab("Residuals") + xlab("Grader ID") +
    theme_bw(base_size = 14) + guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  grid.arrange(plot1, plot2, ncol = 2, 
               top =textGrob(title, gp=gpar(fontsize=16)))
}

plot.monthly.fn2 = function(data,title){
  names(data) = c("grader","raw","resid","month")
  data = na.omit(data)
  data$month = ordered(data$month, month.name)
  levels(data$month) = month.abb
  # plot1 = ggplot(data, aes(as.factor(month),raw)) + 
  #   geom_violin(aes(fill=as.factor(month))) + 
  #   facet_wrap(~grader, ncol=1) +
  #   ylab("Original data")+ xlab("Grader ID") + 
  #   theme_bw(base_size = 14) + guides(fill=FALSE) +
  #   stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  plot2 = ggplot(data, aes(as.factor(month),resid)) + 
    geom_violin(aes(fill=as.factor(month))) + 
    facet_wrap(~grader, ncol=1) +
    ylab("Residuals") + xlab("") +
    theme_bw(base_size = 14) + guides(fill=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=19, size=2)
  return(plot2)
  # grid.arrange(plot1, plot2, ncol = 2, 
  # top =textGrob(title, gp=gpar(fontsize=16)))
}

oall.plot = function(data,variable,title){
  ggplot(data, aes_string(variable)) +
    geom_density() + theme_bw(base_size = 14) + 
    xlab(title) + ylab("Frequency") #+ 
  #title(title)
}

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".pagebreak { page-break-before: always; } // page-break-after works, as well
                    "))
  ),
  
  titlePanel("gcheck: grader evaluation"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload csv file (or zipped csv file)'),
      
      h4("Subset the data"),
      uiOutput("uiselect"),
      br(),
      p("Printing works best with the Google Chrome browser."),
      br(),
      div(align = "center",
          div(img(src = "mla_logo_home.jpg",width = "150px")),
          #tags$small("MLA project V.EQT.1513"),
          #br(),
          icon("envelope"),
          HTML(paste("<a href='mailto:garth.tarr@gmail.com'>")),
          tags$small("garth.tarr@gmail.com"),
          HTML(paste("</a>"))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabpanel",
                  tabPanel("Main", 
                           textOutput("graderText"),
                           DT::dataTableOutput("graderTable"),
                           br(),
                           textOutput("graderMonthText"),
                           DT::dataTableOutput("graderMonthTable")
                           
                  ),
                  tabPanel("Raw data",
                           plotOutput("discretePlot",height = "200px"),
                           plotOutput("continuousPlot"),
                           h3("How to interpret the violin plots"),
                           p("The violin plots for continuous data (as seen above) are mirror image density plots (seen below).  The standard density plots (seen below) are like smoothed histograms.  When viewed as a violin plot, they look similar to a boxplot, but enables the viewer to see more detail, especially with respect to the lumpiness of the data, than a boxplot.  The average (sample mean) of the data set is indicated as a solid dot."),
                           plotOutput("msamarblingPlot"),
                           plotOutput("humpcoldPlot"),
                           plotOutput("ossificationcoldPlot"),
                           plotOutput("ribfatcoldPlot"),
                           plotOutput("phPlot"),
                           plotOutput("totalhscwPlot")#,
                           # plotOutput("feedtypePlot"),
                           # plotOutput("sexPlot"),
                           # plotOutput("hgpPlot")
                  ),
                  tabPanel("Model outputs",
                           h3("Predictive ability"),
                           DT::dataTableOutput("PredictiveTab"),
                           h3("MSA marbling model"),
                           verbatimTextOutput("marbOutputs"),
                           h3("Hump height model"),
                           verbatimTextOutput("humpOutputs"),
                           h3("Ossification model"),
                           verbatimTextOutput("ossOutputs"),
                           h3("Rib fat model"),
                           verbatimTextOutput("rbfOutputs"),
                           h3("pH model"),
                           verbatimTextOutput("phOutputs")
                  ),
                  tabPanel("Grader assessment",
                           h3("Average residuals"),
                           DT::dataTableOutput("GraderTab"),
                           h3("Standard deviation of residuals"),
                           DT::dataTableOutput("GraderSdTab"),
                           h3("Effect size"),
                           p("The effect size is the difference in average residual score divided by the overall residual standard deviation.  The numbers in the table below are the effect size relative to zero.  It's better to consider the difference in effect size between any two graders, for example if one grader had an effect size of 0.3 and another had an effect size of -0.4 then the effect size between the two graders would be 0.7. A small effect size is 0.2, medium (probably worth investigating further) is around 0.5 and large (quite unusual) would be 0.8 or more."),
                           DT::dataTableOutput("GraderEffectTab"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("MSA marbling model"),
                           plotOutput("marbGraderPlot"),
                           h3("Hump height model"),
                           plotOutput("humpGraderPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("Ossification model"),
                           plotOutput("ossGraderPlot"),
                           h3("Rib fat model"),
                           plotOutput("rbfGraderPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("pH model"),
                           plotOutput("phGraderPlot")
                  ),
                  tabPanel("Monthly plots",
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("MSA marbling model"),
                           plotOutput("MarblingResidPlot"),
                           plotOutput("MarblingEffectPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("Hump height model"),
                           plotOutput("HumpResidPlot"),
                           plotOutput("HumpEffectPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("Ossification model"),
                           plotOutput("OssificationResidPlot"),
                           plotOutput("OssificationEffectPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("Rib fat model"),
                           plotOutput("RibfatResidPlot"),
                           plotOutput("RibfatEffectPlot"),
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("pH model"),
                           plotOutput("pHResidPlot"),
                           plotOutput("pHEffectPlot")
                  ),
                  tabPanel("Monthly summary",value = "ms2",
                           shiny::HTML("<div class='pagebreak'> </div>"),
                           h3("Average residuals"),
                           DT::dataTableOutput("GraderMonthTab"),
                           h3("Standard deviation of residuals"),
                           DT::dataTableOutput("GraderSDMonthTab"),
                           h3("Effect size"),
                           DT::dataTableOutput("GraderEffectMonthTab")
                  ),
                  tabPanel("Help",
                           includeMarkdown("help.Rmd")
                           #includeHTML("help.html")
                           )#,
                  # tabPanel("Summary plots", value = "ms1",
                  #          shiny::HTML("<div class='pagebreak'> </div>"),
                  #          h3("MSA marbling"),
                  #          plotOutput("marbGraderMonthlyPlot2"),
                  #          h3("Hump height"),
                  #          plotOutput("humpGraderMonthlyPlot2"),
                  #          shiny::HTML("<div class='pagebreak'> </div>"),
                  #          h3("Ossification"),
                  #          plotOutput("ossGraderMonthlyPlot2"),
                  #          h3("Rib fat"),
                  #          plotOutput("rbfGraderMonthlyPlot2"),
                  #          shiny::HTML("<div class='pagebreak'> </div>"),
                  #          h3("pH"),
                  #          plotOutput("phGraderMonthlyPlot2")
                  # )#,
                  # tabPanel("Other plots",
                  #          h3("MSA marbling"),
                  #          plotOutput("marbGraderMonthlyPlot",height = "600px"),
                  #          h3("Hump height"),
                  #          plotOutput("humpGraderMonthlyPlot",height = "600px"),
                  #          shiny::HTML("<div class='pagebreak'> </div>"),
                  #          h3("Ossification"),
                  #          plotOutput("ossGraderMonthlyPlot",height = "600px"),
                  #          h3("Rib fat"),
                  #          plotOutput("rbfGraderMonthlyPlot",height = "600px"),
                  #          shiny::HTML("<div class='pagebreak'> </div>"),
                  #          h3("pH"),
                  #          plotOutput("phGraderMonthlyPlot",height = "600px")
                  # )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(shiny.maxRequestSize=50*1024^2) 
  
  datain <- reactive({
    inFile <- input$file1
    validate(
      need(!is.null(input$file1), "Please upload a data set")
    )
    if (is.null(inFile))
      return(NULL)
    
    input_file_format <- tools::file_ext(inFile$name)
    new_file_name <- paste0(inFile$datapath, ".", input_file_format)
    file.rename(inFile$datapath, new_file_name)
    dat <- readr::read_csv(new_file_name, 
                           col_types = cols(Grader_No = col_character(),
                                            Grader_no = col_character(),
                                            grader_no = col_character(),
                                            Grader = col_character(),
                                            grader = col_character(),
                                            GradeDate = col_date(format = "%d/%m/%Y"), 
                                            gradedate = col_date(format = "%d/%m/%Y"), 
                                            gradeDate = col_date(format = "%d/%m/%Y")#, 
                                            #gradetime = col_time(format = "%H:%M:%S"), 
                                            #killtime = col_time(format = "%H:%M:%S"), 
                                            #KillDate = col_date(format = "%d/%m/%Y")
                                            )
                           )
    names(dat) = tolower(make.names(names(dat)))
    dat$month = months(dat$gradedate)
    colnames(dat)[colnames(dat)=="grader_no"] = "grader"
    return(dat)
  })
  
  data = reactive({
    dat = datain()
    if(!is.null(input$selectSex)){
      dat = subset(dat,subset = is.element(dat$sex,input$selectSex))
    }
    if(!is.null(input$selectHGP)){
      dat = subset(dat,subset = is.element(dat$hgp,input$selectHGP))
    }
    if(!is.null(input$selectfeed)){
      dat = subset(dat,subset = is.element(dat$feedtype,input$selectfeed))
    }
    if(!is.null(input$selectmonth)){
      dat = subset(dat,subset = is.element(dat$month,input$selectmonth))
    }
    if(!is.null(input$selectgrader)){
      dat = subset(dat,subset = is.element(dat$grader,input$selectgrader))
    }
    return(dat)
  })
  output$uiselect = renderUI({
    list(
      conditionalPanel(
        condition = "input.tabpanel == 'ms1' | input.tabpanel == 'ms2'",
        uiOutput("graderSelect")
      ),
      conditionalPanel(
        condition = "input.tabpanel != 'ms1' & input.tabpanel != 'ms2'",
        selectizeInput("selectSex",label = "Sex",
                       choices = names(table(datain()$sex)),
                       multiple = TRUE,
                       selected = NULL),
        selectizeInput("selectHGP",label = "HGP",
                       choices = names(table(datain()$hgp)),
                       multiple = TRUE,
                       selected = NULL),
        selectizeInput("selectfeed",label = "Feed type",
                       choices = names(table(datain()$feedtype)),
                       multiple = TRUE,
                       selected = NULL),
        selectizeInput("selectmonth",label = "Month",
                       choices = month.name[is.element(month.name,names(table(datain()$month)))],
                       multiple = TRUE,
                       selected = NULL),
        
        selectizeInput("selectgrader",label = "Graders",
                       choices = names(table(datain()$grader)),
                       multiple = TRUE,
                       selected = names(table(datain()$grader)))
      )
    )
  })
  
  output$phPlot <- renderPlot({
    #hist(data()$ph,main="pH")
    oall.plot(data = data(),
              variable = "ph", 
              title = "pH")
  })
  output$msamarblingPlot <- renderPlot({
    #hist(data()$msamarbling,main = "MSA marbling scores")
    oall.plot(data = data(),
              variable = "msamarbling", 
              title = "MSA marbling")
  })
  output$humpcoldPlot <- renderPlot({
    #hist(data()$humpcold,main = "Hump height")
    oall.plot(data = data(),
              variable = "humpcold", 
              title = "Hump height")
  })
  output$ossificationcoldPlot <- renderPlot({
    #hist(data()$ossificationcold,main = "Ossification")
    oall.plot(data = data(),
              variable = "ossificationcold", 
              title = "Ossification")
  })
  output$ribfatcoldPlot <- renderPlot({
    #hist(data()$ribfatcold,main = "Rib fat")
    oall.plot(data = data(),
              variable = "ribfatcold", 
              title = "Rib fat")
  })
  output$totalhscwPlot <- renderPlot({
    #hist(data()$totalhscw, main = "Hot carcase weight")
    oall.plot(data = data(),
              variable = "totalhscw", 
              title = "Hot carcase weight")
  })
  
  output$continuousPlot <- renderPlot({
    tst1 = reshape2::melt(data()[,c("grader","msamarbling","ph","humpcold","ossificationcold","ribfatcold","totalhscw")],
                          measure.vars = c("msamarbling","humpcold","ossificationcold","ribfatcold","ph","totalhscw"))
    
    var1_names <- c(
      `msamarbling`="MSA marbling",
      `humpcold`="Hump height",
      `ph`="pH",
      `ossificationcold`="Ossification",
      `ribfatcold`="Rib fat",
      `totalhscw`="Hot carcase weight"
    )
    
    ggplot(data=tst1,aes(x=variable,y=value)) + geom_violin() + 
      facet_wrap(~variable, scales="free", labeller = as_labeller(var1_names)) + 
      xlab("") + ylab("") + theme_bw(base_size = 14) + 
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      stat_summary(aes(group=variable),fun.y=mean, geom="point", shape=19, size=2)
    
  })
  
  output$discretePlot <- renderPlot({
    tst2 = reshape2::melt(data()[,c("grader","feedtype","sex","hgp")],measure.vars = c("feedtype","sex","hgp"))
    
    var2_names <- c(
      `feedtype`="Feed type",
      `sex`="Sex",
      `hgp`="HGP"
    )
    
    ggplot(tst2) + geom_bar(aes(as.factor(value))) + 
      facet_grid(~variable, labeller = as_labeller(var2_names),
                 scale="free_x", space = "free_x") + 
      xlab("") + ylab("") + theme_bw(base_size = 14) 
  })
  # output$feedtypePlot <- renderPlot({
  #   plot(table(data()$feedtype),main = "Feed type")
  # })
  # output$sexPlot <- renderPlot({
  #   plot(table(data()$sex),main = "Sex")
  # })
  # output$hgpPlot <- renderPlot({
  #   plot(table(data()$hgp),main = "HGP")
  # })
  
  output$graderText = renderText({
    validate(
      need(!is.null(input$file1), "")
    )
    paste("The following graders have been identified in the data set:")
  })
  output$graderTable = DT::renderDataTable({
    tab = data.frame(table(data()$grader))
    colnames(tab) = c("Grader ID","Number of carcases graded")
    DT::datatable(tab,rownames = FALSE,options = list(dom = 't'))
  })
  
  output$graderMonthText = renderText({
    validate(
      need(!is.null(input$file1), "")
    )
    paste("Number of carcases graded by each grader broken down by month:")
  })
  
  output$graderMonthTable = DT::renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    x = data()
    x$month = ordered(x$month,month.name[is.element(month.name,names(table(x$month)))])
    tab = as.data.frame.matrix(table(x$month,x$grader))
    #colnames(tab) = c("Grader ID","Number of carcases graded")
    DT::datatable(tab,rownames = TRUE,options = list(dom = 't',pageLength = 12))
  })
  
  
  model.data = reactive({
    mdat = subset(data(),select=c("ph","feedtype",
                                  "sex","hgp",
                                  "msamarbling","humpcold",
                                  "ossificationcold","ribfatcold",
                                  "totalhscw","month"))
    
    if(dim(table(data()$sex))<=1){
      mdat = subset(mdat, select = -sex)
    }
    if(dim(table(data()$hgp))<=1){
      mdat = subset(mdat, select = -hgp)
    }
    if(dim(table(data()$feedtype))<=1){
      mdat = subset(mdat, select = -feedtype)
    }
    
    if(dim(table(data()$month))<=1){
      mdat = subset(mdat, select = -month)
    }
    return(mdat)
    
  })
  
  ph.mod = reactive({
    lm(ph ~ ., data = model.data())
  })
  marb.mod = reactive({
    lm(msamarbling ~ ., data = model.data())
  })
  hump.mod = reactive({
    lm(humpcold ~ ., data = model.data())
  })
  oss.mod = reactive({
    lm(ossificationcold ~ ., data = model.data())
  })
  rbf.mod = reactive({
    lm(ribfatcold ~ ., data = model.data())
  })  
  output$marbOutputs = renderPrint({
    summary(marb.mod())
  })
  output$phOutputs = renderPrint({
    summary(ph.mod())
  })
  output$humpOutputs = renderPrint({
    summary(hump.mod())
  })
  output$ossOutputs = renderPrint({
    summary(oss.mod())
  })
  output$rbfOutputs = renderPrint({
    summary(rbf.mod())
  })
  
  output$phGraderPlot = renderPlot({
    ph.dat = data.frame(data()$grader,data()$ph,ph.mod()$residuals)
    plot.fn(data = ph.dat, title = "pH")
  })
  
  
  
  
  output$rbfGraderPlot = renderPlot({
    rbf.dat = data.frame(data()$grader,data()$ribfatcold,rbf.mod()$residuals)
    plot.fn(data = rbf.dat, title = "Rib fat")
  })
  output$humpGraderPlot = renderPlot({
    hump.dat = data.frame(data()$grader,data()$humpcold,hump.mod()$residuals)
    plot.fn(data = hump.dat, title = "Hump height")
  })
  output$marbGraderPlot = renderPlot({
    marb.dat = data.frame(data()$grader,data()$msamarbling,marb.mod()$residuals)
    plot.fn(data = marb.dat, title = "MSA marbling")
  })
  output$marbGraderText = renderText({
    if(marb.mod()$r.sq>0.2){
      return("This model has quite reasonable predictive accuracy.")
    } else if(marb.mod()$r.sq>0.1){
      return("This model has moderate predictive accuracy.")
    } else {
      return("This model has very limited predictive accuracy, so interpret any results with caution.")
    }
  })
  
  output$GraderTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader),mean)
    x = data.frame(marb.resid,hump.resid[,2],oss.resid[,2],rbf.resid[,2],ph.resid[,2])
    colnames(x) = c("Grader ID","Marbling score","Hump height","Ossification","Rib fat","pH")
    DT::datatable(x,rownames = FALSE,options = list(dom = 't')) %>% DT::formatRound(columns = 2:6,digits=2)
  })
  
  output$graderSelect = renderUI({
    selectizeInput("individualGrader",label="Summary details for which grader?",
                   choices = input$selectgrader)
  })
  
  output$GraderMonthTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader,data()$month),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader,data()$month),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader,data()$month),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader,data()$month),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),mean)
    x = data.frame(marb.resid,hump.resid[,3],oss.resid[,3],rbf.resid[,3],ph.resid[,3])
    
    colnames(x) = c("Grader ID","Month","Marbling score","Hump height","Ossification","Rib fat","pH")
    x = subset(x,x[,1]==input$individualGrader)
    x$MonthNo = ordered(x$Month,month.name)
    levels(x$MonthNo) = 1:12
    DT::datatable(x, rownames = FALSE, 
                  options = list(dom = 't',
                                 pageLength = 12, 
                                 order=list(list(7,'asc'))
                  )
    ) %>% DT::formatRound(columns = 3:7,digits=2)
  })
  
  
  output$GraderEffectTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader),mean)
    x = data.frame(marb.resid[,1],
                   marb.resid[,2]/sd(marb.mod()$resid),
                   hump.resid[,2]/sd(hump.mod()$resid),
                   oss.resid[,2]/sd(oss.mod()$resid),
                   rbf.resid[,2]/sd(rbf.mod()$resid),
                   ph.resid[,2]/sd(ph.mod()$resid))
    colnames(x) = c("Grader ID","Marbling score","Hump height","Ossification","Rib fat","pH")
    DT::datatable(x,rownames = FALSE,options = list(dom = 't')) %>% DT::formatRound(columns = 2:6,digits=2)
  })
  
  MonthEffect = reactive({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader,data()$month),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader,data()$month),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader,data()$month),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader,data()$month),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),mean)
    n.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),length)
    x = data.frame(marb.resid[,1],marb.resid[,2],
                   marb.resid[,3]/sd(marb.mod()$resid),
                   hump.resid[,3]/sd(hump.mod()$resid),
                   oss.resid[,3]/sd(oss.mod()$resid),
                   rbf.resid[,3]/sd(rbf.mod()$resid),
                   ph.resid[,3]/sd(ph.mod()$resid),
                   n.resid[,3])
    colnames(x) = c("Grader","Month","Marbling","Hump","Ossification","Ribfat","pH","n")
    x$Month = ordered(x$Month,month.name)
    levels(x$Month) = month.abb
    return(x)
  })
  
  MonthResid = reactive({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader,data()$month),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader,data()$month),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader,data()$month),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader,data()$month),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),mean)
    n.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),length)
    x = data.frame(marb.resid[,1],marb.resid[,2],
                   marb.resid[,3],
                   hump.resid[,3],
                   oss.resid[,3],
                   rbf.resid[,3],
                   ph.resid[,3],
                   n.resid[,3])
    colnames(x) = c("Grader","Month","Marbling","Hump","Ossification","Ribfat","pH","n")
    x$Month = ordered(x$Month,month.name)
    levels(x$Month) = month.abb
    return(x)
  })
  
  
  
  output$MarblingResidPlot = renderPlot({
    x = MonthResid()
    ggplot(data=x, aes(x=Month, y=Marbling, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average marbling score residual") +
      theme_bw(base_size = 14) 
  })
  
  output$HumpResidPlot = renderPlot({
    x = MonthResid()
    ggplot(data=x, aes(x=Month, y=Hump, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average hump height residual") +
      theme_bw(base_size = 14) 
  })
  
  output$OssificationResidPlot = renderPlot({
    x = MonthResid()
    ggplot(data=x, aes(x=Month, y=Ossification, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average ossification residual") +
      theme_bw(base_size = 14) 
  })
  
  output$RibfatResidPlot = renderPlot({
    x = MonthResid()
    ggplot(data=x, aes(x=Month, y=Ribfat, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average ribfat residual") +
      theme_bw(base_size = 14) 
  })
  
  output$pHResidPlot = renderPlot({
    x = MonthResid()
    ggplot(data=x, aes(x=Month, y=pH, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average pH residual") +
      theme_bw(base_size = 14) 
  })
  
  output$MarblingEffectPlot = renderPlot({
    x = MonthEffect()
    ggplot(data=x, aes(x=Month, y=Marbling, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Marbling score effect size") +
      theme_bw(base_size = 14) 
  })
  
  output$HumpEffectPlot = renderPlot({
    x = MonthEffect()
    ggplot(data=x, aes(x=Month, y=Hump, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Hump height effect size") +
      theme_bw(base_size = 14) 
  })
  
  output$OssificationEffectPlot = renderPlot({
    x = MonthEffect()
    ggplot(data=x, aes(x=Month, y=Ossification, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Ossification effect size") +
      theme_bw(base_size = 14) 
  })
  
  output$RibfatEffectPlot = renderPlot({
    x = MonthEffect()
    ggplot(data=x, aes(x=Month, y=Ribfat, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Rib fat effect size") +
      theme_bw(base_size = 14) 
  })
  
  output$pHEffectPlot = renderPlot({
    x = MonthEffect()
    ggplot(data=x, aes(x=Month, y=pH, group=Grader)) +
      geom_line(aes(color=Grader)) +
      geom_point(aes(color=Grader,size=n)) +
      ylab("Average pH residual") +
      theme_bw(base_size = 14) 
  })
  
  output$GraderEffectMonthTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader,data()$month),mean)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader,data()$month),mean)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader,data()$month),mean)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader,data()$month),mean)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),mean)
    x = data.frame(marb.resid[,1],marb.resid[,2],
                   marb.resid[,3]/sd(marb.mod()$resid),
                   hump.resid[,3]/sd(hump.mod()$resid),
                   oss.resid[,3]/sd(oss.mod()$resid),
                   rbf.resid[,3]/sd(rbf.mod()$resid),
                   ph.resid[,3]/sd(ph.mod()$resid))
    
    colnames(x) = c("Grader ID","Month","Marbling score","Hump height","Ossification","Rib fat","pH")
    x = subset(x,x[,1]==input$individualGrader)
    x$MonthNo = ordered(x$Month,month.name)
    levels(x$MonthNo) = 1:12
    DT::datatable(x, rownames = FALSE, 
                  options = list(dom = 't',
                                 pageLength = 12, 
                                 order=list(list(7,'asc'))
                  )
    ) %>% DT::formatRound(columns = 3:7,digits=2)
  })
  
  
  
  output$GraderSdTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader),sd)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader),sd)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader),sd)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader),sd)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader),sd)
    x = data.frame(marb.resid,hump.resid[,2],oss.resid[,2],rbf.resid[,2],ph.resid[,2])
    colnames(x) = c("Grader ID","Marbling score","Hump height","Ossification","Rib fat","pH")
    DT::datatable(x,rownames = FALSE,options = list(dom = 't')) %>% DT::formatRound(columns = 2:6,digits=2)
  })
  
  
  output$GraderSDMonthTab = DT::renderDataTable({
    marb.resid = aggregate(marb.mod()$resid,by=list(data()$grader,data()$month),sd)
    hump.resid = aggregate(hump.mod()$resid,by=list(data()$grader,data()$month),sd)
    oss.resid = aggregate(oss.mod()$resid,by=list(data()$grader,data()$month),sd)
    rbf.resid = aggregate(rbf.mod()$resid,by=list(data()$grader,data()$month),sd)
    ph.resid = aggregate(ph.mod()$resid,by=list(data()$grader,data()$month),sd)
    x = data.frame(marb.resid,hump.resid[,3],oss.resid[,3],rbf.resid[,3],ph.resid[,3])
    
    colnames(x) = c("Grader ID","Month","Marbling score","Hump height","Ossification","Rib fat","pH")
    x = subset(x,x[,1]==input$individualGrader)
    x$MonthNo = ordered(x$Month,month.name)
    levels(x$MonthNo) = 1:12
    DT::datatable(x, rownames = FALSE, 
                  options = list(dom = 't',
                                 pageLength = 12, 
                                 order=list(list(7,'asc'))
                  )
    ) %>% DT::formatRound(columns = 3:7,digits=2)
  })
  
  output$phGraderMonthlyPlot = renderPlot({
    ph.dat = data.frame(data()$grader,data()$ph,ph.mod()$residuals,data()$month)
    plot.monthly.fn(data = ph.dat, title = "pH")
  })
  
  output$rbfGraderMonthlyPlot = renderPlot({
    rbf.dat = data.frame(data()$grader,data()$ribfatcold,rbf.mod()$residuals,data()$month)
    plot.monthly.fn(data = rbf.dat, title = "Rib fat")
  })
  output$humpGraderMonthlyPlot = renderPlot({
    hump.dat = data.frame(data()$grader,data()$humpcold,hump.mod()$residuals,data()$month)
    plot.monthly.fn(data = hump.dat, title = "Hump height")
  })
  output$marbGraderMonthlyPlot = renderPlot({
    marb.dat = data.frame(data()$grader,data()$msamarbling,marb.mod()$residuals,data()$month)
    plot.monthly.fn(data = marb.dat, title = "MSA marbling")
  })
  
  output$ossGraderMonthlyPlot = renderPlot({
    oss.dat = data.frame(data()$grader,data()$ossificationcold,oss.mod()$residuals,data()$month)
    plot.monthly.fn(data = oss.dat, title = "Ossification")
  })
  
  output$phGraderMonthlyPlot2 = renderPlot({
    dat = data.frame(data()$grader,data()$ph,ph.mod()$residuals,data()$month)
    dat = subset(dat,dat[,1]==input$individualGrader)
    plot.monthly.fn2(data = dat, title = "pH")
  })
  
  output$rbfGraderMonthlyPlot2 = renderPlot({
    dat = data.frame(data()$grader,data()$ribfatcold,rbf.mod()$residuals,data()$month)
    dat = subset(dat,dat[,1]==input$individualGrader)
    plot.monthly.fn2(data = dat, title = "Rib fat")
  })
  output$humpGraderMonthlyPlot2 = renderPlot({
    dat = data.frame(data()$grader,data()$humpcold,hump.mod()$residuals,data()$month)
    dat = subset(dat,dat[,1]==input$individualGrader)
    plot.monthly.fn2(data = dat, title = "Hump height")
  })
  output$marbGraderMonthlyPlot2 = renderPlot({
    dat = data.frame(data()$grader,data()$msamarbling,marb.mod()$residuals,data()$month)
    dat = subset(dat,dat[,1]==input$individualGrader)
    plot.monthly.fn2(data = dat, title = "MSA marbling")
  })
  
  output$ossGraderMonthlyPlot2 = renderPlot({
    dat = data.frame(data()$grader,data()$ossificationcold,oss.mod()$residuals,data()$month)
    dat = subset(dat,dat[,1]==input$individualGrader)
    plot.monthly.fn2(data = dat, title = "Ossification")
  })
  
  
  
  output$PredictiveTab = DT::renderDataTable({
    r2 = c(summary(marb.mod())$r.sq*100, 
           summary(hump.mod())$r.sq*100, 
           summary(oss.mod())$r.sq*100, 
           summary(rbf.mod())$r.sq*100, 
           summary(ph.mod())$r.sq*100)
    stdevs = c(sd(marb.mod()$resid), 
               sd(hump.mod()$resid), 
               sd(oss.mod()$resid), 
               sd(rbf.mod()$resid), 
               sd(ph.mod()$resid))
    res = data.frame(matrix(c(r2,stdevs),ncol=5,byrow = TRUE))
    colnames(res) = c("Marbling score","Hump height","Ossification","Rib fat","pH")
    rownames(res) = c("R2","Residual SD")
    DT::datatable(res,rownames = TRUE,options = list(dom = 't')) %>% DT::formatRound(columns = 1:5,digits=1)
  })
  
  output$ossGraderPlot = renderPlot({
    oss.dat = data.frame(data()$grader,data()$ossificationcold,oss.mod()$residuals)
    plot.fn(data = oss.dat, title = "Ossification")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

