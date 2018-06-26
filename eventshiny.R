#load packages
library(shiny)
library(ggplot2)

#global objects
#create a list of result files
result_files=list.files()
result_files=result_files[grep("results.csv",result_files,ignore.case=TRUE)]

#define convenience functions for t statistics (used as FUN argument in aggregate)
tvals=function(x) {
  if(length(x)<5) return(NA)
  t=t.test(x)
  t$statistic
}
pval_gt0=function(x) {
  if(length(x)<5) return(NA)
  t=t.test(x,alternative="greater")
  t$p.value}
pval_lt0=function(x) {
  if(length(x)<5) return(NA)
  t=t.test(x,alternative="less")
  t$p.value
}

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Event Study"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
     
      
      #Input: which results file?
      selectInput(inputId="filename",
                  label="Select file for analysis",
                  choices=result_files),
      #next two inputs are driven by file content
      #see related renderUI commands in the server
      uiOutput("pickportfolio"),
      uiOutput("pickevent"),
      #gather rest of input 
      selectInput(inputId="startday",
                  label="Analysis time frame",
                  choices=c("Announce Date","Effective Date","Announce to Effective")),
      selectInput(inputId="cumulative",
                  label="Cumulative?",
                  choices=c("Cumulative","Daily Returns")),
      selectInput(inputId="excess",
                  label="Absolute or Excess Returns?",
                  choices=c("Excess Return","Absolute Return")),
      dateRangeInput("daterange", "Date range:",
                     start = "2010-01-01",
                     end   = Sys.Date(),
                     min="1991-01-01",
                     max=Sys.Date()),
      
      
      #add a download button to save graph as pdf
      downloadButton("report", "Save numbers to csv")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      #display plot
      plotOutput("summary"),
      
      #display statistical summary
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  #define UI elements based on file content 
  #dropdown list based on indices present in the result file
  output$pickportfolio= renderUI({
    dataset=datasetInput()
    portfolios=as.character(unique(dataset$index))
    selectInput(inputId="Index",
                label="Which Portfolio?",
                choices=c(portfolios,"All"))
  })
  
  #dropdown list based on event categories present in the results file
  output$pickevent=renderUI({
    dataset=datasetInput()
    events=unique(dataset$Category)
    selectInput(inputId="event_type",
                label="Which event type?",
                choices=events)
  })
 
  #define reactive actions
  #load the file selected in the filename dropdown
  datasetInput <- reactive({
    d=read.csv(input$filename)
    d$andate=as.Date(d$andate)
    e=d$EffPlus
    n=length(e)
    if(is.na(e[n])) stop("last value in EffPlus is NA")
    for(k in 1:(n-1)) {
      if(is.na(e[n-k])) e[n-k]=-1+e[1+n-k]
  }
    d$dayplus=e
    d$EffPlus=e
    f=d$cum_eff_mu
    f[is.na(f)]=0
    d$cum_eff_mu=f
    g=d$cum_effsec_mu
    g[is.na(g)]=0
    d$cum_effsec_mu=g
    #browser()
    d
  })
  
    #subset the data based on input selections
  
  ans=reactive({
    ans=list()
    dataset <- datasetInput()
    
    #subset for index, event type and date range
    if(input$Index != "All") dataset=subset(dataset,dataset$index==input$Index)
    dataset=subset(dataset,dataset$Category==input$event_type)
    dataset=subset(dataset,dataset$andate>=input$daterange[1])
    dataset=subset(dataset,dataset$andate<=input$daterange[2])
    
    #how many observations
    nobs=sum(dataset$AnncPlus==1)
    
    config=paste(input$cumulative,input$excess,input$startday)
    #subset for startday
    #default dayplus is effective date
    #switch to announce date if necessary
    if (grepl(".*Announce",config)) dataset$dayplus=dataset$AnncPlus
    if(grepl(".*Effective Date",config)) {
      dataset=subset(dataset,dataset$dayplus>=0)
    } else {
      if (grepl(".*Announce to",config)) {
        dataset=subset(dataset,dataset$EffPlus<=0)
      }
    }

    #select return to analyze based startday, cumulative and excess
    
    plotreturn=dataset$excessmu
    
    if(grepl("Dai.*Abs",config)) plotreturn=dataset$secmu

    if(grepl("Cum.*Exc.*Ann",config)) plotreturn=dataset$cum_annc_mu

    if(grepl("Cum.*Exc.*Effective D",config)) plotreturn=dataset$cum_eff_mu

    if (grepl("Cum.*Abs.*Ann",config)) plotreturn=dataset$cum_secmu

    if (grepl("Cum.*Abs.*Effective D",config))  plotreturn=dataset$cum_effsec_mu
   
    dataset$plotreturn=plotreturn
    #browser()
   
  #define ylabel for graphing based on selections
    ylabel="Return"
    if (input$excess=="Excess Return") {
      ylabel=paste("Excess",ylabel)
    } 
    if (input$cumulative=="Cumulative") {
      ylabel=paste("Cumulative",ylabel)
    } else {
      ylabel=paste("Daily",ylabel)
    }

  #define xlabel based on selections
    xlabel="Trading days after announcement"
    if(input$startday=="Effective Date") xlabel="Trading days after effective"

  #create a ggplot
   #browser()
    
    eventplot=ggplot(dataset,
                     aes(x=factor(dayplus),y=100*plotreturn,color=IsEff))+
      geom_boxplot()+
      ylim(c(-5,5))+
      ggtitle(paste(input$event_type,input$Index,nobs,"observations"))+
      xlab(xlabel)+
      ylab(ylabel)
    ans$eventplot=eventplot
  #create stats
     datastats=data.frame(
      Days=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=mean)[,1],
      Mean=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=mean)[,2],
      Min=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=min)[,2],
      Median=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=median)[,2],
      Max=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=max)[,2],
      SD=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=sd)[,2],
      T.Value=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=tvals)[,2],
      P.Val_LTZero=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=pval_lt0)[,2],
      P.Val_GTZero=aggregate(100*dataset$plotreturn,by=list(dataset$dayplus),FUN=pval_gt0)[,2]
    )
    ans$stats=datastats
    ans
 })
  
  #render outputs for display
  #the plot
  output$summary <- renderPlot({
    answer=ans()
    eventplot=answer$eventplot
    print(eventplot)
  })
  
  #the statistical table
  output$view <- renderTable({
    answer=ans()
    round(answer$stats,2)
  })
  
  #download handler
  output$report <- downloadHandler(
    #make filename including selections in input
    filename = function() {
      # paste0(paste(  input$pickportfolio,
      #                input$pickevent,
      #                input$startday,
      #                input$cumulative,
      #                input$excess,sep="_"),
      #                ".csv")
      paste(input$pickportfolio,".csv", sep="")
      },
    content = function(file) {
      answer=ans()
      df=answer$stats
      write.csv(df,file,row.names=FALSE)
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      #tempReport <- file.path(tempdir(), "eventreport.Rmd")
      #file.copy("eventreport.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      #params <- list(graph = graph_a())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      #rmarkdown::render("eventreport.rmd", output_file = file,
                        #params = params,
                        #envir = new.env(parent = globalenv())
      #)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
