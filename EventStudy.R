#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load required packages
#library(Rbbg)
library(lubridate)
library(tidyr)
library(zoo)
library(shiny)
event_files=list.files()
event_files=event_files[grep(".csv",event_files,ignore.case=TRUE)]
event_files=event_files[!grepl("results.csv",event_files,ignore.case=TRUE)]
event_files=event_files[!grepl("bad tickers.csv",event_files,ignore.case=TRUE)]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Event Study Processor"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("file1", "Choose CSV File",
                  choices=event_files,
                  selected=NULL
        ),
        selectInput("onlynew",
                    "Process instructions",
                    c("Process all", "Process only new entries"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
         tableOutput("bad_tickers")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   badtickers=reactive({
     # Here is where you enter the file name for the csv file
     # do not include .csv in the name -- we add that later
     #
     inFile <- input$file1
    # browser()
     if (inFile=="")
       return(NULL)
     
     filename=inFile
     filename=gsub(".csv","",filename)
     #
     # defaults to "event" if no filename provided
     #
     # results will be written to a csv file named paste(filename,"results.csv")
     #
     # Ticker information
     # change to null if your data already has the suffixes 
     #
     tickersuffix="US EQUITY"
     indexsuffix="INDEX"
     #
     # once a file is processed it will mark the rows completed in a column called "processed"
     # if you want to reprocess an entire file, change the following to TRUE
     # when you add entries to a file, leave the processed field blank for the new entries
     # if process_entire is FALSE, only those entries will be processed and the results added to the 
     # existing results file
     process_entire= input$onlynew=="Process all"
     #if ((!paste(filename,"results.csv") %in% list.files())& process_entire==FALSE) {
     #  stop("there are no prior results to merge -- did you mean process_entire = TRUE?")
     #}
     #
     #
     #
     
     #read event file
     #if(is.na(filename)) filename="event"
     event.data=read.csv(paste0(filename,".csv"),stringsAsFactors = FALSE)
     event.data2=event.data
     event.data2$processed=TRUE
     if("processed" %in% rownames(event.data)&!process_entire) {
       procindex=which(TRUE==event.data$processed)
       event.data=event.data[-procindex,]
       if(nrow(event.data)==0) stop("Entire file already processed -- change process_entire to TRUE if you want to reprocess")
       
       
     }
     event.data$Eff_Date=as.Date(event.data$Eff_Date,format="%m/%d/%Y")
     event.data$An_Date=as.Date(event.data$An_Date,format="%m/%d/%Y")
     event.data$Ticker=paste(event.data$Ticker,tickersuffix)
     event.data$Benchmark=paste(event.data$Benchmark,indexsuffix)
     #open BBG
     conn=blpConnect(verbose=FALSE)
     
     #get index data
     firstdate=min(event.data$An_Date)-4
     lastdate=min(today()-1,max(event.data$Eff_Date)+20)
     field="PX_LAST"
     indtickers=unique(event.data$Benchmark)
     bbgdat=bdh(conn,indtickers,field,start_date=firstdate,end_date=lastdate)
     bbgdat2=spread(bbgdat[!is.na(bbgdat$PX_LAST),],ticker,PX_LAST)
     bbgdatz=zoo(bbgdat2[,-1],as.Date(bbgdat2$date))
     bbgdatz=log(bbgdatz)
     bbgdatz=diff(bbgdatz)
     bbgdat3=data.frame(date=time(bbgdatz),coredata(bbgdatz))
     indexmu=gather(bbgdat3,Benchmark,indmu,-date)
     indexmu$index=gsub("\\."," ",indexmu$Benchmark)
     #initiate empty result data frame
     if(process_entire) {
       result.df=data.frame(date=vector(),
                            Benchmark=vector(),
                            Ticker=vector(),
                            Category=vector(),
                            secmu=vector(),
                            AnncPlus=vector(),
                            EffPlus=vector(),
                            IsEff=vector(),
                            indmu=vector(),
                            excessmu=vector(),
                            cum_annc_mu=vector(),
                            cum_secmu=vector(),
                            cum_eff_mu=vector(),
                            cum_effsec_mu=vector(),
                            andate=vector())
     } else {
       result.df=read.csv(paste(filename,"result.csv"))
     }
     bad.tickers=vector()
     field=c("PX_LAST","PX_OPEN")
     #process events
     for (i in 1:length(event.data$Ticker)) {
       firstdate=event.data$An_Date[i]-4
       lastdate=event.data$Eff_Date[i]+15
       bbgdat=try(bdh(conn,
                      event.data$Ticker[i],
                      field,
                      start_date=firstdate,
                      end_date=lastdate))
       if (class(bbgdat)=="try-error") {
         bad.tickers=c(bad.tickers,event.data$Ticker[i])
         next(i)
       }
       if (0==length(bbgdat$PX_LAST)) {
         bad.tickers=c(bad.tickers,event.data$Ticker[i])
         next(i)
       }
       bbgdat=bbgdat[!is.na(bbgdat$PX_LAST),]
       bbgdatz=zoo(bbgdat$PX_LAST,as.Date(bbgdat$date))
       an_ind=which(time(bbgdatz)==event.data$An_Date[i])
       bbgdatz[an_ind]=bbgdat$PX_OPEN[an_ind+1]
       bbgdatz=diff(log(bbgdatz))
       bbgdatz=bbgdatz[time(bbgdatz)>as.Date(event.data$An_Date[i])]
       #aplus=paste0("Annc+",0:(length(bbgdatz)-1))
       aplus=1:length(bbgdatz)
       eplus=time(bbgdatz)>event.data$Eff_Date[i]
       eplus=cumsum(eplus)
       eplus[eplus==0]=NA
       eplus=eplus
       iseff=!is.na(eplus)
       #eplus=paste0("Eff+",eplus)
       eplus[grep("NA",eplus)]=NA
       df=data.frame(date=time(bbgdatz),Ticker=event.data$Ticker[i],
                     Category=event.data$Category[i],
                     secmu=coredata(bbgdatz),AnncPlus=aplus,EffPlus=eplus,IsEff=iseff,
                     index=event.data$Benchmark[i])
       df=merge(df,indexmu)
       df$excessmu=df$secmu-df$indmu
       df$cum_annc_mu=cumsum(df$excessmu)
       df$cum_secmu=cumsum(df$secmu)
       eind=which(!is.na(df$EffPlus))
       df$cum_eff_mu=NA
       df[eind,"cum_eff_mu"]=cumsum(df$excessmu[eind])
       df$cum_effsec_mu=NA
       df[eind,"cum_effsec_mu"]=cumsum(df$secmu[eind])
       df$andate=event.data$An_Date[i]
       #add to result data frame
       result.df=rbind(result.df,df)
       print(paste(i,event.data$Ticker[i]))
     }
     write.csv(result.df,paste(filename,"results.csv"))
     write.csv(event.data2,paste0(filename,".csv"))
     x=blpDisconnect(conn)
     #
     # The following tickers were not recognized by BBG
     if(length(bad.tickers)>0) {
       print(c("The following are bad event tickers",bad.tickers))
       write.csv(table(bad.tickers),paste(filename,"bad tickers.csv"))
     } else {
       print("There were no bad event tickers")
     }
     
     bad.tickers
   })
   
   output$bad_tickers=renderTable({
     if (is.null(badtickers())) return("No file selected yet")
     b=table(badtickers())
     bdf=as.data.frame(b)
     colnames(bdf)=c("BadTickers","Frequency")
     bdf
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

