library(readr)  # for read_csv
library(knitr)  # for kable
library(RCurl)
library(stringr)
library(shinydashboard)
library(shiny)
library(ggplot2)
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
# library(ggpubr)
rm(list = ls())
invisible(gc)
fileCouncils <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos.csv")
fileRegions <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")

Council_Cases <- read.csv(text=fileCouncils)

Councillist=colnames(Council_Cases)
Councillist=Councillist[2:length(Councillist)]
Councillist=str_replace_all(Councillist, "\\.", " ")
Councillist=str_to_title(Councillist)
colnames(Council_Cases)=c("data",Councillist)

Region_Cases <- read.csv(text=fileRegions)
Region_Cases = Region_Cases[,c("data","confirmados_arsnorte","confirmados_arscentro","confirmados_arslvt","confirmados_arsalentejo","confirmados_arsalgarve","confirmados_acores","confirmados_madeira")]
Regionlist = c("Norte","Centro","Lisboa e Vale do Tejo","Alentejo","Algarve","Acores","Madeira")
colnames(Region_Cases)=c("data",Regionlist)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabItem(tabName = "councildata",
            fluidRow(
              tabBox(width = 12,
                     id="tab1",
                     tabPanel(
                       "Region data",
                       # selectInput("location_region", "Region:",
                       #             Regionlist,
                       #             selected = "Lisboa e Vale do Tejo", selectize = TRUE),
                       plotOutput("newcasesplot_region", height = "600px"),
                       "Source: https://github.com/dssg-pt/covid19pt-data"
                     ),
                     tabPanel(
                       "Council data",
                       selectInput("location_council", "Council:",
                                   Councillist,
                                   selected = "Lisboa", selectize = TRUE),
                       plotOutput("newcasesplot_council"),
                       "Source: https://github.com/dssg-pt/covid19pt-data"
                     )
              )
            )
    )
  )
)

server <- function(input, output) {
  
  ####Councils plot####
  observeEvent(input$location_council,{
    
    location = input$location_council
    Council_Cases = Council_Cases[,c("data",location)]
    date=Council_Cases$data
    date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Council_Cases[,c(location)]
    newcases=TotalInfected
    smooth_newcases=newcases
    smooth_newcases_2=newcases
    rollingaverage=7
    newcases[1]=0
    smooth_newcases[1]=0
    
    for (i in 1:(length(TotalInfected)-1)){
      newcases[i+1] = max(TotalInfected[i+1]-TotalInfected[i],0)
      newcases[is.na(newcases)]=0
      newcases[newcases<0]=0
      smooth_newcases[i+1] = max((TotalInfected[i+1]-TotalInfected[max(1,i-rollingaverage-1)])/min(i,rollingaverage),0)
    }
    
    plotdata_council=data.frame(date,smooth_newcases,newcases)
    
    output$newcasesplot_council <- renderPlot({
      p <- ggplot(plotdata_council,aes(x=date)) +
        geom_line(aes(y=smooth_newcases), size = 2) +
        geom_point(aes(y=newcases), size = 3) +
        ggtitle(paste0("New daily cases for ",location,"\nTotal cases: ",TotalInfected[length(TotalInfected)])) + 
        xlab ("") + ylab("Number of daily cases") + ylim(0, max(20,max(newcases,smooth_newcases))) + 
        theme(axis.text=element_text(size=14,face="bold"),
              axis.title=element_text(size=16,face="bold"),
              aspect.ratio = 1/1.618)
      p
    })
  })
  
  ####Regions plot####
  # observeEvent(input$location_region,{
  
  maxnewcases= 0
  for (k in Regionlist){
    location = k
    Region_Cases_k = Region_Cases[,c("data",location)]
    date=Region_Cases_k$data
    # print(Region_Cases_k)
    date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Region_Cases_k[,c(location)]
    newcases=TotalInfected
    smooth_newcases=newcases
    smooth_newcases_2=newcases
    rollingaverage=7
    newcases[1]=0
    smooth_newcases[1]=0
    
    for (i in 1:(length(TotalInfected)-1)){
      newcases[i+1] = max(TotalInfected[i+1]-TotalInfected[i],0)
      newcases[is.na(newcases)]=0
      newcases[newcases<0]=0
      smooth_newcases[i+1] = max((TotalInfected[i+1]-TotalInfected[max(1,i-rollingaverage-1)])/min(i,rollingaverage),0)
    }
    maxnewcases=max(maxnewcases,smooth_newcases)
    
    
    plotdata_region=data.frame(date,smooth_newcases,newcases)
    
    colour = if(smooth_newcases[length(smooth_newcases)] < 10){
      "#52854C"
    } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 20){
      "#E7B800"
    } else {
      "#FC4E07"
    }
    
    
    p <- ggplot(plotdata_region,aes(x=date)) +
      geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
      # geom_point(aes(y=newcases), size = 3, color=colour) +
      ggtitle(paste0("New daily cases for ",k,"\nTotal cases: ",TotalInfected[length(TotalInfected)])) +
      xlab ("") + ylab("Number of daily cases") +
      theme(axis.text=element_text(size=14,face="bold"),
            axis.title=element_text(size=16,face="bold"),
            aspect.ratio = 1/1.618)
    if (k == "Norte"){
      p1 = p
    } else if (k == "Centro"){
      p2 = p
    } else if (k == "Lisboa e Vale do Tejo"){
      p3 = p
    } else if (k == "Alentejo"){
      p4 = p
    } else if (k == "Algarve"){
      p5 = p
    } else if (k == "Acores"){
      p6 = p
    } else if (k == "Madeira"){
      p7 = p
    }
    # p
    # nam=paste("p",k,sep = "")
    # assign(nam,p)
    
  }
  output$newcasesplot_region <- renderPlot({
    # p1 = p1 + ylim(0, maxnewcases)
    # p2 = p2 + ylim(0, maxnewcases)
    # p3 = p3 + ylim(0, maxnewcases)
    # p4 = p4 + ylim(0, maxnewcases)
    # p5 = p5 + ylim(0, maxnewcases)
    # p6 = p6 + ylim(0, maxnewcases)
    # p7 = p7 + ylim(0, maxnewcases)
    
    multiplot(p1, p2, p3, p4, p5, p6, p7, cols=4)
  })
  # })
}

shinyApp(ui, server)