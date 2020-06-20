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
todayCouncil=Council_Cases$data[length(Council_Cases$data)]
todayCouncil=as.Date(todayCouncil,"%d-%m-%y")

Region_Cases <- read.csv(text=fileRegions)
Region_Deaths = Region_Cases[,c("data","obitos_arsnorte","obitos_arscentro","obitos_arslvt","obitos_arsalentejo","obitos_arsalgarve","obitos_acores","obitos_madeira")]
Region_Recovered = Region_Cases[,c("data","recuperados_arsnorte","recuperados_arscentro","recuperados_arslvt","recuperados_arsalentejo","recuperados_arsalgarve","recuperados_acores","recuperados_madeira")]
Region_Cases = Region_Cases[,c("data","confirmados_arsnorte","confirmados_arscentro","confirmados_arslvt","confirmados_arsalentejo","confirmados_arsalgarve","confirmados_acores","confirmados_madeira")]
Regionlist = c("Norte","Centro","Lisboa e Vale do Tejo","Alentejo","Algarve","Acores","Madeira")
colnames(Region_Cases)=c("data",Regionlist)
colnames(Region_Deaths)=c("data",Regionlist)
colnames(Region_Recovered)=c("data",Regionlist)

Country_Cases <- read.csv(text=fileRegions)
Country_Deaths = Country_Cases[,c("data","obitos")]
Country_Recovered = Country_Cases[,c("data","recuperados")]
Country_Cases = Country_Cases[,c("data","confirmados")]
Countrylist = c("Portugal")
colnames(Country_Cases)=c("data",Countrylist)
colnames(Country_Deaths)=c("data",Countrylist)
colnames(Country_Recovered)=c("data",Countrylist)

Country_Active = Country_Cases-Country_Deaths-Country_Recovered
Country_Active$data=Country_Cases$data

todayRegion=Region_Cases$data[length(Region_Cases$data)]
todayRegion=as.Date(todayRegion,"%d-%m-%y")

confinamentostart = "18-03-2020"
confinamentostart = as.Date(confinamentostart,"%d-%m-%y")
confinamentoend = "03-05-2020"
confinamentoend = as.Date(confinamentoend,"%d-%m-%y")

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
                       "Country data",
                       selectInput("type_case_country", "Type:",
                                   c("Total","Deaths","Active"),
                                   selected = "Total"),
                       plotOutput("newcasesplot_country", height = "600px"),
                       paste0("Source: https://github.com/dssg-pt/covid19pt-data (last update: ",todayRegion,")\n Inspiration from @yaneerbaryam")
                     ),
                     tabPanel(
                       "Region data",
                       textOutput("legend1"),
                       selectInput("type_case", "Type:",
                                   c("Total","Deaths"),
                                   selected = "Total"),
                       plotOutput("newcasesplot_region", height = "600px"),
                       paste0("Source: https://github.com/dssg-pt/covid19pt-data (last update: ",todayRegion,")\n Inspiration from @yaneerbaryam")
                     ),
                     tabPanel(
                       "Council data",
                       selectInput("location_council", "Council:",
                                   Councillist,
                                   selected = "Lisboa", selectize = TRUE),
                       "Daily cases below 5 - green; Daily cases below 10 or below 20% of peak - yellow; anything else - red",
                       plotOutput("newcasesplot_council"),
                       paste0("Source: https://github.com/dssg-pt/covid19pt-data (last update: ",todayCouncil,")\n Inspiration from @yaneerbaryam")
                     )
              )
            )
    )
  )
)

server <- function(input, output) {
  
  output$legend1 = renderText({
    paste0("Daily cases below 10 - green; Daily cases below 20 or below 20% of peak - yellow\n anything else - red")
  })
  
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
    colour = if(smooth_newcases[length(smooth_newcases)] < 5){
      "#52854C"
    } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 10){
      "#E7B800"
    } else {
      "#FC4E07"
    }
    
    output$newcasesplot_council <- renderPlot({
      p <- ggplot(plotdata_council,aes(x=date)) +
        geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
        ggtitle(paste0(location," (total cases: ",TotalInfected[length(TotalInfected)],")")) +
        xlab ("") + ylab("Daily cases") + ylim(0, max(smooth_newcases,5)) +
        theme(axis.text=element_text(face="bold"),
              axis.title=element_text(face="bold"),
              aspect.ratio = 1/1.618,
              plot.title = element_text(face = "bold"))
      p
    })
  })
  
  ####Regions plot####
  observeEvent(input$type_case,{
    
    # print(Region_Cases_k)
    maxnewcases= 0
    for (k in Regionlist){
      location = k
      if (input$type_case == "Total"){
        Region_Cases_k = Region_Cases[,c("data",location)]
      } else if(input$type_case == "Deaths"){
        Region_Cases_k = Region_Deaths[,c("data",location)]
      }
      
      date=Region_Cases_k$data
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
      
      
      
      
      if (input$type_case == "Total"){
        colour = if(smooth_newcases[length(smooth_newcases)] < 10){
          "#52854C"
        } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 20){
          "#E7B800"
        } else {
          "#FC4E07"
        }
        p <- ggplot(plotdata_region,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
          ggtitle(paste0(location," (total cases: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Daily cases") + ylim(0, max(smooth_newcases,10)) +
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
      } else if(input$type_case == "Deaths"){
        colour = if(smooth_newcases[length(smooth_newcases)] < 1){
          "#52854C"
        } else if(smooth_newcases[length(smooth_newcases)] < 2){
          "#E7B800"
        } else {
          "#FC4E07"
        }
        p <- ggplot(plotdata_region,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +ggtitle(paste0(location," (total deaths: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Daily deaths") + ylim(0, max(smooth_newcases,1)) +
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
      }
      
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
    }
    output$newcasesplot_region <- renderPlot({
      multiplot(p1, p2, p3, p4, p5, p6, p7, cols=4)
    })
  })
  
  ####Country plot####
  observeEvent(input$type_case_country,{
    
    # print(Region_Cases_k)
    maxnewcases= 0
    # for (k in Countrylist){
    location = "Portugal"
    if (input$type_case_country == "Total"){
      Country_Cases_k = Country_Cases[,c("data",location)]
    } else if(input$type_case_country == "Deaths"){
      Country_Cases_k = Country_Deaths[,c("data",location)]
    } else if(input$type_case_country == "Active"){
      Country_Cases_k = Country_Active[,c("data",location)]
    }
    
    date=Country_Cases_k$data
    date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Country_Cases_k[,c(location)]
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
      if(input$type_case_country != "Active"){
        smooth_newcases[i+1] = max((TotalInfected[i+1]-TotalInfected[max(1,i-rollingaverage-1)])/min(i,rollingaverage),0)
      } else {
        smooth_newcases[i+1] = TotalInfected[i+1]
        }
    }
    maxnewcases=max(maxnewcases,smooth_newcases)
    
    plotdata_country=data.frame(date,smooth_newcases,newcases)
    
    if (input$type_case_country == "Total"){
      colour = if(smooth_newcases[length(smooth_newcases)] < 100){
        "#52854C"
      } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 200){
        "#E7B800"
      } else {
        "#FC4E07"
      }
      output$newcasesplot_country <- renderPlot({
        p <- ggplot(plotdata_country,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
          ggtitle(paste0(location," (total cases: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Daily cases") + ylim(0, max(smooth_newcases,10)) +
          annotate(
            geom = "curve",
            x = confinamentostart,
            y = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentostart)],
            xend = confinamentostart-2,
            yend = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentostart)]+100, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = confinamentostart-2,
            y = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentostart)]+120,
            label = "Lockdown start",
            hjust = "right"
          ) +
          annotate(
            geom = "curve",
            x = confinamentoend,
            y = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentoend)],
            xend = confinamentoend+2,
            yend = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentoend)]+100, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = confinamentoend+2,
            y = plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentoend)]+120,
            label = "Lockdown end",
            hjust = "left"
          )
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
        p
        # print(plotdata_country$smooth_newcases[which(plotdata_country$date == confinamentostart)])
      })
    } else if(input$type_case_country == "Deaths"){
      colour = if(smooth_newcases[length(smooth_newcases)] < 10){
        "#52854C"
      } else if(smooth_newcases[length(smooth_newcases)] < 20){
        "#E7B800"
      } else {
        "#FC4E07"
      }
      output$newcasesplot_country <- renderPlot({
        p <- ggplot(plotdata_country,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +ggtitle(paste0(location," (total deaths: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Daily deaths") + ylim(0, max(smooth_newcases,1)) +
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
        p
      })
    } else if(input$type_case_country == "Active"){
      colour = if(smooth_newcases[length(smooth_newcases)] < 100){
        "#52854C"
      } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 200){
        "#E7B800"
      } else {
        "#FC4E07"
      }
      output$newcasesplot_country <- renderPlot({
        p <- ggplot(plotdata_country,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +ggtitle(paste0(location," (active cases: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Active cases") + ylim(0, max(smooth_newcases,1)) +
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
        p
      })
    }
  })
}

shinyApp(ui, server)