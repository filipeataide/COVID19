library(readr)  # for read_csv
library(knitr)  # for kable
library(RCurl)
library(stringr)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

rm(list = ls())
invisible(gc)
####COVID data####
dataPT_councils <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos.csv")
dataPT_regions <- getURL("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")
dataIT_country <- getURL("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
dataBR_country <- getURL("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv")
data_country <- getURL("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

####Demographics data####
# populationPortugal <- getURL("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")
# populationPortugal <- read.csv(text=populationPortugal)
# populationPortugal <- populationPortugal %>% as.data.frame() %>% group_by(Country.Name) %>% filter(Country.Name == "Portugal") %>% filter(Year == max(Year))
# populationPortugal <- populationPortugal$Value


####Data structuring####
Council_Cases <- read.csv(text=dataPT_councils)
Councillist=colnames(Council_Cases)
Councillist=Councillist[2:length(Councillist)]
Councillist=str_replace_all(Councillist, "\\.", " ")
Councillist=str_to_title(Councillist)
colnames(Council_Cases)=c("data",Councillist)
todayCouncil=Council_Cases$data[length(Council_Cases$data)]
todayCouncil=as.Date(todayCouncil,"%d-%m-%y")

Region_Cases <- read.csv(text=dataPT_regions)
Region_Deaths = Region_Cases[,c("data","obitos_arsnorte","obitos_arscentro","obitos_arslvt","obitos_arsalentejo","obitos_arsalgarve","obitos_acores","obitos_madeira")]
Region_Recovered = Region_Cases[,c("data","recuperados_arsnorte","recuperados_arscentro","recuperados_arslvt","recuperados_arsalentejo","recuperados_arsalgarve","recuperados_acores","recuperados_madeira")]
Region_Cases = Region_Cases[,c("data","confirmados_arsnorte","confirmados_arscentro","confirmados_arslvt","confirmados_arsalentejo","confirmados_arsalgarve","confirmados_acores","confirmados_madeira")]
Regionlist = c("Norte","Centro","Lisboa e Vale do Tejo","Alentejo","Algarve","Acores","Madeira")
RegionPopulation = c(3689173,2327026,3447173,758739,451006,246772,289000)
RegionAreaKm2 = c(21278,28462,11930,31551.2,4996.80,2351,801)
RegionDensity = as.data.frame(RegionPopulation/RegionAreaKm2)
RegionDensity$Region = Regionlist

colnames(RegionDensity)=c("density","region")
colnames(Region_Cases)=c("data",Regionlist)
colnames(Region_Deaths)=c("data",Regionlist)
colnames(Region_Recovered)=c("data",Regionlist)

Country_Cases <- read.csv(text=dataPT_regions)
Country_Deaths = Country_Cases[,c("data","obitos")]
# Country_Recovered = Country_Cases[,c("data","recuperados")]
# Country_Admitted = Country_Cases[,c("data","internados")]
# Country_NewAdmitted = Country_Cases[,c("data","internados")]
# Country_Admitted_ICU = Country_Cases[,c("data","internados_uci")]
Country_Cases_Density = Country_Cases[,c("data","confirmados")]
Country_Cases_Population = Country_Cases[,c("data","confirmados")]
Country_data = Country_Cases[,c("data","obitos","recuperados","internados","internados_uci","confirmados")]
Countrylist = c("Portugal")
Country_Cases = Country_Cases[,c("data","confirmados")]
Country_Cases <- read.csv(text=data_country)
Country_Deaths = Country_Cases[,c("location","date","total_deaths")]
Country_Cases_Density = Country_Cases[,c("location","date","total_cases")]
Country_Cases_Population = Country_Cases[,c("location","date","total_cases")]
Country_Cases = Country_Cases[,c("location","date","total_cases")]
colnames(Country_Deaths)=c("location","date","number")
colnames(Country_Cases_Density)=c("location","date","number")
colnames(Country_Cases_Population)=c("location","date","number")
colnames(Country_Cases)=c("location","date","number")

# Country_data = read.csv(text=data_country)
# Country_data <- as.data.frame(Country_data) %>% select(location,date,total_cases,total_deaths)
countries = read.csv(text=data_country)
countries = unique(countries$location)

# colnames(Country_Cases)=c("data",Countrylist)
# colnames(Country_Deaths)=c("data",Countrylist)
# colnames(Country_Recovered)=c("data",Countrylist)
# colnames(Country_Admitted)=c("data",Countrylist)
# colnames(Country_NewAdmitted)=c("data",Countrylist)
# colnames(Country_Admitted_ICU)=c("data",Countrylist)
# colnames(Country_Cases_Density)=c("data",Countrylist)
# colnames(Country_Cases_Population)=c("data",Countrylist)
# Country_Cases_Density$Portugal = Country_Cases_Density$Portugal/CountryDensity
# Country_data$active = Country_data$confirmados - Country_data$obitos - Country_data$recuperados
# Country_Active = Country_Cases-Country_Deaths-Country_Recovered
# Country_Active$data = Country_Cases$data

todayRegion=Region_Cases$data[length(Region_Cases$data)]
todayRegion=as.Date(todayRegion,"%d-%m-%y")

lockdown_start = "18-03-2020"
lockdown_start = as.Date(lockdown_start,"%d-%m-%y")
lockdown_end = "03-05-2020"
lockdown_end = as.Date(lockdown_end,"%d-%m-%y")

Color_bounds= c("Daily infections",
                "Daily infections (normalized by population density)",
                "Daily infections (per 100.000 habitants)",
                "Daily deaths")#,
# "Active infections",
# "Daily hospitalizations",
                # "Active hospitalizations")
Color_bounds=as.data.frame(Color_bounds)
Color_bounds$green=c(100,1,1,100)
Color_bounds$yellow=c(200,2,2,200)
colnames(Color_bounds)=c("type","green","yellow")
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
                       radioButtons("type_case_country", "Type:",
                                    Color_bounds$type,
                                    selected = "Daily infections"),
                       selectInput("country", "Country:",
                                   countries,
                                   selected = "Portugal", selectize = TRUE),
                       plotOutput("newcasesplot_country", height = "600px"),
                       paste0("Source: https://github.com/dssg-pt/covid19pt-data (last update: ",todayRegion,")\n Inspiration from @yaneerbaryam")
                     ),
                     tabPanel(
                       "Region data",
                       textOutput("legend1"),
                       radioButtons("type_case", "Type:",
                                    c("Daily infections","Daily deaths"),
                                    selected = "Daily infections"),
                       plotOutput("newcasesplot_region", height = "1200px"),
                       paste0("Source: https://github.com/dssg-pt/covid19pt-data (last update: ",todayRegion,")\n Inspiration from @yaneerbaryam")
                     ),
                     tabPanel(
                       "Council data",
                       selectInput("location_council", "Council:",
                                   Councillist,
                                   selected = "Lisboa", selectize = TRUE),
                       "Daily infections below 5 - green; Daily infections below 10 or below 20% of peak - yellow; anything else - red",
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
    paste0("Daily infections below 10 - green; Daily infections below 20 or below 20% of peak - yellow\n anything else - red")
  })
  
  ####Councils plot####
  observeEvent(input$location_council,{
    
    location = input$location_council
    Council_Cases = Council_Cases[,c("data",location)]
    date=Council_Cases$data
    date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Council_Cases[,c(location)]
    TotalInfected[is.na(TotalInfected)]=0
    newcases=TotalInfected
    smooth_newcases=newcases
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
        ggtitle(paste0(location," (total infections: ",TotalInfected[length(TotalInfected)],")")) +
        xlab ("") + ylab("Daily infections") + ylim(0, max(smooth_newcases,5)) +
        theme(axis.text=element_text(face="bold"),
              axis.title=element_text(face="bold"),
              aspect.ratio = 1/1.618,
              plot.title = element_text(face = "bold"))
      p
    })
  })
  
  ####Regions plot####
  observeEvent(input$type_case,{
    
    maxnewcases= 0
    for (k in Regionlist){
      location = k
      if (input$type_case == "Daily infections"){
        Region_Cases_k = Region_Cases[,c("data",location)]
      } else if(input$type_case == "Daily deaths"){
        Region_Cases_k = Region_Deaths[,c("data",location)]
      }
      
      date=Region_Cases_k$data
      date=as.Date(x=date,"%d-%m-%y")
      TotalInfected = Region_Cases_k[,c(location)]
      newcases=TotalInfected
      smooth_newcases=newcases
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
      
      if (input$type_case == "Daily infections"){
        colour = if(smooth_newcases[length(smooth_newcases)] < 10){
          "#52854C"
        } else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < 20){
          "#E7B800"
        } else {
          "#FC4E07"
        }
        p <- ggplot(plotdata_region,aes(x=date)) +
          geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
          ggtitle(paste0(location," (total infections: ",TotalInfected[length(TotalInfected)],")")) +
          xlab ("") + ylab("Daily infections") + ylim(0, max(smooth_newcases,10)) +
          annotate(
            geom = "curve",
            x = lockdown_start,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)],
            xend = lockdown_start-2,
            yend = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)]+maxnewcases*0.05, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = lockdown_start-2,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)]+maxnewcases*0.06,
            label = "Lockdown start",
            hjust = "right") +
          annotate(
            geom = "curve",
            x = lockdown_end,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)],
            xend = lockdown_end+2,
            yend = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)]+maxnewcases*0.05, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = lockdown_end+2,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)]+maxnewcases*0.06,
            label = "Lockdown end",
            hjust = "left") +
          theme(axis.text=element_text(face="bold"),
                axis.title=element_text(face="bold"),
                aspect.ratio = 1/1.618,
                plot.title = element_text(face = "bold"))
      } else if(input$type_case == "Daily deaths"){
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
          annotate(
            geom = "curve",
            x = lockdown_start,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)],
            xend = lockdown_start-2,
            yend = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)]+maxnewcases*0.05, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = lockdown_start-2,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_start)]+maxnewcases*0.06,
            label = "Lockdown start",
            hjust = "right") +
          annotate(
            geom = "curve",
            x = lockdown_end,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)],
            xend = lockdown_end+2,
            yend = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)]+maxnewcases*0.05, 
            curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
          annotate(
            geom = "text",
            x = lockdown_end+2,
            y = plotdata_region$smooth_newcases[which(plotdata_region$date == lockdown_end)]+maxnewcases*0.06,
            label = "Lockdown end",
            hjust = "left") +
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
      multiplot(p1, p2, p3, p4, p5, p6, p7, cols=3)
    })
  })
  
  ####Country plot####
  observeEvent(input$type_case_country,{
    
    maxnewcases= 0
    countrySelected = input$country#"Portugal"
    if (input$type_case_country == "Daily infections"){
      Country_Cases_k = Country_Cases[which(Country_Cases$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily infections (normalized by population density)"){
      Country_Cases_k = Country_Cases_Density[which(Country_Cases_Density$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily infections (per 100.000 habitants)"){
      Country_Cases_k = Country_Cases_Population[which(Country_Cases_Population$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily deaths"){
      Country_Cases_k = Country_Deaths[which(Country_Deaths$location==countrySelected),c("date","number")]
         }
    date=Country_Cases_k$date
    # date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Country_Cases_k$number
    TotalInfected[is.na(TotalInfected)]=0
    newcases=TotalInfected
    smooth_newcases=newcases
    rollingaverage=7
    newcases[1]=0
    smooth_newcases[1]=0
    
    for (i in 1:(length(TotalInfected)-1)){
      newcases[i+1] = max(TotalInfected[i+1]-TotalInfected[i],0)
      newcases[is.na(newcases)]=0
      newcases[newcases<0]=0
      if(input$type_case_country == "Daily infections" || input$type_case_country == "Daily infections (normalized by population density)" || input$type_case_country == "Daily infections (per 100.000 habitants)" || input$type_case_country == "Daily deaths"){
        smooth_newcases[i+1] = max((TotalInfected[i+1]-TotalInfected[max(1,i-rollingaverage-1)])/min(i,rollingaverage),0)
      } else {
        smooth_newcases[i+1] = sum(TotalInfected[max(1,i-rollingaverage-1):(i+1)])/min(i,rollingaverage)
      }
    }
    smooth_newcases[is.na(smooth_newcases)]=0
    if(input$type_case_country == "Daily infections (normalized by population density)"){
      smooth_newcases = smooth_newcases/CountryDensity
    }
    if(input$type_case_country == "Daily infections (per 100.000 habitants)"){
      smooth_newcases = smooth_newcases/CountryPopulation*100000
    }
    maxnewcases=max(maxnewcases,smooth_newcases)
    
    plotdata_country=data.frame(date,smooth_newcases,newcases)

    colour = if(smooth_newcases[length(smooth_newcases)] < Color_bounds$green[which(Color_bounds$type==input$type_case_country)]){
      "#52854C"
    }
    else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < Color_bounds$yellow[which(Color_bounds$type==input$type_case_country)]){
      "#E7B800"
    }
    else {
      "#FC4E07"
    }

    output$newcasesplot_country <- renderPlot({
      p <- ggplot(plotdata_country,aes(x=as.Date(date))) +
        geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
        ggtitle(paste0(countrySelected)) +
        xlab ("") + ylab(input$type_case_country) + ylim(0, max(smooth_newcases,10)) +
                theme(axis.text=element_text(face="bold"),
              axis.title=element_text(face="bold"),
              aspect.ratio = 1/1.618,
              plot.title = element_text(face = "bold"))
      p
      # annotate(
      #   geom = "curve",
      #   x = lockdown_start,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)],
      #   xend = lockdown_start-2,
      #   yend = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)]+maxnewcases*0.05, 
      #   curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
      # annotate(
      #   geom = "text",
      #   x = lockdown_start-2,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)]+maxnewcases*0.06,
      #   label = "Lockdown start",
      #   hjust = "right") +
      # annotate(
      #   geom = "curve",
      #   x = lockdown_end,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)],
      #   xend = lockdown_end+2,
      #   yend = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)]+maxnewcases*0.05, 
      #   curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
      # annotate(
      #   geom = "text",
      #   x = lockdown_end+2,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)]+maxnewcases*0.06,
      #   label = "Lockdown end",
      #   hjust = "left") +
      
    })
  })
  observeEvent(input$country,{
    
    maxnewcases= 0
    countrySelected = input$country#"Portugal"
    population <- read.csv(text=data_country) 
    population$date <- as.Date(population$date)
    population <- population %>% 
      as.data.frame() %>% filter(location == countrySelected) %>% 
      select(date,population) %>% filter(date == max(date)) %>% select(population) 
    population <- population$population
    populationdensity <- read.csv(text=data_country) 
    populationdensity$date <- as.Date(populationdensity$date)
    populationdensity <- populationdensity %>% 
      as.data.frame() %>% filter(location == countrySelected) %>% 
      select(date,population_density) %>% filter(date == max(date)) %>% select(population_density) 
    populationdensity <- populationdensity$population_density
    print(populationdensity)
    CountryDensity = populationdensity
    CountryPopulation = population
    if (input$type_case_country == "Daily infections"){
      Country_Cases_k = Country_Cases[which(Country_Cases$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily infections (normalized by population density)"){
      Country_Cases_k = Country_Cases_Density[which(Country_Cases_Density$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily infections (per 100.000 habitants)"){
      Country_Cases_k = Country_Cases_Population[which(Country_Cases_Population$location==countrySelected),c("date","number")]
    } else if(input$type_case_country == "Daily deaths"){
      Country_Cases_k = Country_Deaths[which(Country_Deaths$location==countrySelected),c("date","number")]
    }
    date=Country_Cases_k$date
    # date=as.Date(x=date,"%d-%m-%y")
    TotalInfected = Country_Cases_k$number
    TotalInfected[is.na(TotalInfected)]=0
    newcases=TotalInfected
    smooth_newcases=newcases
    rollingaverage=7
    newcases[1]=0
    smooth_newcases[1]=0
    
    for (i in 1:(length(TotalInfected)-1)){
      newcases[i+1] = max(TotalInfected[i+1]-TotalInfected[i],0)
      newcases[is.na(newcases)]=0
      newcases[newcases<0]=0
      if(input$type_case_country == "Daily infections" || input$type_case_country == "Daily infections (normalized by population density)" || input$type_case_country == "Daily infections (per 100.000 habitants)" || input$type_case_country == "Daily deaths"){
        smooth_newcases[i+1] = max((TotalInfected[i+1]-TotalInfected[max(1,i-rollingaverage-1)])/min(i,rollingaverage),0)
      } else {
        smooth_newcases[i+1] = sum(TotalInfected[max(1,i-rollingaverage-1):(i+1)])/min(i,rollingaverage)
      }
    }
    smooth_newcases[is.na(smooth_newcases)]=0
    if(input$type_case_country == "Daily infections (normalized by population density)"){
      smooth_newcases = smooth_newcases/CountryDensity
    }
    if(input$type_case_country == "Daily infections (per 100.000 habitants)"){
      smooth_newcases = smooth_newcases/CountryPopulation*100000
    }
    maxnewcases=max(maxnewcases,smooth_newcases)
    
    plotdata_country=data.frame(date,smooth_newcases,newcases)

    colour = if(smooth_newcases[length(smooth_newcases)] < Color_bounds$green[which(Color_bounds$type==input$type_case_country)]){
      "#52854C"
    }
    else if(smooth_newcases[length(smooth_newcases)]/max(smooth_newcases)<0.2 || smooth_newcases[length(smooth_newcases)] < Color_bounds$yellow[which(Color_bounds$type==input$type_case_country)]){
      "#E7B800"
    }
    else {
      "#FC4E07"
    }

    output$newcasesplot_country <- renderPlot({
      p <- ggplot(plotdata_country,aes(x=as.Date(date))) +
        geom_line(aes(y=smooth_newcases), size = 2, color=colour) +
        ggtitle(paste0(countrySelected)) +
        xlab ("") + ylab(input$type_case_country) + ylim(0, max(smooth_newcases,10)) +
        theme(axis.text=element_text(face="bold"),
              axis.title=element_text(face="bold"),
              aspect.ratio = 1/1.618,
              plot.title = element_text(face = "bold"))
      p
      # annotate(
      #   geom = "curve",
      #   x = lockdown_start,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)],
      #   xend = lockdown_start-2,
      #   yend = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)]+maxnewcases*0.05, 
      #   curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
      # annotate(
      #   geom = "text",
      #   x = lockdown_start-2,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_start)]+maxnewcases*0.06,
      #   label = "Lockdown start",
      #   hjust = "right") +
      # annotate(
      #   geom = "curve",
      #   x = lockdown_end,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)],
      #   xend = lockdown_end+2,
      #   yend = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)]+maxnewcases*0.05, 
      #   curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
      # annotate(
      #   geom = "text",
      #   x = lockdown_end+2,
      #   y = plotdata_country$smooth_newcases[which(plotdata_country$date == lockdown_end)]+maxnewcases*0.06,
      #   label = "Lockdown end",
      #   hjust = "left") +
      
    })
  })
}

shinyApp(ui, server)