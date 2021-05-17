
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(ggplot2)


countries<-read.csv("countries.csv")

shinyServer(function(input, output) {
    download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "covid.csv")
    covid<-read.csv("covid.csv") %>% mutate(date= as.Date(date, "%Y-%m-%d"))
    output$today<-renderText(format(max(covid$date)))
    covid<-covid %>% select(location, date, All=people_vaccinated_per_hundred,
                            Fully=people_fully_vaccinated_per_hundred, population) %>%
        filter(location %in% countries$x, date>as.Date("2020-12-14"), date<max(date),
                  !is.na(population))
    world<-group_by(covid,location) %>% summarize(pop=mean(population))
    world<-sum(world$pop)
    c<-list()
    clong<-list()
    for (state in countries$x) {
        c[[state]]<-filter(covid, location==state)
    }
    for(state in names(c)) {
        if(is.na(c[[state]]$All[1])) {c[[state]]$All[1]<-0}
        if(is.na(c[[state]]$Fully[1])) {c[[state]]$Fully[1]<-0}
        for(i in 2:length(c[[state]]$All)) {
            if(is.na(c[[state]]$All[i])) {c[[state]]$All[i]<-c[[state]]$All[i-1]}
            if(is.na(c[[state]]$Fully[i])) {c[[state]]$Fully[i]<-c[[state]]$Fully[i-1]}
        }
        clong[[state]]<-melt(c[[state]], measure.vars = c("All", "Fully"), variable.name="doses")
    }
    data<-bind_rows(c)
    data<-mutate(data, peo1=All*population/100, peo2=Fully*population/100)
    meanvax<-group_by(data, date) %>%
        summarize(All=round(sum(peo1)/world*100,2),Fully=round(sum(peo2)/world*100,2)) %>%
        melt(measure.vars = c("All", "Fully"), variable.name = "doses") %>% 
        mutate(location="World", type=doses, percentage=value, People= round(value/100*world))
    meanv<-reactive({filter(meanvax, between(date, input$dates[1], input$dates[2]),
                            doses %in% input$doses)})
    datalong<-bind_rows(clong)
    a<-reactive({
        if(input$world) countries$x
        else input$where
    })
    usedata<-reactive({filter(datalong, location %in% a(), doses %in% input$doses,
                              between(date,input$dates[1], input$dates[2])) %>%
            mutate(type=doses, percentage=value, d=date, loc=location, People=population/100*value)})
    output$data<-renderDataTable(
        if(dim(usedata())[1]==0) {d<-mutate(data.frame(),location="", date="", People_All_doses="", "All %"="",
                                         People_fully_vaccinated="", "Fully %"="")
        d}
        else {d<-dcast(usedata(),location + date + population ~ doses, value.var="value")
            if("All" %in% usedata()$doses) {d<-mutate(d,People_All_doses=round(population/100*All),
                                                      "All %"=All) %>% select(-All)}
            if("Fully" %in% usedata()$doses) {d<-mutate(d,People_fully_vaccinated=round(population/100*Fully),
                                                        "Fully %"=Fully) %>% select(-Fully)} %>%
                select(-population)
        d
        }
        )
    output$wdata<-renderDataTable(
        if(dim(meanv())[1]==0) {mutate(data.frame(),location="", date="", People_All_doses="", "All %"="",
                                           People_fully_vaccinated="", "Fully %"="")}
        else {e<-dcast(meanv(),location + date ~ doses, value.var="value")
            if("All" %in% meanv()$doses) {e<-mutate(e,People_All_doses=round(world/100*All), 
                                                    "All %"=All) %>% select(-All)}
            if("Fully" %in% meanv()$doses) {e<-mutate(e,People_fully_vaccinated=round(world/100*Fully),
                                                      "Fully %"=Fully) %>% select(-Fully)}
        e
        }
        )
    w<-reactive({
        if(length(input$doses)==0) {ggplot(meanv(), aes(x=date, y=value))}
        else {ggplot(meanv(), aes(x=date, y=value, group=doses, linetype=doses, col=location,
                                  label=percentage, label2=type, label3=People)) +geom_line(
                               cex=1.5)+ geom_area(position="identity",alpha=.3) + theme(legend.title = element_blank(
                               ), axis.text.x = element_text(
                                   angle=90))+ ylab("% Vaccinated population")}
        })
    
    
    output$prova<-renderPlotly({
        if(dim(usedata())[1]==0) {ggplotly(w(), tooltip=c("location", "date", "percentage", "type", "People"), dynamicTicks=T)
        }
        else {
        ggplotly(
        w()+geom_line(data=usedata(), mapping=aes(x=d, y=value,
                                                group=paste(doses, loc),
                                                                 linetype=doses, col=loc)), 
        tooltip=c("location", "date", "percentage", "type", "People"),dynamicTicks=T)
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Countries_data_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(select(usedata(),-type,-loc,-d,-value), file)
        }
    )
    
    output$downloadDataW <- downloadHandler(
        filename = function() {
            paste("World_data_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(select(meanv(),-value, -type), file)
        }
    )

})
