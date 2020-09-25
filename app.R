#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sf)
library(GADMTools)
library(tidyverse)
library(ggiraph)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
new_civ=readRDS("https://github.com/dseka/h-project/blob/master/CIV_adm2.sf.rds")
long=c(-3.8, -6.0, -6.7, -7.0, -3.4, -3.3, -7.4, -7.3, -5.9, -5.3, -5.2,-4.1, -4.3, -4.7, -4.5, -4.6, -3.7, -7.8, -7.2, -7.9, -6.7, -5.8, -6.5, -5.7, -4.7, -5.3, -4.9, -7.45, -5.9, -6.7, -5.0, -3.5, -3.4)
lat=c(5.3, 5.2, 5.9, 5.0, 6.6, 5.4, 10.1, 9.3, 6.2, 5.8, 7.0, 7.5, 6.5, 7.0, 5.9, 5.3, 6.0, 6.3, 6.92, 7.5, 6.8, 6.98, 9.7, 9.4, 9.5, 7.7, 8.5, 8.3, 8.32, 8.25, 6.4, 9.5, 7.95)
ci_covid <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/dseka/h-project/master/covid_ci_region.csv"))
ci_cov=data.frame(ci_covid, long, lat)
n_ci = new_civ %>% left_join(ci_cov, by="NAME_2") %>% slice(1:33)
nciv=st_as_sf(n_ci)

cov_ci <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/dseka/h-project/master/Covid_CI.csv"))
cov_ci <- type_convert(cov_ci,trim_ws=TRUE,col_types = cols(percent_confirmed =col_double()),locale = locale(decimal_mark = ","))

cumul_confirmed<-cov_ci %>% select(6)  %>% slice(1)
cumul_recovered<-cov_ci %>% select(8)  %>% slice(1)
cumul_death<-cov_ci %>% select(10)  %>% slice(1)
ci_active_cases<-cov_ci %>% select(11)  %>% slice(1)

cumul_confirmed
cumul_recovered
cumul_death
ci_active_cases


world_total <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))

world_recovered <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))

world_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))

#owid<-data.table::fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
#af=owid %>% filter(continent=="Africa")
#af_tot_case <- af %>% group_by(location) %>% summarise(tot_cases=last(total_cases)) #%>% select(2) %>% sum()

country_names<-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros", "Congo",  "Cote d'Ivoire", "Democratic Republic of Congo", "Djibouti" , "Egypt" , "Equatorial Guinea",  "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea",  "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique" , "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",  "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara", "Zambia", "Zimbabwe", "Cabo Verde", "Congo (Brazzaville)", "Congo (Kinshasa)" )
af_country<- world_total %>% filter(world_total[,2] %in% country_names)
af_total<- sum(af_country[, ncol(af_country)])
af_recovered<- world_recovered %>% filter(world_recovered[,2] %in% country_names) %>% select(ncol(world_recovered)) %>% sum()
af_deaths<- world_deaths %>% filter(world_deaths[,2] %in% country_names) %>% select(ncol(world_deaths)) %>% sum()
af_active<-af_total - (af_recovered + af_deaths)

world_total[is.na(world_total)]=0
world_recovered[is.na(world_recovered)]=0
world_deaths[is.na(world_deaths)]=0

ci=world_total %>% filter(world_total[, 2]=="Cote d'Ivoire")
ci_tot_case= ci %>% select(ncol(ci))
ci_tot_case<-ifelse(cumul_confirmed > ci_tot_case, cumul_confirmed, ci_tot_case)

cr<- world_recovered %>% filter(world_recovered[, 2]=="Cote d'Ivoire")
cumul_rec <- cr %>% select(ncol(cr))
cumul_rec<-ifelse(cumul_recovered > cumul_rec, cumul_recovered, cumul_rec)
cumul_rec<-as.numeric(as.character(cumul_rec))

cw<- world_deaths %>% filter(world_deaths[, 2]=="Cote d'Ivoire")
cumul_dea<- cw %>% select(ncol(cw))
cumul_dea<-ifelse(cumul_death > cumul_dea, cumul_death, cumul_dea)

cumul_rec<-as.numeric(as.character(cumul_rec))
cumul_dea<-as.numeric(as.character(cumul_dea))
ci_tot_case<-as.numeric(as.character(ci_tot_case))
ci_active_case<- ci_tot_case - (cumul_rec + cumul_dea)
ci_active<- ifelse(ci_active_cases > ci_active_case, ci_active_cases, ci_active_case)

wt <- world_total %>% select(ncol(world_total)) %>% sum()
rec <- world_recovered %>% select(ncol(world_recovered)) %>% sum()
deaths <- world_deaths %>% select(ncol(world_deaths)) %>% sum()
wa = wt - (rec + deaths)

ci_tot_case= format(ci_tot_case, big.mark=" ", scientific=FALSE, trim=TRUE)
cumul_rec<- format(cumul_rec, big.mark=" ", scientific=FALSE, trim=TRUE)
cumul_dea<- format(cumul_dea, big.mark=" ", scientific=FALSE, trim=TRUE)
ci_active<- format(ci_active, big.mark=" ", scientific=FALSE, trim=TRUE)

wt <- format(wt, big.mark=" ", scientific=FALSE, trim=TRUE)
wa <- format(wa, big.mark=" ", scientific=FALSE, trim=TRUE)

af_total <- format(af_total, big.mark=" ", scientific=FALSE, trim=TRUE)
af_active <- format(af_active, big.mark=" ", scientific=FALSE, trim=TRUE)

a=colnames(world_total)
b=a[-c(1:53)]
df <- data.frame(b)
df = as.Date(df$b, "%m/%d/%y")
c=world_total%>% filter(world_total[,2]=="Cote d'Ivoire")
c=c[-c(1:53)]
colnames(c)=NULL
c=as.numeric(c)
c=matrix(c, ncol=1)

d= world_recovered %>%filter(world_recovered[,2]=="Cote d'Ivoire")
d=d[-c(1:53)]
colnames(d)=NULL
d=as.numeric(d)
d=matrix(d, ncol=1)

e = world_deaths %>%filter(world_deaths[,2]=="Cote d'Ivoire")
e=e[-c(1:53)]
colnames(e)=NULL
e=as.numeric(e)
e=matrix(e, ncol=1)


actv=c-(d+e)
actv=matrix(actv, ncol=1)
dt=data.frame(Date=df,Cases= c, Recovered=d, Deaths=e, Active=actv)


c_daily=c[-1]-c[-nrow(c)]
c_daily=c_daily[-c(1:4)]
c_daily[1]=1
c_daily=ifelse(c_daily==0, 1, c_daily)
rate=c_daily[-1]/c_daily[-length(c_daily)]

e_daily=e[-1]-e[-nrow(e)]

e_daily=e[-1]-e[-nrow(e)]

rate_date=dt$Date[-c(1:6)]
rate_dt=data.frame(Rate=rate,Date=rate_date)
e_date=dt$Date[-1]
e_dt=data.frame(Deaths=e_daily, Date=e_date)

nciv$tooltip_point <- c(paste0("Region: ", nciv$NAME_2, "\nCases: ", nciv$active_cases))  

ui <- fluidPage(style="padding:0px",
                setBackgroundColor(color="#000000"),
                tags$style(HTML("
                         .tabbable > .nav > li > a{background-color: #101010;  color:#B1B1B1; border-color:#101010;}
                         .tabbable > .nav > li > a[data-value='t1'] {background-color: #101010;   color:olive}
                         .tabbable > .nav > li > a[data-value='t2'] {background-color: #101010;  color:olive}
                         .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:olive}
                         .tabbable > .nav > li[class=active]> a {background-color: black; color:#B1B1B1; border-color:#101010; border-top-color:aqua;}
                          ")),
                titlePanel(
                    tags$h4("COVID-19 Situation in Cote d'Ivoire by", span(style='color:blue', 'the School of Natural Sciences, University Nangui Abrogoua,'), "Abidjan, Cote d'Ivoire",
                            style="background-color:#212121; padding:10px; text-align:left; 
                             color:#B1B1B1; border: 2px solid #2A2A2A;  border-radius: 1px; font-size:1.5vw;")),
                
                fluidRow(
                    column(width=3, 
                           wellPanel(style="background-color:#212121; padding:10px; text-align:center; color:#777777; height:255px; border-radius: 10px; border: 2px solid #2A2A2A; font-size:1vw;",  
                                     
                                     span(style="font-size:1.7vw; color: #777777;", "World Statistics"), 
                                     hr(style="width:80%; border-color:teal"),
                                     
                                     span(style="font-size:1vw;color: #777777",    "Total Confirmed Cases"), 
                                     
                                     span(style="color:red", textOutput("wt")),
                                     hr(style="width:50%; border-color:teal"),
                                     
                                     span(style="font-size:1vw;color: #777777",    "Active Cases"), 
                                     
                                     span(style="color:red", textOutput("wa")),
                                     hr(style="width:50%; border-color:teal"),
                           ),
                           wellPanel(style="background-color:#212121; padding:10px; text-align:center; color:#B1B1B1; border-radius: 10px; height:385px; border: 2px solid #2A2A2A; font-size:1.6vw;",  
                                     
                                     span(style="font-size:1.7vw; color: #777777;", "Africa Statistics"), 
                                     hr(style="width:80%; border-color:teal"),
                                     
                                     span(style="font-size:1vw;color: #777777",    "Confirmed Cases in Africa"), 
                                     
                                     span(style="color:red", textOutput("af_total")),
                                     hr(style="width:50%; border-color:teal"),
                                     
                                     span(style="font-size:1vw;color: #777777",    "Active Cases in Africa"), 
                                     
                                     span(style="color:red", textOutput("af_active")),
                                     hr(style="width:80%; border-color:teal"),
                                     
                                     
                                     
                                     span(style= "color:lightblue; font-size:1vw; ", "By Dr SEKA Dagou, University Nangui Abrogoua, Abidjan, Cote d'Ivoire ") 
                           )
                           
                    ),
                    
                    column(5,  
                           fluidRow(
                               box(width=3, style="background-color:#212121; padding:10px; 
text-align:center; color:#777777; border-radius: 10px; border: 2px solid #2A2A2A; font-size:1vw; height:90px; ","Total Cases", span(style="color:lime; font-size:1.6vw;", textOutput("ci_tot_case"))),
                               box(width=3, style="background-color:#212121; padding:10px; 
text-align:center; color:#777777; border-radius: 10px; border: 2px solid #2A2A2A;  font-size:1vw; height:90px; ","Recovered", span(style="color:lime; font-size:1.6vw;", textOutput("cumul_rec"))),
                               box(width=3, style="background-color:#212121; padding:10px; 
text-align:center; color:#777777; border-radius: 10px; border: 2px solid #2A2A2A; font-size:1vw; height:90px; ", "Active Cases", span(style="color:lime; font-size:1.6vw;", textOutput("ci_active"))),
                               box(width=3, style="background-color:#212121; padding:10px; 
text-align:center; color:#777777; border-radius: 10px; border: 2px solid #2A2A2A; font-size:1vw; height:90px; ","Deceased", span(style="color:lime; font-size:1.6vw;",
                                                                                                                                 textOutput("cumul_dea")))
                           ),
                           
                           fluidRow(
                               wellPanel(style="background-color:#212121; padding:10px; text-align:left; 
                       color:#B1B1B1; border-radius: 10px; border: 2px solid #2A2A2A; font-size:1vw;",
                                         
                                         tabsetPanel(type="tabs", 
                                                     
                                                     tabPanel("Avtive Cases",
                                                              style="background-color:#ffffff; text-align:left; color:#B1B1B1; font-size:1vw;",
                                                              
                                                              girafeOutput("totcases", width = "100%", height="507px"), style="align:center;"
                                                     ),
                                                     tabPanel("Situation Over Time",    
                                                              style="background-color:#212121;",
                                                              
                                                              plotOutput("plot4", height="245px"),
                                                              
                                                              br(),
                                                              
                                                              plotOutput("plot5", height="245px")
                                                              
                                                     )
                                         )))),
                    
                    column(width=4, 
                           wellPanel(style="background-color:#212121; padding:10px; text-align:center; color:#B1B1B1; border-radius: 10px; font-size:1vw; border: 2px solid #1A1A1A;",  
                                     
                                     span(style="color:aqua; font-size:1.7vw ", "Cases Over Time" ),
                                     hr(style="width:80%; border-color:teal"),
                                     
                                     br(),
                                     
                                     plotOutput("plot1", height="170px"),
                                     
                                     br(),
                                     
                                     plotOutput("plot2", height="170px"),
                                     
                                     br(),
                                     
                                     plotOutput("plot3", height="170px"),
                                     
                                     
                           ))
                    
                    
                )
)

server<-function(input, output){
    
    output$wt<-renderText({wt})
    output$wa<-renderText({wa})
    
    output$af_total<-renderText({af_total})
    output$af_active<-renderText({af_active})
    
    output$ci_tot_case<-renderText({ci_tot_case})
    output$cumul_rec <-renderText({cumul_rec})
    output$ci_active <-renderText({ci_active})
    output$cumul_dea <-renderText({cumul_dea})
    output$totcases<-renderGirafe({
        map_ci=ggplot() +
            geom_sf(data=nciv, fill="#FFDB6D") +
            geom_point_interactive(data=nciv, mapping=aes(x=long, y=lat, group=NAME_2, size=active_cases, tooltip=tooltip_point), fill="red", alpha=0.3, shape=21) + 
            geom_text(data=nciv, size=2, aes(x=long, y=lat, label=NAME_2), color="darkblue", nudge_y=-0.08) +
            labs(x = NULL, y = NULL) +
            theme(panel.background = element_rect(fill = "#ffffff", colour = "#ffffff", size = 1.5, linetype = "solid"), panel.grid.major = element_blank(), panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#ffffff"), plot.margin = margin(0, 0, 0, 0, "cm"),
                  legend.position = "none",
                  panel.spacing.x=unit(0, "lines"),
                  panel.spacing.y=unit(0, "lines"),
                  axis.text = element_blank(), 
                  axis.ticks.length = unit(0, "mm"),
                  axis.line = element_blank(),
                  
                  
                  plot.background = element_rect(fill = "#ffffff", size=0))
        
        mytheme_map <- theme(panel.background = element_rect(fill = "#ffffff"))
        
        ggiraph(code = {print(map_ci + mytheme_map)}, hover_css = "cursor:pointer;", options = list(opts_sizing(rescale = TRUE, width = 1), opts_zoom(max=5), opts_tooltip(offx = 20, offy = -7)
        ))
    })
    
    output$plot1<-renderPlot({
        
        ggplot(dt, aes(x=Date, y=Cases)) +
            geom_line(color="yellow", size=2, alpha=0.9) +
            ggtitle("Cumulative Distribution of Confirmed Cases") +
            theme(
                panel.background = element_rect(fill = "black",  colour = "black"),
                panel.border = element_blank(),
                plot.margin = unit(c(0, 0, 0, 0), "null"), 
                panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),   
                axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                plot.title = element_text(color="#666666", size=14, face="bold", hjust = 0.5),
                axis.title.x = element_text(color="#666666"),
                axis.title.y = element_text(angle = 90, color="#666666"), 
                plot.background = element_rect(fill = "#111111", color="#111111", size=0) )
    }, bg="#B1B1B1", execOnResize=T)
    
    
    output$plot2<-renderPlot({
        
        ggplot(dt, aes(x=Date, y=Recovered)) +
            geom_line(color="#69b3a2", size=2, alpha=0.9) +
            ggtitle("Recovered Patients") +
            theme(
                panel.background = element_rect(fill = "black",  colour = "black"),
                panel.border = element_blank(),
                panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                plot.title = element_text(color="#666666", size=14, face="bold", hjust = 0.5),
                axis.title.x = element_text(color="#666666"),
                axis.title.y = element_text(angle = 90, color="#666666"), 
                plot.background = element_rect(fill = "#111111", color="#111111", size=0)
            )
    }, bg="#B1B1B1", execOnResize=T)
    
    
    output$plot3<-renderPlot({
        ggplot(data=e_dt, aes(x=Date, y=Deaths)) + 
            geom_bar(stat="identity", width=0.1, color="steelblue", 
                     fill="steelblue") +
            
            ggtitle("Number of Deaths ") +
            theme(
                panel.background = element_rect(fill = "black",  colour = "black"),
                panel.border = element_blank(),
                panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                plot.title = element_text(color="#666666", size=14, face="bold", hjust = 0.5),
                axis.title.x = element_text(color="#666666"),
                axis.title.y = element_text(angle = 90, color="#666666"), 
                plot.background = element_rect(fill = "#111111", color="#111111", size=0)
            )
    }, bg="#B1B1B1", execOnResize=T)
    
    
    output$plot4<-renderPlot({
        ggplot(dt, aes(x=Date)) +
            geom_area(aes(y=Cases), fill="#E69F00", color="#E69F00", alpha=0.5) +  
            geom_area(aes(y=Recovered), fill="#333333", color="#333333", alpha=0.5) +
            ggtitle("Evolution of Confirmed Cases and Recovered Patients") +
            theme(
                panel.background = element_rect(fill = "black",  colour = "black"),
                panel.border = element_blank(),
                panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                plot.title = element_text(color="#666666", size=14, face="bold", hjust = 0.5),
                axis.title.x = element_text(color="#666666"),
                axis.title.y = element_text(angle = 90, color="#666666"), 
                plot.background = element_rect(fill = "#111111", color="#111111", size=0)
            )
    }, bg="#B1B1B1", execOnResize=T)
    
    
    
    
    output$plot5<-renderPlot({
        ggplot(dt, aes(x=Date, y=Active)) +
            geom_line(color="chartreuse", size=2, alpha=0.9) +
            ggtitle("Number of Active Cases") +
            theme(
                panel.background = element_rect(fill = "black",  colour = "black"),
                panel.border = element_blank(),
                panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#100111"),
                axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                plot.title = element_text(color="#666666", size=14, face="bold", hjust = 0.5),
                axis.title.x = element_text(color="#666666"),
                axis.title.y = element_text(angle = 90, color="#666666"), 
                plot.background = element_rect(fill = "#111111", color="#111111", size=0)
            )
    }, bg="#B1B1B1", execOnResize=T)
    
}
shinyApp(ui, server)
