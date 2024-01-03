library(shiny)
library(shinydashboard)
library(ggplot2)
library(highcharter)
library(plotly)
library(tidyverse)
library(ggthemes)

joined.ncd.prop <- read.csv("joined.ncd.prop.csv")
ncd.world <- read.csv('ncd.world.csv')
Total.NCD.Deaths.in.thousands <- read.csv("Total.NCD.Deaths.in.thousands.new.csv")
risks <- read.csv("risks.csv")
mortality.rate <- read.csv("adjusted.age.standardized.mortality.rate.csv") 
dalys.avg <- read.csv("dalys.avg.csv")
data<-read.csv("data3.csv",encoding = "UTF-8",na.strings=c(NA,'..'))
data<-select(data,Country.Name=1,Country.Code=2,UHC.service.coverage.index=3,Cause.of.death.by.non.communicable.diseases=4,Current.health.expenditure.per.capita=5,Domestic.general.government.health.expenditure.per.capita=6,Domestic.private.health.expenditure.per.capita=7,External.health.expenditure.per.capita=8)
data1<-na.omit(data)

loc <- c("Africa","Americas","Eastern Mediterranean","Europe","South-East Asia","Western Pacific")
sex <- c("Male","Female")
age <- c("0 to 19","20 to 54","55 to 89")

choice.freq <- c("Never","Maybe sometimes","I usually do","I often do","Cannot live without it") 
choice.level <- c("Low","Below average","Middle","Beyond average","High")

ui <- dashboardPage( skin = "black",
  dashboardHeader(title = 'NCD: Risk and Challenge of Human',titleWidth = 500),
  
  dashboardSidebar(

    sidebarMenu(
      menuItem("Overview", tabName= "overview",icon = icon("bar-chart-o",size =3),
               menuSubItem("Introduction",tabName = "introduction"),
               menuSubItem("Gender",tabName = "gender"),
               menuSubItem("World Map",tabName = "map"),
               menuSubItem("World Death Number",tabName = "death"),
               menuSubItem("Each Disease Number",tabName = "each")
               ),
      menuItem("Policy Expenditure",tabName = "policy",icon=icon("archway"),
               menuSubItem("UHC Service Coverage Index",tabName = "uhc"),
               menuSubItem("Current Health",tabName = "currh"),
               menuSubItem("Domestic Private Health",tabName="domeh"),
               menuSubItem("External Health",tabName = "exh")),
      menuItem("Factors", tabName = "Factors", icon = icon("th"), 
               menuSubItem("GDP",tabName = "gdp"),
               menuSubItem("Age",tabName = "age") ),
      
      menuItem("You and NCD", tabName = "youandNCD", icon = icon("fas fa-user-md"),
               badgeLabel = "Try it!",badgeColor = "green"),
      menuItem("Advice",tabName = "advice",icon = icon("table"))
      
      ))
    ,
  
  dashboardBody(
    tags$head(tags$style(type="text/css", "
  .content{overflow:auto !important; }
 #loadmessage {
top: 0px; left: 0px;
width: 100%; padding: 5px 0px 5px 0px;
text-align: center; font-weight: bold;
font-size: 100%; color: #000003;
background-color: #FFC1C1; z-index: 105;}",HTML(' /* body */
                                .content-wrapper, .right-side {
                                background-color: #f3f9f1 ;
                                }
'))), 
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("loading...",id="loadmessage")),
    
    tabItems(
      tabItem(tabName = "overview",fluidRow(h1("homepage")
        )
      ),
      
      tabItem(
      tabName = "introduction",
              fluidRow(
                # HTML("<script type='text/javascript' src='swiper-bundle.min.js'></script>"),
                # HTML("<link rel='stylesheet' href='../package/swiper-bundle.min.css'>"),
                div( style="width:100%",
                div( style="width:1195px; margin:0 auto",
                # div(class="swiper-container",div(class="swiper-wrapper", div(class = "swiper-slide", img( src="risks.png")),div(class = "swiper-slide", img( src="risks.png")))),
                img( src="risks.png",
                     style="width:100%; padding:0 15px 15px 15px; height:250px"),
                tabBox(title = HTML("<strong>What is NCD?</strong>"),
                       tabPanel("Definition",textOutput('intro.def'))),
                tabBox(title = HTML("<strong>How NCD kills people.</strong>"),
                       tabPanel("Dangerous NCD",textOutput('intro.dangerous'))),
                tabBox(title = HTML("<strong>Common things can contribute to NCD.</strong>"),
                       tabPanel("Risks everywhere",textOutput('intro.risk')),width = 12),
                div(style="padding:20px;width:80%;"),
                div(style="clear:both; overflow:hidden;",
                tabBox(title = HTML("<strong>Our data and brief introduction</strong>"),tabPanel("Brief introduction",
                       textOutput('intro.data'))),
                img( src="images.png",
                     style="width:50%;height:204px"))))
               )),
      
      tabItem(
        tabName = "gender",
              fluidRow( textOutput("p1a"),
                         tags$head(tags$style("#p1a{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                        
                 
                tabBox(title = "Between gender: What may contribute to NCD?",height = "500px", width = 12,
                       tabPanel("Between gender", box(plotlyOutput('Overview_plot_1_m'),width = 12,background = "olive" ))),
                
                htmlOutput("p1b"),
                tags$head(tags$style("#p1b{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }"))
              )),
      
        tabItem( tabName = "map",
              fluidRow(
                textOutput("p2a"),
                tags$head(tags$style("#p2a{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                tabBox(title = "2019: NCD in world",height = "500px", width = 12,
                       tabPanel("World", box(highchartOutput("Overview_plot_2"),width = 12))
                ),

                textOutput("p2b"),
                tags$head(tags$style("#p2b{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }"))
                )),
        
        tabItem( tabName = "death",
              fluidRow(
                
                textOutput("p3a"),
                tags$head(tags$style("#p3a{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                
                tabBox(title = "Evolution of NCD: death number from 2000 to 2019",height = "500px", width = 12,
                       tabPanel("World", box(plotlyOutput('Overview_plot_3'),width = 12,background = "olive"))),
                
               textOutput("p3b"),
               tags$head(tags$style("#p3b{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
      )),
      tabItem( tabName = "uhc",
               fluidRow(
                 
                 textOutput("uhca"),
                 tags$head(tags$style("#uhca{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                 
                 tabBox(title = "NCD and UHC service coverage",height = "500px", width = 12,
                        tabPanel("World", box(plotlyOutput('UHCgraph'),width = 12,background = "olive"))),
                 
                 textOutput("uhcb"),
                 tags$head(tags$style("#uhcb{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
               )),
      tabItem( tabName = "currh",
               fluidRow(
                 
                 textOutput("currha"),
                 tags$head(tags$style("#currha{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                 
                 tabBox(title = "NCD and Current health expenditure",height = "500px", width = 12,
                        tabPanel("World", box(plotlyOutput('currhgraph'),width = 12,background = "olive"))),
                 
                 textOutput("currhb"),
                 tags$head(tags$style("#currhb{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
               )),
      tabItem( tabName = "domeh",
               fluidRow(
                 
                 textOutput("domeha"),
                 tags$head(tags$style("#domeha{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                 
                 tabBox(title = "NCD and Domestic private health expenditure",height = "500px", width = 12,
                        tabPanel("World", box(plotlyOutput('domehgraph'),width = 12,background = "olive"))),
                 
                 textOutput("domehb"),
                 tags$head(tags$style("#domehb{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
               )),
      tabItem( tabName = "exh",
               fluidRow(
                 
                 textOutput("exha"),
                 tags$head(tags$style("#exha{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                 
                 tabBox(title = "NCD and External health expenditure",height = "500px", width = 12,
                        tabPanel("World", box(plotlyOutput('exhgraph'),width = 12,background = "olive"))),
                 
                 textOutput("exhb"),
                 tags$head(tags$style("#exhb{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
               )),

      tabItem( tabName = "each", 
               fluidRow(
                 box(selectInput("type", label = strong("You can choose one disease!"),
                                 choices = unique(risks$Cause),
                                 selected = "Travel"))),
                 
                fluidRow( box(plotOutput('lineplot', height = "500px"),width = 12,background = "teal")
               )
      ),
    
      tabItem(tabName = "gdp",
              fluidRow(
                textOutput("p21a"),
                tags$head(tags$style("#p21a{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                tabBox(title = "NCD mortality rate (100,000) vs ...",height = "500px", width = 12,
                       tabPanel("GDP per capita", box(plotlyOutput('Factors_plot_2'),width = 12))),
                textOutput("p21b"),
                tags$head(tags$style("#p21b{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }"))
                )),
      
              tabItem(tabName = "age",
              fluidRow(
                textOutput("p22a"),
                tags$head(tags$style("#p22a{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 margin-bottom:20px;
                                 }")),
                tabBox(title = "The propotion of different NCDs in differennt age groups",height = "500px", width = 12,
                       tabPanel("Age", box(plotlyOutput('Factors_plot_1'),width = 12,background = "olive"))),
                       
                textOutput("p22b"),
                tags$head(tags$style("#p22b{color: grey;
                                 font-size: 15px; 
                                 padding-left:20px;
                                 width:90%;
                                 font-style: italic;
                                 }"))
                       
                ))
      ,
    
      tabItem(tabName = "youandNCD",
              fluidRow(  box(title = HTML("<strong style=\"margin-left:5px\">WE PREDICT</strong><br/><strong style=\"margin-left:5px\">YOU PREVENT & WE ARE HEALTHY</strong>"),width = '100%',background = "navy",
                             style="padding:0; margin-left:10px"),
                box(title = "Let me know something about you.", solidHeader = TRUE,
                    selectInput("location", "Where do you come from?", loc),
                    selectInput("sex","What is your gender?",sex),
                    selectInput("age", "What is your age?",age)),
                
                box(title = "What about your daily habits?",solidHeader=TRUE,
                    selectInput("alcohol","How often do you drink?",choice.freq),
                    selectInput("smoke","What about smoking?",choice.freq),
                    selectInput("insu.act","Do you exercise or just move often?",choice.freq)),
                  
                box(title="Please fill up your physical indicators.",solidHeader = TRUE,selectInput("bmi","What do you think of your Body Mass Index(BMI)?",choice.level),
                    selectInput("bp","What do you think of your blood presure?",choice.level),
                    selectInput("cholesterol","What do you think of your cholesterol?",choice.level)
                    ),
               box(title = "Your chance of getting NCD:", width = 7, solidHeader = TRUE,textOutput("prob"),background = "olive") )
      ),
      tabItem(tabName = "advice",
              fluidRow( img( src="6.png",
                        style="width:100%; padding:0 15px 15px 15px; height:360px"),
                        
                      ),
                        box(title=HTML("<strong>CONTROL ALCOHOL USE</strong>"),textOutput("alc")),
                         img( src="5.jpg",
                             style="width:50%; padding:0 15px 15px 15px; height:220px"),
              
                        box(title=HTML("<strong>HEALTHY DIETS</strong>"),textOutput("diet"),width = 12),
                           
                        img( src="2.jpg",
                          style="width:50%; padding:0 15px 15px 15px; height:350px",alpha=0.5),
              
                          box(title=HTML("<strong>CONTROL TABACCO USE</strong>"),textOutput("tab"),width = 5),
                        
                        box(title=HTML("<strong>PHYSICAL ACTIVITY</strong>"),textOutput("phy"),width =12),
                        
                
              )
        
      )
    )
  )
 



server <- function(input, output, session) {
  
  
  #  intro
  output$intro.def <- renderText("A non-communicable disease (NCD) is a disease that is not transmissible directly from one person to another. NCDs include Parkinson's disease, autoimmune diseases, strokes, most heart diseases, most cancers, diabetes, chronic kidney disease, osteoarthritis, osteoporosis, Alzheimer’s disease, cataracts, and others.")
  output$intro.dangerous <- renderText("It can be obtained from WHO that noncommunicable diseases (NCDs) kill 41 million people each year, equivalent to 71% of all deaths globally. Each year, more than 15 million people die from a NCD between the ages of 30 and 69 years; 85% of these \"premature\" deaths occur in low- and middle-income countries.")
  output$intro.risk <- renderText("Risk factors such as a person's background, lifestyle and environment increase the likelihood of certain NCDs. Every year, at least 5 million people die because of tobacco use and about 2.8 million die from being overweight. High cholesterol accounts for roughly 2.6 million deaths and 7.5 million die because of high blood pressure.As the above facts, it is more and more urgent to study and analyze the development status, risk factors and prevention of NCDs. Next, let's explore NCD together!")
  output$intro.data <- renderText("We have obtained a lot of data on NCD from WHO, IHME and other platforms, including the prevalence data of NCD in all regions of the world from 2000 to 2019, the number of deaths caused by NCD, the main diseases leading to death, the main factors affecting the incidence of NCD, etc. Through the analysis of these data, we can clearly understand the development and trend of NCD in the world. Secondly, by analyzing the risk factors of NCD, we can clearly show the influence of different factors on the prevalence of NCD, and also provide important clues for the prevention of NCDs.")
  
  output$p1a <- renderText("It is well known that some diseases are related to gender, for example, hemophilia is mostly male. Besides, for the same disease, the incidence and mortality rate may be different for different genders. Accordingly, for exploring the relationship between gender and deaths relevant to NCDs，we analyze the proportion of causes of deaths in men，women and total in 2019, which could aid us offering helpful and valuable advices associated with NCDs prevention for men and women.")
  div()
    # Overview_plot_1  
  Overview_plot_1_gen <- function(x)
  { Overview_plot_1_tmp <- joined.ncd.prop %>%
    filter(sex == x) %>%
    arrange(Death.number.in.2019)
    Overview_plot_1_tmp$cause <- factor(Overview_plot_1_tmp$cause,levels = Overview_plot_1_tmp$cause)
    Overview_plot_1_p <- plot_ly()
    
    Overview_plot_1_p <- Overview_plot_1_p %>% add_pie(data = man.ncd.prop, labels = ~cause, values = ~total.cause.number.man,
                                             name = "Men", hole = 0.6,domain = list(x = c(0, 0.4), y = c(0, 1)))
    Overview_plot_1_p <- Overview_plot_1_p %>% add_pie(data = woman.ncd.prop, labels = ~cause, values = ~total.cause.number.woman
                                             ,name = "Women", hole = 0.6,domain = list(x = c(0.6, 1), y = c(0, 1)))
    Overview_plot_1_p <- Overview_plot_1_p %>% layout(title = "NCD in 2019: Proportion of causes of deaths, in men and women", showlegend = T, 
                                            xaxis = list(showgrid = T, zeroline = T, showticklabels = TRUE),
                                            yaxis = list(showgrid = T, zeroline = T, showticklabels = TRUE),
                                            annotations = list(x = c(.04, .62),
                                                               y = c(.9, .9),
                                                               text = c("Male","Female"),
                                                               xref = "papper",
                                                               yref = "papper",
                                                               showarrow = F
                                            )
    )
    
    return(Overview_plot_1_p)
  }
  
  output$Overview_plot_1_m <- renderPlotly({ Overview_plot_1_gen('man')})
  # output$Overview_plot_1_f <- renderPlotly({ Overview_plot_1_gen('woman')})
  # output$Overview_plot_1_t <- renderPlotly({ Overview_plot_1_gen('total')})
  
  output$p1b <- renderText("one insightful findings could be summarized as follows:</br>
 a) Regardless of gender, the deaths caused by cardiovascular diseases take the most proportion, with 66.1% and 70.6% for men and women respectively.</br>
 b) Following cardiovascular diseases, neoplasms are the second important reasons (relevant to NCDs) for the deaths of men in 2019, while diseases contributing to the deaths of women are diabetes and kidney diseases.</br>
 c) Compared with men, women have lower proportion of deaths caused by digestive diseases and chronic respiratory diseases. However, women also have higher probability of deaths because of neurological disorders.
")
  
  
  output$p2a <- renderText("In order to further study the impact of NCDs on countries around the world, after collecting and analyzing the data, we draw the figure below, which shows the global deaths caused by NCDs in 2019.")
  # Overview.plot.2
  output$Overview_plot_2 <- renderHighchart({
    hcmap(
      "custom/world-eckert3",
      data = ncd.world,
      value = "value",
      joinBy = c("iso-a2", "X2"),
      name = "2019: NCD in world",
      dataLabels = list(enabled = TRUE, format = "{point.name}"),
      borderColor = "#FAFAFA",
      borderWidth = 0.1,
      tooltip = list(
        valueDecimals = 2,
        valuePrefix = "Total deaths: ",
        valueSuffix = ""
      )
    ) %>%
      hc_colorAxis(minColor = "#F6E3E0", maxColor = "#911103") %>% 
      hc_title(text = "2019: NCD deaths in world") %>%
      hc_add_theme(hc_theme_gridlight()) %>% 
      hc_credits(enabled = T,text = 'Source: Institute for Health Metrics and Evaluation')
    
  })
  output$p2b <- renderText("As can be seen from the world map, it is clear that China has the largest number of deaths caused by NCDs in 2019, with 10million approximately, which is followed by the number of that in India, around 6 million. In addition, the number of deaths in the United States in 2019 is close to 3 million, ranking the third place in the world, and that of other countries is relatively small, less than 3 million.")
  
  # Overview_plot_3
  output$p3a <- renderText("AS illustrated in current studies, NCDs are the leading cause of death globally. In 2012, they caused 68% of all deaths (38 million) up from 60% in 2000. NCDs cause 41 million deaths each year, equivalent to 71% of the total global deaths. The following figure shows the evolution of NCDs deaths in six main regions of the world from 2000 to 2019.")
  output$Overview_plot_3 <- renderPlotly({
    fig.Total.NCD.Deaths.in.thousands <- ggplot(Total.NCD.Deaths.in.thousands,aes(x = year,y = number,fill = location),alpha(alpha = 0.3)) +
      geom_col() + theme(axis.text.x = element_text(size=7,angle = 0))+scale_y_continuous(labels = scales::comma)
    
    fig.Total.NCD.Deaths.in.thousands <- ggplotly(fig.Total.NCD.Deaths.in.thousands)
    fig.Total.NCD.Deaths.in.thousands <- fig.Total.NCD.Deaths.in.thousands %>% layout(
                                                                                      xaxis = list(title = "Year",
                                                                                                   gridcolor = 'rgb(255, 255, 255)'
                                                                                      ),
                                                                                      yaxis = list(title = "Death number",
                                                                                                   gridcolor = 'rgb(255, 255, 255)'
                                                                                      ),
                                                                                      paper_bgcolor = 'rgb(243, 243, 243)',
                                                                                      plot_bgcolor = 'rgb(243, 243, 243)')
    return(fig.Total.NCD.Deaths.in.thousands)
  })
  output$p3b <- renderText("As can be seen from the bar chart, In general, the number of deaths caused by NCDs in the world is on the rise，increasing by 10,000,000 roughly between 2000 and 2019. Obviously, the number of deaths because of NCDs in Western Pacific takes the first place, rising from 8,923,878 to 12,037,286. By contrast, countries in Eastern Mediterranean have the least number of deaths globally, which also rises from 1,792,260 to 2,825,741 in the period of theses 20 years. Generally, from the above figure, we can see that more and more people's lives and health are threatened by NCDs, so it is more and more urgent to prevent NCDs.")
  
  # UHC graph
  output$uhca <- renderText("UHC(United Health Care) service coverage index shows the comprehensive health strength of a country. The relation between UHC service coverage index and cause of death by NCD indicates the mutual influence between UHC service coverage and NCD.")
  output$UHCgraph <- renderPlotly({
    data2<-ggplot(data1,aes(x = Cause.of.death.by.non.communicable.diseases,y = UHC.service.coverage.index))+geom_bar(stat="identity",position="dodge",width=0.9,fill="darkturquoise")+theme(axis.text.x = element_text(size=7,angle = 0))+scale_y_continuous(labels = scales::comma)
    
    data2<-ggplotly(data2)
    data2 <- data2 %>% layout(xaxis = list(title = "Cause of death by NCD(% of total)",gridcolor = 'rgb(255, 255, 255)'),yaxis = list(title = "UHC service coverage index",gridcolor = 'rgb(255, 255, 255)'),paper_bgcolor ='rgb(243, 243, 243)',plot_bgcolor = 'rgb(243, 243, 243)')
    return(data2)
  })
  output$uhcb <- renderText("According to the data from World Bank, countries with more causes of death by NCD have higher UHC service coverage index, which implies that most countries make effort to increasing the coverage of UHC service in order to solve the high proportional  causes of death by NCD.")
  #current health expenditure
  output$currha<-renderText("This chart describes the current health expenditure and cause of death by NCD in every country. Current health expenditure shows the emphasis placed by the government on health service.")
  output$currhgraph<-renderPlotly({
    data3<-ggplot(data1,aes(x = Cause.of.death.by.non.communicable.diseases,y = Current.health.expenditure.per.capita))+geom_bar(stat="identity",position="dodge",width=0.9,fill="lightseagreen")+theme(axis.text.x = element_text(size=7,angle = 0))+scale_y_continuous(labels = scales::comma)
    data3<-ggplotly(data3)
    data3 <- data3 %>% layout(xaxis = list(title = "Cause of death by NCD(% of total)",gridcolor = 'rgb(255, 255, 255)'),yaxis = list(title = "Current health expenditure per capita($)",gridcolor = 'rgb(255, 255, 255)'),paper_bgcolor ='rgb(243, 243, 243)',plot_bgcolor = 'rgb(243, 243, 243)')
    return(data3)
    })
  output$currhb<-renderText("It seems that current health expenditure is not linear correleted with cause of death by NCD. Despite that most countries with more causes of death by NCD commit more money to improving the national health service, several countries do not improve the current health expenditure to solve the high proportional  causes of death by NCD, which implies that NCD is not the main factor to influence the health expenditure. ")
  #domestic health expenditure
  output$domeha<-renderText("Domestic private health expenditure indicates personal emphasis on health. This chart reflects the relation between domestic private health expenditure and cause of death by NCD.")
  output$domehgraph<-renderPlotly({
    data4<-ggplot(data1,aes(x = Cause.of.death.by.non.communicable.diseases,y = Domestic.private.health.expenditure.per.capita))+geom_bar(stat="identity",position="dodge",width=0.9,fill="lightblue2")+theme(axis.text.x = element_text(size=7,angle = 0))+scale_y_continuous(labels = scales::comma)
    data4<-ggplotly(data4)
    data4 <- data4 %>% layout(xaxis = list(title = "Cause of death by NCD(% of total)",gridcolor = 'rgb(255, 255, 255)'),yaxis = list(title = "Domestic private health expenditure per capita($)",gridcolor = 'rgb(255, 255, 255)'),paper_bgcolor ='rgb(243, 243, 243)',plot_bgcolor = 'rgb(243, 243, 243)')
    return(data4)
    })
  output$domehb<-renderText("The relation between domestic private health expenditure and cause of death by NCD is similar to the relation between current health expenditure and cause of death by NCD. High proportion causes of death by NCD leads to more domestic private health expenditure, but some countries can not commit much money to domestic private health expenditure though they have many causes of death by NCD.")
  #external health expenditure
  output$exha<-renderText("This chart describes the external health expenditure and cause of death by NCD in every country. The relation can help better learn about how NCD affects the policy of external health expenditure.")
  output$exhgraph<-renderPlotly({
    data5<-ggplot(data1,aes(x = Cause.of.death.by.non.communicable.diseases,y = External.health.expenditure.per.capita))+geom_bar(stat="identity",position="dodge",width=0.9,fill="steelblue3")+theme(axis.text.x = element_text(size=7,angle = 0))+scale_y_continuous(labels = scales::comma)
    data5<-ggplotly(data5)
    data5 <- data5 %>% layout(xaxis = list(title = "Cause of death by NCD(% of total)",gridcolor = 'rgb(255, 255, 255)'),yaxis = list(title = "External health expenditure per capita($)",gridcolor = 'rgb(255, 255, 255)'),paper_bgcolor ='rgb(243, 243, 243)',plot_bgcolor = 'rgb(243, 243, 243)')
    return (data5)
    })
  output$exhb<-renderText("Different from current health expenditure and domestic private health expenditure, cause of death by NCD does not make a big difference to external health expenditure. External health expenditure does noe change a lot along with the change of cause of death by NCD.")
  #each disease
  aa <- reactive({
    risks %>% filter( Cause == input$type )
  })
  
  output$lineplot <-renderPlot({  plot(x = factor(aa()$age,levels = c("1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 to 89","90 to 94")), y = aa()$Number,
                        xlab = "Age", ylab = "Death Number")
                     })
 
  
  
  #Factors_plot_1
  output$p21a <- renderText("")
  output$Factors_plot_1 <- renderPlotly({
    Age<- factor(risks$age,levels = c("1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 to 89","90 to 94"))
    
    fig.risks <- ggplot(risks) + geom_col(aes(Age,fill=Cause,y = Number),alpha=0.7)+guides(shape = guide_legend(override.aes = list(size = 0.7)))+labs(x="Ages",y="Global death number in 2019")+theme(axis.text.x =element_text(size=7,angle = 45))+scale_y_continuous(labels = scales::comma)
    ggplotly(fig.risks)
    return(fig.risks)
  })
  output$p21b <- renderText("From the bubble chart, generally speaking, the higher the country's per capita GDP is, the lower the mortality of NCDs would be. For example，Spain , the country with the maximum GDP per capital in 2019, has the lowest morality rate of NCDs. What’s more, we can also draw the same conclusion by analyzing the data of China. By comparing the data of China during these consecutive 20 years, it can be found that its morality rate of NCDs continues to decline with the ongoing improvement of China’s GDP per capital. More importantly, countries in Europe have relatively high GDP per capita and relatively low mortality of NCDs, while African countries with lower GDP per capita are faced with the highest morality rate of NCDs. In fact, the relationship between economy and morality rate of NCDs is obvious in people’s daily life. As you can imagine, in a resource deficient environment, the medical expenses for NDCs will quickly deplete household resources. The high costs of NDCs, including often long-term and expensive treatment and the loss of breadwinners, force millions of people into poverty every year and inhibit development. Therefore, for some underdeveloped and even developing countries, governments can take the form of NCDs grants to help patients get active treatment in time so as to reduce the mortality of NCDs effectively.")
  
  
  
  #Factors_plot_2
  output$p22a <- renderText("As is known to all，NCDs include Parkinson's disease, autoimmune diseases, strokes, most heart diseases, most cancers, diabetes, chronic kidney disease, osteoarthritis, osteoporosis, Alzheimer’s disease, cataracts, and others. It is worth noting that if we can count the proportion of people of different ages suffering from various NCDs, we can know more clearly which NCD people of different ages are most likely to suffer from, which provides an important reference for people of different ages to prevent NCDs. The following figure shows the relationship between the global deaths of different NCDs in 2019 and age")
 
   output$Factors_plot_2 <- renderPlotly({
    mortality.rate <- mortality.rate %>%
      filter(!is.na(population) & !is.na(gdp.percapita))
    fig.mort.rate <- ggplot(mortality.rate, aes(gdp.percapita, value.weight.avg.both.sex, color = location)) +
      geom_point(alpha = 0.7 ,aes(size = population, frame = year, ids = country.name)) +
      scale_y_continuous(labels = scales::comma)+scale_x_continuous(labels = scales::comma,trans = 'log10') 
    fig.mort.rate <- ggplotly(fig.mort.rate) %>%
      style(hovertemplate = paste('Country:', mortality.rate$country.name, '<br>Mortality rate:', mortality.rate$value.weight.avg.both.sex, '<br>GDP:', mortality.rate$gdp.percapita,
                                  '<br>Pop.:', mortality.rate$population))
    
    fig.mort.rate <- fig.mort.rate %>% layout(title = 'NCD mortality rate (100,000) vs Per Capita GDP',
                                              xaxis = list(title = 'GDP per capita (2010 constant US dollar),after log base 10',
                                                           gridcolor = 'rgb(255, 255, 255)'
                                              ),
                                              yaxis = list(title = 'Mortality rate (in 100,000)',
                                                           gridcolor = 'rgb(255, 255, 255)'
                                              ),
                                              paper_bgcolor = 'rgb(243, 243, 243)',
                                              plot_bgcolor = 'rgb(243, 243, 243)')
    fig.mort.rate
    return(fig.mort.rate)
  })
  
  output$p22b <- renderText(" As can be seen from the bar chart above, in general, Few of children aged 1-13 died of NCDs and the number of deaths caused by NCDs increases with the growth of age (from 15 to 84 years old). However, a totally different trend, the number of deaths caused by NCDs reduces with the growth of age, could be viewed for people older than 84 years. In ，the findings obtained from the chart and some NCDs prevention suggestions for people from different age groups can be given as follows: 
a) The deaths relevant to NCDs between 15 and 24 years old are mainly caused by diabetes and kidney diseases. Therefore, young people in this age group should pay more attention to the prevention of diabetes and kidney diseases.
b) The deaths relevant to NCDs between 25 and 84 years old are mainly caused by cardiovascular diseases.
 Therefore，adult (25-84 years old) should develop a healthy lifestyle, early detection and effective control of a variety of related risk factors, such as hypertension, hyperlipidemia, smoking, diabetes and so on. For high-risk patients, aspirin and statins should be used timely.
c) Although the total number of deaths relevant to NCDs deceases for the old above 84, an increasing trend of deaths caused by neurological disorders could be seen from the bar.
To help the old prevent neurological disorders，the elderly can read more books, play more puzzle games, communicate more with people, and do appropriate aerobic exercise.
")
  
  output$tab <-renderText("Tobacco use is a common risk factor to the main NCDs - cardiovascular disease, cancer, chronic respiratory disease - and other diseases including tuberculosis and neurological disorders. Globally, 14% of all NCDs deaths among adults aged 30 years and over are attributable to tobacco. Tobacco use is also responsible for 3.7% of disability-adjusted life years (DALYS), globally. And It’s notable that there are more than 4000 chemicals in tobacco smoke, of which at least 250 are known to be harmful and more than 69 are known to cause cancer.
Full implementation of the WHO Framework Convention on Tobacco Control would bring the single biggest blow to heart diseases, cancer, diabetes & respiratory disease. 
- Dr. Margaret Chan, WHO Director-General, NCD Summit, 2011")
  
  output$alc <- renderText("Along with tobacco, diet and lack of exercise, alcohol is recognized as one of four major common risk factors for NCDs. Despite this, alcohol is still widely consumed in ways and volumes that are particularly hazardous. For example, a causal link has been found between alcohol and cancer of the oral cavity, pharynx, larynx, oesophagus, liver, colon, rectum, and female breast. For all of these cancers, the risk of cancer increases steadily with greater volumes of drinking.")
  
  output$diet <- renderText("What we eat and our nutritional status can affect cardiovascular diseases, some types of cancer, and diabetes. Foods, diet and nutritional status, including overweight and obesity, are also associated with elevated blood pressure and blood cholesterol, and resistance to the action of insulin. These conditions are not only risk factors for NCDs, but major causes of illness themselves. Both undernutrition and overweight and obesity place individuals at risk of developing NCDs and it is critical to address malnutrition in all its forms in an integrated manner along the life-course approach.
Globally, calories obtained from meat, sugars and oils and fats have been increasing during recent decades, and those from fibre-rich foods such as wholegrains, pulses and roots have been declining. Consumption of processed and convenience foods continue to rise rapidly in LMICs. This nutrition transition affects dietary patterns and nutrient intake, which influence the risk of developing NCDs.")
  

  
  
  output$phy <- renderText("Regular, moderate intensity physical activity reduces the risk of various NCDs and protects health.

For children under 5 years of age
In a 24-hour day, children 1-2 years of age should:
1.spend at least 180 minutes in a variety of types of physical activities at any intensity, including moderate- to vigorous-intensity physical activity, spread throughout the day; more is better;
2.not be restrained for more than 1 hour at a time (e.g., prams/strollers, high chairs, or strapped on a caregiver’s back) or sit for extended periods of time.
In a 24-hour day, children 3-4 years of age should:
1.spend at least 180 minutes in a variety of types of physical activities at any intensity, of which at least 60 minutes is moderate- to vigorous-intensity physical activity, spread throughout the day; more is better;
2.not be restrained for more than 1 hour at a time (e.g., prams/strollers) or sit for extended periods of time.
 
Children and adolescents aged 5-17 years
1.should do at least an average of 60 minutes per day of moderate-to-vigorous intensity, mostly aerobic, physical activity, across the week.
2.should incorporate vigorous-intensity aerobic activities, as well as those that strengthen muscle and bone, at least 3 days a week.
3.should limit the amount of time spent being sedentary, particularly the amount of recreational screen time.
Adults aged 18–64 years
1.should do at least 150–300 minutes of moderate-intensity aerobic physical activity; 
2.or at least 75–150 minutes of vigorous-intensity aerobic physical activity; or an equivalent combination of moderate- and vigorous-intensity activity throughout the week
3.should also do muscle-strengthening activities at moderate or greater intensity that involve all major muscle groups on 2 or more days a week, as these provide additional health benefits.
Adults aged 65 years and above
1.Same as for adults; and
2.as part of their weekly physical activity, older adults should do varied multicomponent physical activity that emphasizes functional balance and strength training at moderate or greater intensity, on 3 or more days a week, to enhance functional capacity and to prevent falls. 
")
  
  get.value.loc <- function(x)
  {
    case_when(
      x=='Africa' ~ 1,
      x=='Americas'~0.993789,
      x=='Eastern Mediterranean'~0.99045,
      x=='Europe'~0.996609,
      x=='South-East Asia'~0.992742,
      x=='Western Pacific'~0.994449
    )
  }
  get.value.sex <- function(x)
  {
    case_when(
      x=='Male'~0.988,
      x=='Female'~1
    )
  }
  get.value.age <- function(x)
  {
    case_when(
      x=='0 to 19'~1,
      x=='20 to 54'~1.009569,
      x=='55 to 89'~1.01189
    )
  }
  get.value.freq <- function(x)
  {
    case_when(
      x=='Never'~0,
      x=='Maybe sometimes'~0.25,
      x=='I usually do'~0.5,
      x=='I often do'~0.75,
      x=='Cannot live without it'~1
    )
  }
  get.value.level <- function(x)
  {
    case_when(
      x=='Low'~0.2,
      x=='Below average'~0.4,
      x=='Middle'~0.6,
      x=='Beyond average'~0.8,
      x=='High'~1
    )
  }
  get.value.bmi <- function(x)
  {
    case_when(
      x=='Low'~1,
      x=='Below average'~0.5,
      x=='Middle'~0,
      x=='Beyond average'~0.5,
      x=='High'~1
      
    )
  }
  #calculate prob
  output$prob <- reactive({
    p <-0.010066*get.value.freq(input$alcohol)+0.016622*get.value.level(input$bp)+0.006411*get.value.bmi(input$bmi)+0.022564*get.value.level(input$cholesterol)+0.006885*(-get.value.freq(input$insu.act))+0.005061*get.value.freq(input$smoke)+0.024217
    p.fixed <- p*get.value.age(input$age)*get.value.sex(input$sex)*get.value.loc(input$location)
    return(paste0(substr(as.character(p.fixed*100),1,4),'%'))
  })

}

shinyApp(ui=ui, server=server)

