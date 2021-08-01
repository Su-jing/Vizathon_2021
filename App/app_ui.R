library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(leaflet)
library(tidyverse)
library(plotly)
library(rworldmap)
library(RColorBrewer)
library(classInt)
library(rnaturalearth)
library(countrycode)

home <- tabPanel(
  h2("Home"),
  titlePanel(h3("General introction of app")),
  p(span("Globally, ", style = "color: red"), "as of 6:17pm CEST, 29 July 2021, there have been 195,886,929 
    confirmed cases of COVID-19, including 4,189,148 deaths, reported to WHO. 
    As of 28 July 2021, a total of 3,839,816,037 vaccine doses have been administered.")
  # sidebarLayout(
  #   sidebarPanel(
  #     # a `radioButtons` widget, the default choice is "Professional"
  #     radioButtons(
  #       inputId = "status",
  #       label = "Hi, are you a student or professional?",
  #       choices = list("Student" = "college education", "Professional"
  #                      = "profession"),
  #       selected = "profession"
  #     ),
  #     textOutput("suggestion"),
  #     
  #     # a `selectInput` widget, the default choice is "Profession"
  #     selectInput(
  #       inputId = "interest",
  #       label = "Choose a rate of interest here (not need to be the one
  #               in suggesstion above):",
  #       choices = c("College_Education_Rates", "Profession_Rates"),
  #       selected = "Profession_Rates"
  #     )
  #   ),
  #   mainPanel(
  #     plotOutput("comparison_plot")
  #   )
  # )
)


page_one <- tabPanel(
  h2("Covid-19 Overview"),
  titlePanel(h3("How It Grows?")),
  p("This section visualizes the number of Covid-19 cases, including confirmed, deaths, 
    and recovered, from 22 January 2020 to 20 July 2021. It aims to provide an overview
    of the level fo severity of Covid-19 in each country and shows how it progresses over time."),
  selectInput(
    inputId = "caseType",
    label = "Choose type of data you are interested in:",
    choices = c("Confirmed", "Death", "Recovered"),
    selected = "Confirmed"
  ),
  plotlyOutput("inter_world_case_map"),
  p("blahhhhhhhhhhhhhhhh"),
  
  
  
  # daily change
  selectInput(
    inputId = "country",
    label = "Choose country you are interested in:",
    choices = temp_c_total$Country.Region,
    selected = "US"
  ),
  plotlyOutput("inter_country_case"),
  p("blahhhhhhhhhhhhhhhh")
)


page_two <- tabPanel(
  h2("Second-order Impacts"),
  titlePanel(h3("For the groups of people being more vulnerable during the 
  second-order impacts of Covid-19 pandemic, inspect from three aspects: 
  mental health, aging population, and history of NCDs with analysis.")),
  
  # Mental Health
  h3("1) Mental Health", style = "font-size:20px;"),
  uiOutput("link1"),
  p("Many research has shown that there are bidirectional associations 
  between COVID-19 and psychiatric disorder, and patients with a history of 
  psychiatric illness are at a higher risk of being diagnosed with COVID-19.
  This measn that mental disorders might be a risk factor for severe COVID-19.
  
  Thus, the seriousness of mental problems can be considered as a factor in our 
  evaluation framework to assess the vulnerability of a country."),
  p("Now, lets first visualize the overall trend of mental disorder problem from
  1992 to 2017."),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "type1",
        label = "Choose one of the following visualizations:",
        choices = c("Global trend of mental health (1992-2017)", 
                    "Histogram summary of data for each year"),
        selected = "Global trend of mental health (1992-2017)"
      ),
      sliderInput(inputId = "year1",
      label = "For histogram of each year,
      choose a year below or click the play button at the bottom right corner:", 
      value = 1992, min = 1992,
      max = 2017, step = 1, sep = "", animate = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("mental_health_plot")
    )
  ),
  p("From the first visualization, we use the average percentage of mental
    and substance use disorders in the world to visualize the global trend from 1992
    to 2017. We can know that the line is slightly decreasing but almost
    remains at the same level, which is about 12.5%-13%. 
    By animating the histograms of the percentage data
    from all countries each year, we can see that the histogram doesn't change
    much.
    Therefore, continuing with this trend, the percentage would still be 12%-13%
    at the start of COVID-19 pandemic."),
  p("Due to this relatively stable situation
    of the global mental health problem, ", span("10% ", style = "color: red"),
    "is a reasonable weight for mental health factor in our evaluation
    framework."),
  
  # Median Age
  h3("2) Aging Population", style = "font-size:20px;"),
  uiOutput("link2"),
  p("Studies also show that the fatality rate for people over 80 from COVID-19 
    is almost 15% according to data from China and that there's a direct 
    correlation between mortality and age. There are two main reasons of this.
    First, The elderly are more likely to get acute respiratory distress syndrome, 
    the acute lung injury that is causing many of the deaths. Second, a 
    side-effect of the impact of coronavirus is that the health system is
    overwhelmed, so the elderly are hospitalized on a daily basis at much higher 
    rates than younger people and they are not going to get the quality care 
    they deserve during the outbreak."),
  p("Thus, we use aging population as a factor in our vulnerability evaluation
    framework."),
  p("Lets take a look at the global trend of aging population problem."),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "type2",
        label = "Choose one of the following visualizations:",
        choices = c("Global trend of median age (1950-2050)", 
                    "Histogram summary of data for each year"),
        selected = "Global trend of median age (1950-2050)"
      ),
      sliderInput(inputId = "year2",
                  label = "For histogram of each year,
      choose a year below or click the play button at the bottom right corner:", 
                  value = 1950, min = 1950,
                  max = 2050, step = 5, sep = "", animate = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("median_age_plot")
    )
  ),
  p("From the first visualization, we take the average of median ages of all
    countries for each year, which shows the global trend.
    Notice that this trend goes beyong 2021 year and predicts the global median
    age at 2050. We can see that the line is obviously increasing, which means
    that there are more older people now and much more in the near future than
    in the past. 
    By animating the histograms of the median ages
    of all countries each year, we can clearly see that the whole histogram is
    'moving to the right'.  
    Therefore, continuing with this trend, the aging population problem is getting
    more and more serious globally both now and in the short run."),
  p("Due to this fierce increment of the number of older people, we decide to
    use ", span("20% ", style = "color: red"),
    "as the weight for aging population factor in our evaluation
    framework."),
  
  # NCD
  h3("3) Non-communicable Diseases", style = "font-size:20px;"),
  uiOutput("link3"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "type3",
        label = "Choose one of the following visualizations:",
        choices = c("Global trend of Non-communicable Diseases (1990-2017)", 
                    "Histogram summary of data for each year"),
        selected = "Global trend of Non-communicable Diseases (1990-2017)"
      ),
      sliderInput(inputId = "year3",
                  label = "For histogram of each year,
      choose a year below or click the play button at the bottom right corner:", 
                  value = 1990, min = 1990,
                  max = 2017, step = 1, sep = "", animate = TRUE
      )
    ),
    mainPanel(
      plotlyOutput("NCD_plot"),
      textOutput("NCD_analysis")
    )
  )
)


page_three <- tabPanel(
  h2("Vulnerability"),
  titlePanel(h3("The vulnerability assessment of each country, using data/statistics from previous sections")),
  plotlyOutput("inter_world_v_map"),
  p("xxxxx"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "countryp",
        label = "Choose country you are interested in:",
        choices = total$Country,
        selected = "US"
      ),
      p("The pie chart shows.....")
    ),
    mainPanel(
      plotOutput("inter_country_pie"),
      p("blahhhhhhhhhhhhhhhh")
    )
  ),
  tableOutput('table')
)



ui <- shinyUI(fluidPage(
  p("By Ryan & Wuwei", style = "font-size: 10px; text-align: left"),
  h1(span("Covid-19", style = "font-weight: 300; color: red",), 
     "Trend Visualization and Vulnerability Assessment"),
  includeCSS("style.css"),
  tabsetPanel(
    home,
    page_one,
    page_two,
    page_three
  )
))
