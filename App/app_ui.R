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
  titlePanel(h3("General Introduction")),
  p("Globally, as of 6:17pm CEST, 29 July 2021, there have been 195,886,929 
    confirmed cases of COVID-19, including 4,189,148 deaths, reported by WHO. 
    As of 28 July 2021, a total of 3,839,816,037 vaccine doses have been administered (from WHO). 
    The huge number of reported cases has made Covid-19 a global pandemic, and thus makes
    it a central topic of public health."),
  p("Thus, our aim is to explore how Covid-19 would impact, or be impacted by, the social public health construction
    of each country and build visualizations based on public health data to gain a deeper understanding of Covid-19.
    And finally, we will make a vulnerability assessment for each country on top of that. As such, this app is naturally
    separated into three main parts:"),
  h3("1. Covid-19 Overview", style = "font-size:20px;"),
  p("In page 1, we make visualizations for trend of the COVID-19 pandemic from
    both a global perspective and a specific country perspective.
    We also analyze the number of ", span("confirmed cases, death cases, and cured 
    cases",style="color: red"),", respectively, with ", span("a weight assigned to each one", style = "color: red"),
    " , which is used for vulnerability assessment in page 3."),
  h3("2. Second-order Impacts", style = "font-size:20px;"),
  p("In page 2, from public health persepctive, we visualize and analyze 
    three more vulnerable groups of people during the COVID-19 pandemic: ",
    span("mental health, aging population, and people with non-communicable diseases",style="color: red"),
    ". For each of these three factors, we also ", span("assign a weight", style = "color: red"), 
    " to it, which is used for vulnerability assessment in page 3."),
  h3("3. Vulnerability Assessment", style = "font-size:20px;"),
  p("In page 3, for each country, we use the following formula to assess vulnerability: "),
  img(src='f.png', width = "808.8", height = "22.8"),
  # uiOutput("f"),
  p("where 0.25, 0.25, 0.15, 0.15, 0.2, and -0.5 are weights we used for these 6 factors 
    in our evaluation framework, which have detailed explainations in page 1 and 2."),
  p("The final result of vulnerability of each country is a number floating around 0.
    Thus, if the result is negative, we can say that this country is relatively
    less vulnerable. If the result is positive, we can say that this country
    is relatively more vulnerable.")
)


page_one <- tabPanel(
  h2("Covid-19 Overview"),
  titlePanel(h3("Covid-19 Overview")),
  p("To build a reliable assessment framework of the country's vulnerability to Covid-19, 
    one of the key things is obviously to study Covid-19 itself. As a global pandemic, 
    the infection rate, fatality rate, etc, decides how severe it is and how vulnerable 
    people are when encountering it. Also, the reported cases of Convid-19 (either 
    confirmed cases, recovered cases, and deaths) to some extent reflects the countries' 
    level of public health construction, providing a quantitive indication of factors that 
    are hard to be quantified, such as the effectiveness of policy responses. All those explain
    why we include Covid-19 reported cases as a metric in the assessment framework."),
  p("So, this section visualizes the number of Covid-19 reported cases with data collected by ", 
  a(href = "https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins University Center for 
  Systems Science and Engineering (JHU CSSE)"), ". The data includes Covid-19 confirmed cases, 
  recovered cases, and deaths, from 22 January 2020 to 20 July 2021. The visualizations below
  aim to provide an overview of Covid-19 and the underlying data are prepared for later assessment."),
 
  
  h3("1) Global Cumulative Reported Cases", style = "font-size:20px;"),
  p("The global cumulative reported cases are visualized as an interactive geographic heatmap. It shows
    the number of a certain type of reported cases by country and the darker the color on the map, the more
    cases in the corresponding country. (Since the datasets do not cover every country of the world, so
    missing values are showed in white.)"),
  selectInput(
    inputId = "caseType",
    label = "Choose type of case you are interested in:",
    choices = c("Confirmed", "Death", "Recovered"),
    selected = "Confirmed"
  ),
  plotlyOutput("inter_world_case_map"),
  p("The map reveals that the Americas, Europe, and South-East Asia are the major areas where Covid-19 causes
    a great number of confirmed cases and deaths. Intuitively, the greater number of confirmed cases and deaths
    indicates that countries in these areas are more vulnerable to Covid-19. For simplicity, it is assumed that
    the number of confirmed cases and deaths cases contribute to the same weights on calculating the vulnerability
    of counteries. On the other hand, the number of recovered cases is a positive indicator for the countries. The more 
    recovered cases in a country, the country is less vulnerable to Covid-19. It indicates that people in such
    countries may have better health conditions and the countries hold a higher level of public health infrastructure.
    As a result, when including recovered cases in calculating the vulnerability of countries, we use the negative
    value instead, and thus more number of recovered cases would lead to a lower level of vulnerability."),
  
  
  h3("1) Country Cumulative Reported Cases (Daily Trend)", style = "font-size:20px;"),
  p("This visualization shows the daily trend of the number of reported cases in the selected country from 22 January 2020 to 20 July 2021."),
  # daily change
  selectInput(
    inputId = "country",
    label = "Choose the country you are interested in:",
    choices = temp_c_total$Country.Region,
    selected = "US"
  ),
  plotlyOutput("inter_country_case"),
  p("As we would see, a promising fact is that in almost all of the countries, the number of recovered cases is close to the number
  of confirmed cases, even though the number of confirmed cases is huge. The increase rate of reported cases is revealed by the slope of the 
  shape. As for the increase rate, different countries diverge. Some countries have got a very small, even a zero, increase rate of
   confirmed and deaths early in 2020, but there are still countries experiencing an ever-higher rate of deaths currently."),
  p("To build the vulnerability assessment framework, we would assign different weights to the above variables 
    and use them of gauge the vulnerability of counteries in face of Covid-19. As mentioned above, we assumed a same
    weight for the number of confirmed cases and deaths. Combined with other metrics we would discuss later, we finally decided
    to assign the number of confirmed cases and deaths with a weight of ", span("12.5% ", style = "color: red"), ", and
    the number of recovered cases is assigned with a weight of ", span("50% ", style = "color: red"), ".")
)


page_two <- tabPanel(
  h2("Second-order Impacts"),
  titlePanel(h3("Second-oder Impacts of COVID-19 Pandemic")),
  
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
  p("From the first visualization, we use the average percentage of people with mental
    and substance use disorders in the world to visualize the global trend from 1992
    to 2017. We can know that the line is slightly decreasing but almost
    remains at the same level, which is about 12.5%-13%. 
    By animating the histograms of the percentage data
    from all countries each year, we can see that the histogram doesn't change
    much.
    Therefore, continuing with this trend, the average percentage would still be 12%-13%
    at the start of COVID-19 pandemic."),
  p("Due to this relatively stable situation
    of the global mental health problem, ", span("15% ", style = "color: red"),
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
    age even at 2050. We can see that the line is obviously increasing, which means
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
  h3("3) Non-communicable Diseases (NCDs)", style = "font-size:20px;"),
  uiOutput("link3"),
  p("We use the prevalence of non-communicable diseases in each country as the
    third factor in our vulnerability evaluation framework.
    A research from UNSW Sydney found that people with NCDs are more vulnerable 
    to catching and dying from COVID-19.
    There has never been a more dangerous time than the COVID-19 pandemic 
    for people with non-communicable diseases (NCDs). Scientists mentioned
    two main reasons about this. First, during the pandemic, it is more likely
    for them to be exposed to NCD risk factors, such as substance abuse, social 
    isolation and unhealthy diets. Second, COVID-19 disrupted essential public 
    health services which people with NCDs rely on to manage their conditions."),
  p("Lets take a look at the global trend of Non-communicabl diseases
    measured by disability-adjusted life year (DALY)."),
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
  ),
  p("From the first visualization of global trend, we can see that the DALY rates
    decrease by year.
    This means that fewer and fewer people get NCDs each year.
    By animating the histograms of DALY rates
    of all countries each year, we can clearly see that the whole histogram is
    gradually 'moving to the left'.  
    Therefore, continuing with this trend, there are fewer people that suffer
    NCDs both now and in the near future than in the past."),
  p("Due to this gradual decrement, we decide to use ", span("15% ", style = "color: red"),
    "as the weight for NCD factor in our evaluation framework.")
)


page_three <- tabPanel(
  h2("Vulnerability Assessment"),
  titlePanel(h3("The Vulnerability Assessment of Each Country")),
  plotlyOutput("inter_world_v_map"),
  p("Using the formula"),
  uiOutput("f"),
  p("we make this heat map with
    a number result representing the vulnerability associated with each country.
    From this map, we can see that USA is the most vulnerable country based
    on the factors we consider. Also, the countries in Eastern Hemisphere seems 
    to be less vulnerable than the countries in Western Hemisphere."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "countryp",
        label = "Choose the country you are interested in:",
        choices = total$Country,
        selected = "US"
      ),
      p("This pie chart shows the percentage distribution of the 6 factors in
        our vulnerability assessment framework for each country.")
    ),
    mainPanel(
      plotOutput("inter_country_pie"),
      h3("Table of Top 10 Vulnerable Countries During COVID-19 Pandemic", 
         style = "font-size: 15px")
    )
  ),
  tableOutput('table')
)



ui <- shinyUI(fluidPage(
  p("By Ryan & Wuwei (Public Health Track)", style = "font-size: 10px; text-align: left"),
  h1(span("Covid-19", style = "font-weight: 300; color: red",), 
     "Visualizations and Vulnerability Assessment"),
  includeCSS("style.css"),
  tabsetPanel(
    home,
    page_one,
    page_two,
    page_three
  )
))
