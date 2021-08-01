library(shiny)
library(ggplot2)
library(lintr)

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
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "caseType",
        label = "Choose type of data you are interested in:",
        choices = c("Confirmed", "Death", "Recovered"),
        selected = "Confirmed"
      )
    ),
    mainPanel(
      plotlyOutput("inter_world_case_map"),
      p("blahhhhhhhhhhhhhhhh")
    )
  ),
  
  # daily change
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "country",
        label = "Choose country you are interested in:",
        choices = temp_c_total$Country.Region,
        selected = "US"
      )
    ),
    mainPanel(
      plotlyOutput("inter_country_case"),
      p("blahhhhhhhhhhhhhhhh")
    )
  )
)


page_two <- tabPanel(
  h2("Second-order Impacts"),
  titlePanel(h3("The second-order impacts of Covid-19, inspect from four aspects: gender, age, 
               history of diseases, and mental health")),
  
  # Mental Health
  h3("1) mental health blablablablablabla", style = "font-size:20px;"),
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
      plotlyOutput("mental_health_plot"),
      textOutput("mental_health_analysis")
    )
  ),
  
  # Median Age
  h3("2) median age blablablablablabla", style = "font-size:20px;"),
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
      plotlyOutput("median_age_plot"),
      textOutput("median_age_analysis")
    )
  ),
  
  # NCD
  h3("3) NCDs blablablablablabla", style = "font-size:20px;"),
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


page_four <- tabPanel(
  h2("Possible Bonus Page?"),
  titlePanel(h3("Word analysis from Tweeter")),
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


ui <- shinyUI(fluidPage(
  p("By Ryan & Wuwei", style = "font-size: 10px; text-align: left"),
  h1(span("Covid-19", style = "font-weight: 300; color: red",), 
     "Trend Visualization and Vulnerability Assessment"),
  includeCSS("style.css"),
  tabsetPanel(
    home,
    page_one,
    page_two,
    page_three,
    page_four
  )
))
