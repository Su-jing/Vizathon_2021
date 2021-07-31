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
  h2("Direct Influences"),
  titlePanel(h3("The direct influences of Vovid-19, including deaths, recovered 
               cases, confirmed cases, and vaccinations in each country from year XXXX to XXXX")),
  sidebarLayout(
    sidebarPanel(
      # a `radioButtons` widget, the default choice is "Sure!"
      # radioButtons(
      #   inputId = "emotion",
      #   label = "Hi, are you excited to be going to use this app?",
      #   choices = list("Sure!" = "Thank you!",
      #                  "Emm...not really." =
      #                    "Hope you could change your idea later:)"),
      #   selected = "Thank you!"
      # ),
      # textOutput("greeting"),
      # a `selectInput` widget, the default choice is "Other Races"
      selectInput(
        inputId = "caseType",
        label = "Choose type of data you are interested in:",
        choices = c("Confirmed", "Death", "Recovered", "Vaccined"),
        selected = "Confirmed"
      )
    ),
    mainPanel(
      plotOutput("world_case_map"),
      plotlyOutput("inter_world_case_map")
    )
  )
)


page_two <- tabPanel(
  h2("Second-order Impacts"),
  titlePanel(h3("The second-order impacts of Covid-19, inspect from four aspects: gender, age, 
               history of diseases, and mental health")),
  sidebarLayout(
    sidebarPanel(
      # a `radioButtons` widget, the default choice is "Professional"
      radioButtons(
        inputId = "status",
        label = "Hi, are you a student or professional?",
        choices = list("Student" = "college education", "Professional"
                       = "profession"),
        selected = "profession"
      ),
      textOutput("suggestion"),
      
      # a `selectInput` widget, the default choice is "Profession"
      selectInput(
        inputId = "interest",
        label = "Choose a rate of interest here (not need to be the one
                  in suggesstion above):",
        choices = c("College_Education_Rates", "Profession_Rates"),
        selected = "Profession_Rates"
      )
    ),
    mainPanel(
      plotOutput("comparison_plot")
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
     "Vulnerability Assessment"),
  includeCSS("style.css"),
  tabsetPanel(
    home,
    page_one,
    page_two,
    page_three,
    page_four
  )
))
