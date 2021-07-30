library(shiny)
library(ggplot2)
library(lintr)

home <- tabPanel(
  h2("Home"),
  titlePanel(h3("General intruction of app")),
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
      radioButtons(
        inputId = "emotion",
        label = "Hi, are you excited to be going to use this app?",
        choices = list("Sure!" = "Thank you!",
                       "Emm...not really." =
                         "Hope you could change your idea later:)"),
        selected = "Thank you!"
      ),
      textOutput("greeting"),
      # a `selectInput` widget, the default choice is "Other Races"
      selectInput(
        inputId = "race",
        label = "Choose a race of interest here:",
        choices = c("Whites", "Blacks", "American_Indians",
                    "Asians", "Other_Races"),
        selected = "Other_Races"
      )
    ),
    mainPanel(
      plotOutput("race_pop_plot")
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
  h1(span("Covid-19", style = "font-weight: 300; color: red",), 
     "Vulnerability Assessment",
     style = "font-family: 'Source Sans Pro';
        color: #fff; text-align: center;
        background-image: url('texturebg.png');
        padding: 20px"),
  includeCSS("style.css"),
    tabsetPanel(
    home,
    page_one,
    page_two,
    page_three,
    page_four
  )
))
