library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)

server <- function(input, output) {
  # page one
  # render the greeting text
  output$greeting <- renderText({
    input$emotion
  })

  # wrangle the data first and then render the plot based on the user's input
  # select the `state` and population(in respect to race) columns,
  # group the data by state, change the column names
  race_pop_data <- midwest %>%
    na.omit() %>%
    select(state, popwhite, popblack, popamerindian, popasian, popother) %>%
    group_by(state) %>%
    summarise("Whites" = sum(popwhite), "Blacks" = sum(popblack),
              "American_Indians" = sum(popamerindian),
              "Asians" = sum(popasian), "Other_Races" = sum(popother))

  output$race_pop_plot <- renderPlot({
  # x-axis is the state, y-axis is the population of races of user's interest
  ggplot(data = race_pop_data) +
    geom_col(
      mapping = aes_string(x = "state", y = input$race),
      fill = "red",
      alpha = 0.7
    ) +
    labs(
      title = paste0("Population of \"", input$race, "\" in each state"),
      x = "State",
      y = input$race
    )
  })

  # page two
  # render the suggestion text
  output$suggestion <- renderText({
    paste0("You might be interested in the comparison between the ",
           input$status, " rates and the adult poverty rates.", "You
           can select the respective choice below.")
  })

  # wrangle the data first and then render the plot based on the user's input
  # select the `percadultpoverty`, `percollege` and `percprof` columns
  # change the column names
  comparison_data <- midwest %>%
    na.omit() %>%
    select(percadultpoverty, percollege, percprof)
  colnames(comparison_data) <- c("Adult_Poverty_Percent",
                              "College_Education_Rates",
                              "Profession_Rates")
  output$comparison_plot <- renderPlot({
    # x-axis is the percent of college education or profession rate
    # y-axis is the adult poverty rate
    ggplot(data = comparison_data) +
      geom_point(
        mapping = aes_string(x = input$interest, y = "Adult_Poverty_Percent",
                             color = "Adult_Poverty_Percent"), size = 5,
      ) +
      labs(
        title = paste0("Compare the \"", input$interest,
                       "\" to the \"Percent_of_Adult_Poverty\""),
        x = input$interest,
        y = "Percent_of_Adult_Poverty",
        color = "Percent_of_Adult_Poverty"
      )
  })
}
