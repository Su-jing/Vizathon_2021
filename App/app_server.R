library(shiny)
library(ggplot2)
library(dplyr)
library(lintr)
library(plotly)

daily_cases <- read.csv("data/WHO-COVID-19-global-data_comformed_death_by_date.csv", stringsAsFactors = FALSE)
cumu_cases <- read.csv("data/WHO-COVID-19-global-table-data_cumulative_confirmed_death_by_country.csv", stringsAsFactors = FALSE)
vaccine <- read.csv("data/WHO-vaccination-data.csv", stringsAsFactors = FALSE)
mental_health <- read.csv("data/mental_health_by_country.csv")

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
  
  # global average data
  mental_health_global <- mental_health[, -1]
  mental_health_global <- data.frame("avg" = colMeans(mental_health_global))
  output$mental_health_plot <- renderPlot({
    # line plot for mental health to show trend
    if (input$type1 == "Global trend of mental health (1992-2017)") {
      ggplot(data = mental_health_global, 
             aes(x = seq(1992,2017, by=1), y = avg)) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue") +
        labs(title = "Average Percentage of People with Mental and Substance Use Disorders", 
             x = "Year", y = "Percentage") +
        scale_x_continuous(breaks = seq(1992,2017, by=2)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year1
      ggplot(data = mental_health, aes(x = mental_health[,y-1990])) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of the Percentage of People with Mental and
Substance Use Disorders at", y),
             x = "Percentage",
             y = "Count")
    }
  })
  
  # mental health analysis
  output$mental_health_analysis <- renderText({
    "blablabla (analysis aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"
  })
}
