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

recover <- read.csv("data/time_series_covid19_recovered_global.csv", stringsAsFactors = FALSE)
confirm <- read.csv("data/time_series_covid19_confirmed_global.csv", stringsAsFactors = FALSE)
death <- read.csv("data/time_series_covid19_deaths_global.csv", stringsAsFactors = FALSE)
vaccine <- read.csv("data/WHO-vaccination-data.csv", stringsAsFactors = FALSE)


# pre process of dfs
temp_r_total <- recover %>% 
  select(Country.Region, Total.recover) %>% 
  group_by(Country.Region) %>%
  summarise("Recover" = sum(Total.recover))

temp_c_total <- confirm %>% 
  select(Country.Region, Total.confirm) %>% 
  group_by(Country.Region) %>%
  summarise("Confirmed" = sum(Total.confirm))

temp_d_total <- death %>% 
  select(Country.Region, Total.death) %>% 
  group_by(Country.Region) %>%
  summarise("Death" = sum(Total.death))

temp_v_total <- vaccine %>% 
  select(COUNTRY, TOTAL_VACCINATIONS)

# merge cases data
num_cases_1 <- merge(temp_r_total, temp_c_total,
                     by.x = 'Country.Region',
                     by.y = 'Country.Region')
  
num_cases_1 <- merge(num_cases_1, temp_d_total,
                       by.x = 'Country.Region',
                       by.y = 'Country.Region') 

num_cases_1 <- merge(num_cases_1, temp_v_total,
                     by.x = 'Country.Region',
                     by.y = 'COUNTRY') %>%
  na.omit() 
# rename columns
names(num_cases_1) <- c("Country", "Recovered", "Confirmed", "Death", "Vaccined")


mental_health <- read.csv("data/mental_health_by_country.csv")
median_age <- read.csv("data/median_age_by_country.csv")
NCD <- read.csv("data/NCD_by_country.csv")


server <- function(input, output) {
  # page one
  #Geographic heat map
  #get world data
  world <- map_data('world')
  
  #try to draw world map
  ggplot(world, aes(x = long, y = lat, group = group)) +
    geom_polygon(color = 'black') 
  
  #a static map
  output$world_case_map <- renderPlot({
    # change country name to draw world map
    num_cases_1[num_cases_1$Country==	"North Macedonia","Country"] = "Macedonia"
    num_cases_1[num_cases_1$Country==	"Czechia","Country"] = "Czech Republic"
    num_cases_1[num_cases_1$Country==	"Eswatini","Country"] = "Swaziland"
    case_map <- joinCountryData2Map(num_cases_1,
                                    joinCode = "NAME",
                                    nameJoinColumn = "Country",
                                    verbose = TRUE)
    #getting class intervals using a ✬jenks✬ classification in classInt package
    classInt <- classIntervals( case_map[[input$caseType]], n=9, style="jenks")
    catMethod = classInt[["brks"]]
    # cannot change to snake_case because it is a library function
    colourPalette <- brewer.pal(9, "RdPu")
    m <- mapCountryData(case_map,
                        nameColumnToPlot = input$caseType,
                        colourPalette = colourPalette,
                        catMethod = catMethod,
                        numCats = 100,
                        addLegend = F
    )
    do.call( addMapLegend, c( m, legendLabels="all", 
                              legendWidth=0.8, legendIntervals="page", legendMar = 4 ))
  })
  
  # an interactive map
  output$inter_world_case_map <- renderPlotly({
    #add column of iso code
    num_cases_1$iso <- countrycode(num_cases_1$Country, "country.name", "iso3c")
    #manually add unmatched code
    num_cases_1[num_cases_1$Country==	"Kosovo","iso"] = "KOS"
    #num_cases_1 <- num_cases_1 %>% na.omit()
    
    world <- ne_countries(returnclass = "sf")
    # join tables
    world <- left_join(world, num_cases_1, by = c("adm0_a3"="iso"), copy = T)
    options(warn=-1)
    
    # input$caseType
    if(req(input$caseType) == "Confirmed") {
      p <- ggplot(world, aes(text = paste("Country:", name), color = Confirmed)) +
        geom_sf(aes(fill =Confirmed))
    } else if(req(input$caseType) == "Death") {
      p <- ggplot(world, aes(text = paste("Country:", name), color = Death)) +
        geom_sf(aes(fill =Death))
    } else if(req(input$caseType) == "Recovered") {
      p <- ggplot(world, aes(text = paste("Country:", name), color = Recovered)) +
        geom_sf(aes(fill = Recovered))
    } else if(req(input$caseType) == "Vaccined") {
      p <- ggplot(world, aes(text = paste("Country:", name), color = Vaccined)) +
        geom_sf(aes(fill =Vaccined))
    }
    p <- p +
      scale_fill_distiller(palette = "RdPu", na.value = "white") +
      scale_color_distiller(palette = "RdPu", na.value = "lightgray") +
      theme_light()
    ggplotly(p)
  })
  
  # render the greeting text
  # output$greeting <- renderText({
  #   input$emotion
  # })

  # wrangle the data first and then render the plot based on the user's input
  # select the `state` and population(in respect to race) columns,
  # group the data by state, change the column names
  # race_pop_data <- midwest %>%
  #   na.omit() %>%
  #   select(state, popwhite, popblack, popamerindian, popasian, popother) %>%
  #   group_by(state) %>%
  #   summarise("Whites" = sum(popwhite), "Blacks" = sum(popblack),
  #             "American_Indians" = sum(popamerindian),
  #             "Asians" = sum(popasian), "Other_Races" = sum(popother))
  # 
  # output$race_pop_plot <- renderPlot({
  # # x-axis is the state, y-axis is the population of races of user's interest
  # ggplot(data = race_pop_data) +
  #   geom_col(
  #     mapping = aes_string(x = "state", y = input$race),
  #     fill = "red",
  #     alpha = 0.7
  #   ) +
  #   labs(
  #     title = paste0("Population of \"", input$race, "\" in each state"),
  #     x = "State",
  #     y = input$race
  #   )
  # })

  
  
  
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
  
  # global average data for public health
  mental_health_global <- mental_health[, -1]
  mental_health_global <- data.frame("avg" = colMeans(mental_health_global))
  output$mental_health_plot <- renderPlot({
    # line plot for mental health to show trend
    if (input$type1 == "Global trend of mental health (1992-2017)") {
      ggplot(data = mental_health_global, 
             aes(x = seq(1992,2017, by=1), y = avg)) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue") +
        labs(title = "Average Percentage of People with Mental and Substance Use Disorders
(Global Trend 1992-2017)", 
             x = "Year", y = "Percentage (%)") +
        scale_x_continuous(breaks = seq(1992,2017, by=2)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year1
      ggplot(data = mental_health, aes(x = mental_health[,y-1990])) +
        geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of the Percentage of People with Mental and
Substance Use Disorders at", y),
             x = "Percentage (%)",
             y = "Count (the number of countries)") +
        xlim(8, 20) + 
        ylim(0, 65)
    }
  })
  
  # mental health analysis
  output$mental_health_analysis <- renderText({
    "blablabla (analysis aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"
  })
  
  
  # global average data for median age
  median_age_global <- median_age[, -1]
  median_age_global <- data.frame("avg" = colMeans(median_age_global))
  output$median_age_plot <- renderPlot({
    # line plot for median age to show trend
    if (input$type2 == "Global trend of median age (1950-2050)") {
      ggplot(data = median_age_global, 
             aes(x = seq(1950,2050, by=5), y = avg)) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue") +
        labs(title = "Average Median Age of People (Global Trend 1950-2050)", 
             x = "Year", y = "Age") +
        scale_x_continuous(breaks = seq(1950,2050, by=10)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year2
      ggplot(data = median_age, aes(x = median_age[,(y-1950)/5+2])) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of the Median Age of People at", y),
             x = "Median Age",
             y = "Count (the number of countries)") +
        xlim(10, 55) +
        ylim(0, 50)
    }
  })
  
  # mental health analysis
  output$median_age_analysis <- renderText({
    "blablabla (analysis aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"
  })
  
  
  # global average data for NCD
  NCD_global <- NCD[, -1]
  NCD_global <- data.frame("avg" = colMeans(NCD_global))
  output$NCD_plot <- renderPlot({
    # line plot for NCD to show trend
    if (input$type3 == "Global trend of Non-communicable Diseases (1990-2017)") {
      ggplot(data = NCD_global, 
             aes(x = seq(1990,2017, by=1), y = avg)) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue") +
        labs(title = "Average DALY Rates of NCDs (Global Trend 1990-2017)", 
             x = "Year", y = "DALY Rates (years)") +
        scale_x_continuous(breaks = seq(1990,2017, by=2)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year3
      ggplot(data = NCD, aes(x = NCD[,y-1988])) +
        geom_histogram(binwidth = 1000, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of DALY Rates of NCDs at", y),
             x = "DALY Rates (years)",
             y = "Count (the number of countries)") +
        xlim(5000, 50000) +
        ylim(0, 30)
    }
  })
  
  # NCD analysis
  output$NCD_analysis <- renderText({
    "blablabla (analysis aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"
  })
}
