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



dat <- countryExData

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
}
