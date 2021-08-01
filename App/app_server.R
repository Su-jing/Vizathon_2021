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
# preprocess of data

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
  
  # rename columns
  names(num_cases_1) <- c("Country", "Recovered", "Confirmed", "Death")
  #add column of iso code
  num_cases_1$iso <- countrycode(num_cases_1$Country, "country.name", "iso3c")
  #manually add unmatched code
  num_cases_1[num_cases_1$Country==	"Kosovo","iso"] = "KOS"
  
mental_health <- read.csv("data/mental_health_by_country.csv")
median_age <- read.csv("data/median_age_by_country.csv")
NCD <- read.csv("data/NCD_by_country.csv")

# Used for data merge
NCD2 <- read.csv("data/NCD_by_country2.csv")
mental_health2 <- read.csv("data/mental_health_by_country2.csv")
median_age2 <- read.csv("data/median_age_by_country2.csv")



# merge all tables together to 'total' for calculating vulnerability
temp_NCD <- NCD2 %>% select(iso, X2017)
total <- merge(num_cases_1, temp_NCD, by.x = "iso", by.y = "iso")
total <- total[c(1:184),]
total <- rename(total, NCD_value = X2017)
temp_mental_health <- mental_health2 %>% select(Code, percent)
total <- merge(total, temp_mental_health, by.x = "iso", by.y = "Code")
total <- rename(total, mental_health_percent = percent)
temp_median_age <- median_age2 %>% select(Code, median)
total <- merge(total, temp_median_age, by.x = "iso", by.y = "Code")
total <- rename(total, median_age = median)



server <- function(input, output) {
  # page one
  #Geographic heat map
  # 
  # #a static map
  # output$world_case_map <- renderPlot({
  #   # change country name to draw world map
  #   num_cases_1[num_cases_1$Country==	"North Macedonia","Country"] = "Macedonia"
  #   num_cases_1[num_cases_1$Country==	"Czechia","Country"] = "Czech Republic"
  #   num_cases_1[num_cases_1$Country==	"Eswatini","Country"] = "Swaziland"
  #   case_map <- joinCountryData2Map(num_cases_1,
  #                                   joinCode = "NAME",
  #                                   nameJoinColumn = "Country",
  #                                   verbose = TRUE)
  #   #getting class intervals using a ✬jenks✬ classification in classInt package
  #   classInt <- classIntervals( case_map[[input$caseType]], n=9, style="jenks")
  #   catMethod = classInt[["brks"]]
  #   # cannot change to snake_case because it is a library function
  #   colourPalette <- brewer.pal(9, "RdPu")
  #   m <- mapCountryData(case_map,
  #                       nameColumnToPlot = input$caseType,
  #                       colourPalette = colourPalette,
  #                       catMethod = catMethod,
  #                       numCats = 100,
  #                       addLegend = F
  #   )
  #   do.call( addMapLegend, c( m, legendLabels="all", 
  #                             legendWidth=0.8, legendIntervals="page", legendMar = 4 ))
  # })
  
  # an interactive map
  output$inter_world_case_map <- renderPlotly({
    world <- ne_countries(returnclass = "sf")
    # join tables
    world <- left_join(world, num_cases_1, by = c("adm0_a3"="iso"), copy = T)
    options(warn=-1)
    
    if(req(input$caseType) == "Confirmed") {
      p <- ggplot(world, aes(text = paste("Country: ", name), color = Confirmed)) +
        geom_sf(aes(fill =Confirmed)) +
        scale_fill_distiller(type = "seq", palette = "RdPu", na.value = "white", direction = 1) +
        scale_color_distiller(type = "seq", palette = "RdPu", na.value = "#fee3db", direction = 1) 
    } else if(req(input$caseType) == "Death") {
      p <- ggplot(world, aes(text = paste("Country: ", name), color = Death)) +
        geom_sf(aes(fill =Death))+
        scale_fill_distiller(type = "seq", palette = "GnBu", na.value = "white", direction = 1) +
        scale_color_distiller(type = "seq", palette = "GnBu", na.value = "#e6f5de", direction = 1) 
    } else if(req(input$caseType) == "Recovered") {
      p <- ggplot(world, aes(text = paste("Country: ", name), color = Recovered)) +
        geom_sf(aes(fill = Recovered))+
        scale_fill_distiller(type = "seq", palette = "Oranges", na.value = "white", direction = 1) +
        scale_color_distiller(type = "seq", palette = "Oranges", na.value = "#fee3ca", direction = 1) 
    } else if(req(input$caseType) == "Vaccined") {
      
      num_cases_1 <- merge(num_cases_1, temp_v_total,
                           by.x = 'Country.Region',
                           by.y = 'COUNTRY') %>%
        na.omit() 
      world <- left_join(world, num_cases_1, by = c("adm0_a3"="iso"), copy = T)
      options(warn=-1)
      p <- ggplot(world, aes(text = paste("Country: ", name))) +
        geom_sf(aes(color=Vaccined, fill = Vaccined))+
        scale_fill_distiller(type = "seq", palette = "Greens", na.value = "white", direction = 1) +
        scale_color_distiller(type = "seq", palette = "Greens", na.value = "#e6f5e1", direction = 1) 
    }
    p <- p + theme_light() + labs(title= paste0("Number of ", input$caseType, " Cases by Country"), 
                                  subtitle = paste0("(The Darker The Color, The More Cases)"))
    ggplotly(p, tooltip = c("text","colour"))
  })
  
  # interactive daily trend
  output$inter_country_case <- renderPlotly({
    dates <- seq(as.Date("2020-01-22"), as.Date("2021-07-30"), by="days")
    
    #process data
    df_d <- death %>% 
      filter(Country.Region == input$country) %>%
      select(Country.Region, 6:561) %>%
      group_by(Country.Region) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    df_d <- as.data.frame(t(df_d))
    colnames(df_d) = "Death"
    df_d <- df_d %>% 
      slice(-1) %>%
      mutate_at(vars(Death), as.numeric) %>%
      arrange(Death) 
    df_d$Date = dates
    rownames(df_d) <- NULL
    
    df_r <-recover %>% 
      filter(Country.Region == input$country) %>%
      select(Country.Region, 6:561) %>%
      group_by(Country.Region) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    df_r <- as.data.frame(t(df_r))
    colnames(df_r) = "Recovered"
    df_r <- df_r %>% 
      slice(-1) %>%
      mutate_at(vars(Recovered), as.numeric) %>%
      arrange(Recovered) 
    df_r$Date = dates
    rownames(df_r) <- NULL
    
    df_c <-confirm %>% 
      filter(Country.Region == input$country) %>%
      select(Country.Region, 6:561) %>%
      group_by(Country.Region) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    df_c<- as.data.frame(t(df_c))
    colnames(df_c) = "Confirmed"
    df_c <- df_c %>% 
      slice(-1) %>%
      mutate_at(vars(Confirmed), as.numeric) %>%
      arrange(Confirmed) 
    df_c$Date = dates
    rownames(df_c) <- NULL
    
    
    df_all <- merge(df_r, df_d,
                by.x = 'Date',
                by.y = 'Date')
    df_all <- merge(df_all, df_c,
                    by.x = 'Date',
                    by.y = 'Date')
    
    #draw a plot
    p <- ggplot(df_all, aes(x=Date, group = 1)) +
      geom_area(size = 0.5, color = "white", fill = "#f874a5", aes(y=Confirmed)) +
      geom_area(size = 0.5, color = "white", fill = "#fec28c", aes(y=Recovered)) +
      geom_area(size = 0.5, color = "white",  fill = "#08589e", aes(y=Death)) +
      labs(title = paste0("Number of Cases in ", input$country), 
           x = "Date", y = "Number") + theme_light()
    ggplotly(p)

  })
  
  
  # page two
  # global average data for public health
  mental_health_global <- mental_health[, -1]
  mental_health_global <- data.frame("avg" = colMeans(mental_health_global))
  output$mental_health_plot <- renderPlotly({
    # line plot for mental health to show trend
    if (input$type1 == "Global trend of mental health (1992-2017)") {
      p <- ggplot(data = mental_health_global, 
             aes(x = seq(1992,2017, by=1), y = avg, 
                 text = paste0("Year: ",  seq(1992,2017, by=1), "\nPercentage: ", avg))) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue", group = 1) +
        labs(title = "Average Percentage of People with Mental and Substance Use Disorders
(Global Trend 1992-2017)", 
             x = "Year", y = "Percentage (%)") +
        scale_x_continuous(breaks = seq(1992,2017, by=2)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year1
      p <- ggplot(data = mental_health, aes(x = mental_health[,y-1990])) +
        geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of the Percentage of People with Mental and
Substance Use Disorders at", y),
             x = "Percentage (%)",
             y = "Count (the number of countries)") +
        xlim(8, 20) + 
        ylim(0, 65)
    }
    ggplotly(p, tooltip = c("text"))
  })
  
  
  
  # global average data for median age
  median_age_global <- median_age[, -1]
  median_age_global <- data.frame("avg" = colMeans(median_age_global))
  output$median_age_plot <- renderPlotly({
    # line plot for median age to show trend
    if (input$type2 == "Global trend of median age (1950-2050)") {
      p <- ggplot(data = median_age_global, 
             aes(x = seq(1950,2050, by=5), y = avg, 
                 text = paste0("Year: ",  seq(1950,2050, by=5), "\nAge: ", avg))) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue", group = 1) +
        labs(title = "Average Median Age of People (Global Trend 1950-2050)", 
             x = "Year", y = "Age") +
        scale_x_continuous(breaks = seq(1950,2050, by=10)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year2
      p <- ggplot(data = median_age, aes(x = median_age[,(y-1950)/5+2])) +
        geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of the Median Age of People at", y),
             x = "Median Age",
             y = "Count (the number of countries)") +
        xlim(10, 55) +
        ylim(0, 50)
    }
    ggplotly(p, tooltip = c("text"))
  })
  
  
  
  # global average data for NCD
  NCD_global <- NCD[, -1]
  NCD_global <- data.frame("avg" = colMeans(NCD_global))
  output$NCD_plot <- renderPlotly({
    # line plot for NCD to show trend
    if (input$type3 == "Global trend of Non-communicable Diseases (1990-2017)") {
      p <- ggplot(data = NCD_global, 
             aes(x = seq(1990,2017, by=1), y = avg,
                 text = paste0("Year: ",  seq(1990,2017, by=1), "\nDALY Rates: ", avg))) +
        geom_point(color = "black", size = 2.5) +
        geom_line(size = 1, color = "blue", group = 1) +
        labs(title = "Average DALY Rates of NCDs (Global Trend 1990-2017)", 
             x = "Year", y = "DALY Rates (years)") +
        scale_x_continuous(breaks = seq(1990,2017, by=2)) +
        theme_bw(base_size = 13)
    } else {
      # Histograms for mental health by each year
      y = input$year3
      p <- ggplot(data = NCD, aes(x = NCD[,y-1988])) +
        geom_histogram(binwidth = 1000, color = "black", fill = "lightblue") +
        theme_bw(base_size = 13) +
        labs(title = paste("Histogram of DALY Rates of NCDs at", y),
             x = "DALY Rates (years)",
             y = "Count (the number of countries)") +
        xlim(5000, 50000) +
        ylim(0, 30)
    }
    ggplotly(p, tooltip = c("text"))
  })
  
  # NCD analysis
  output$NCD_analysis <- renderText({
    "blablabla (analysis aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"
  })
  
  
  
  #page three
  #scale data
  total[,"Recovered"] = total$Recovered/sum(total$Recovered)
  total[,"Confirmed"] = total$Confirmed/sum(total$Confirmed)
  total[,"Death"] = total$Death/sum(total$Death)
  total[,"NCD_value"] = total$NCD_value/sum(total$NCD_value)
  total[,"mental_health_percent"] = total$mental_health_percent/sum(total$mental_health_percent)
  total[,"median_age"] = total$median_age/sum(total$median_age)
  
  names(total) <- c("ISO3", "Country", "Recovered", "Confirmed", "Death", 
                    "NCD", "Mental.Health", "Age")
  #calculate vulnerability
  total <- total %>% 
    mutate(Vulnerability = ((Recovered*0.2 + Confirmed*0.2 + Death*0.2
                            + NCD*0.1 + Mental.Health*0.1 + Age*0.2)*100))
  
  #vulnerability map
  output$inter_world_v_map <- renderPlotly({
    world2 <- ne_countries(returnclass = "sf")
    # join tables
    world2 <- left_join(world2, total, by = c("adm0_a3"="ISO3"), copy = T)
    options(warn=-1)
    
    p <- ggplot(world2, aes(text = paste("Country: ", name, "\nVulnerability: ", Vulnerability, "%"), color = Vulnerability)) +
      geom_sf(aes(fill = Vulnerability)) +
      scale_fill_distiller(type = "seq", palette = "Greens", na.value = "white", direction = 1) +
      scale_color_distiller(type = "seq", palette = "Greens", na.value = "#e6f5e1", direction = 1) +
      theme_light() + 
      labs(title= paste0("Country Vulnerability Assessment")) 
    ggplotly(p, tooltip = c("text"))
  })
  
  output$inter_country_pie <- renderPlot({
    #pie chart for big-6 
      pie <- total %>% 
        filter(Country ==input$countryp) %>%
        select(Country, 3:8)
      pie2 <- data.frame(
        Factor=names(pie)[2:7],
        Value=as.numeric(as.vector(pie[1,2:7]))
      )
      # ggplot(pie2, aes(x="", y=Value, fill=Factor)) +
      #   geom_bar(stat="identity", width=1, color="white") +
      #   coord_polar("y", start=0) +
      #   theme_void() +
      #   scale_fill_brewer(palette = "Greens", 
      #                     labels = paste0(pie2$Factor, " ", format(round(pie2$Value*100, 2), nsmall = 2))) +
      #   theme(legend.position = "right")
      
      
      # Compute percentages
      pie2$fraction = pie2$Value / sum(pie2$Value)
      # Compute the cumulative percentages (top of each rectangle)
      pie2$ymax = cumsum(pie2$fraction)
      # Compute the bottom of each rectangle
      pie2$ymin = c(0, head(pie2$ymax, n=-1))
      # Make the plot
      ggplot(pie2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Factor)) +
        geom_rect() +
        coord_polar(theta="y") +
        xlim(c(1, 4)) +
        theme_void() +
        scale_fill_brewer(palette = "Greens", 
                          labels = paste0(pie2$Factor, " ", format(round(pie2$Value*100, 2), nsmall = 2))) +
        theme(legend.position = "right")
     
  })
  
  #rearrange total in asce order by vulnerability and get top 10
  top <- total %>%
    arrange(desc(Vulnerability)) %>%
    slice_head(n=10) %>%
    select(Country, Vulnerability)
  top <- as.data.frame(t(top))
  names(top) = top[1,]
  output$table <- renderTable(top[2,])
  
  
  
  # links
  url11 <- a("Bidirectional associations between COVID-19 and psychiatric disorder: 
           retrospective cohort studies of 62354 COVID-19 cases in the USA.", 
             href="https://www.thelancet.com/journals/lanpsy/article/PIIS2215-0366(20)30462-4/fulltext#seccestitle150")
  url12 <- a("Mental disorders and risk of COVID-19-related mortality, hospitalisation, 
             and intensive care unit admission: a systematic review and meta-analysis.",
             href="https://www.thelancet.com/journals/lanpsy/article/PIIS2215-0366(21)00232-7/fulltext")
  output$link1 <- renderUI({
    tagList("Reference 1: ", url11, "Reference 2: ", url12)
  })
  url2 <- a("The Elderly Are Most At Risk From The Coronavirus.",
            href="https://www.weforum.org/agenda/2020/03/coronavirus-covid-19-elderly-older-people-health-risk/")
  output$link2 <- renderUI({
    tagList("Reference: ", url2)
  })
  url3 <- a("COVID-19 a double blow for chronic disease patients.",
            href="https://www.sciencedaily.com/releases/2020/10/201023123141.htm")
  output$link3 <- renderUI({
    tagList("Reference: ", url3)
  })
}

