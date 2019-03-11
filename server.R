library("shiny")
library("ggplot2")
library("plotly")

source("analysis.R")


server <- function(input, output) {
  
# Page: Explore by Region
# Pie Chart for race distribution
# **********************************************************************
  
  # stand alone reactive expression for shared data
  filtered_region <- reactive({
    data <- census_joined %>% 
      filter(region_name == input$selected_region)
    data  # return the data
  })
  
  # Introduction of the tab
  output$bar_chart_text <- renderText({
    text <- "This page provides interactive visualizations for the race distribution in Seattle in year 2000 and 2010. You may examine the distribution of different Seattle regions by selecting the dropdown menu. To compare the change of race distribution through years please see the 'percentage' tab."
    text
  })
  
  # Reactive introduction of the bar chart for 2000
  output$bar_chart_intro_2000 <- renderText({
    paste0("Race distribution of ", input$selected_region, " in 2000")
  })
  
  # Pie chart of race distribution in the selected region in 2000
  output$bar_chart_2000 <- renderPlotly({
    p <- plot_ly(filtered_region(), labels = ~race_type, values = ~percentage_in_region_2000, type = 'pie', 
                 textposition = 'outside', textinfo = 'label+percent', colors = "Set1") %>% 
      layout(title = "",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # Reactive introduction of the bar chart for 2010
  output$bar_chart_intro_2010 <- renderText({
    paste0("Race distribution of ", input$selected_region, " in 2010")
  })
  
  output$bar_chart_2010 <- renderPlotly({
    p <- plot_ly(filtered_region(), labels = ~race_type, values = ~percentage_in_region_2010, type = 'pie', 
                 textposition = 'outside', textinfo = 'label+percent', colors = "Set1") %>% 
      layout(title = "",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  
  # Bar Chart for percentage change
  # **********************************************************************
  # Introduction for the bar chart
  output$change_text <- renderText({
    text <- "This plot shows the change of percentage in the selected regions from 2000 to 2010. The vertical scale is self-adapted so the numbers may vary."
  })
  
  # Plot the bar chart
  output$change_plot <- renderPlot({
    change_plot <- ggplot(data = filtered_region()) +
      geom_col(mapping = aes(x = race_type, y = percentage_changed, fill = race_type)) +
      theme(axis.text.x = element_text(angle = 15)) +
      scale_fill_manual(values = c("white" = "#f9d6c3", "asian" = "#f7da66", "black" = "#3c2e28", "native.awaiian_pacific.islander" = "#b48a78",
                                   "american.indian_alaska.native" = "#854917", "other_single_race" = "#7fbf7b", "two_or_more_races" = "#af8dc3")) +
      geom_text(aes(x = race_type, y = ifelse(percentage_changed >= 0, percentage_changed + 0.25, percentage_changed - 0.25),
                                              label = paste0(percentage_changed, "%")), 
                position = position_dodge(width = 0.9), size = 3)
    
    change_plot
  })
  
  
  ## Page: Explore by Race
  # **********************************************************************
  # Plot for race population and percentage
  
  # Intro Text
  output$race_text <- renderText({
    text <- "These two plots show the top 10 regions with the greatest selected race. Data of 'City of Seattle Total' and 'Outside Villages' are removed due to uncomparable scale. This presentation focuses on inner Seattle regions. The horizontal scale of two plots are identical so that you may compare the population by examining the length of each bin visually."
  })
  
  # filter reactive data for 2000
  filtered_race_2000 <- reactive({
    data <- census_joined %>% 
      filter(race_type == input$selected_race & region_name != "City of Seattle Total" & region_name != "Outside Villages") %>% 
      arrange(-population_2000) %>% 
      head(10)
    data  # return the data
  })
  
  # filter reactive data for 2000
  filtered_race_2010 <- reactive({
    data <- census_joined %>% 
      filter(race_type == input$selected_race & region_name != "City of Seattle Total" & region_name != "Outside Villages") %>% 
      arrange(-population_2010) %>% 
      head(10)
    data  # return the data
  })
  
  # calculate the reactive limit for both plots based on the maximum population
  # so that both bar charts could be compared on the same scale for uasability
  ymax <- reactive({
    ymax <- max(filtered_race_2000()$population_2000[1], filtered_race_2010()$population_2010[1]) * 1.1
    ymax
  })

  # sort by population in 2000 and extract the top 10 regions
  output$race_2000 <- renderPlot({
    race_plot <- ggplot(data = filtered_race_2000()) +
      geom_col(mapping = aes(x = reorder(region_name, population_2000), y = population_2000, fill = region_name)) + 
      ylim(0, ymax()) +
      coord_flip()
    
    race_plot  # plot the bar chart for 2000
  })
  

  # sort by population in 2010 and extract the top 10 regions
  output$race_2010 <- renderPlot({
    race_plot <- ggplot(data = filtered_race_2010()) +
      geom_col(mapping = aes(x = reorder(region_name, population_2010), y = population_2010, fill = region_name)) +
      ylim(0, ymax()) +
      coord_flip()
    
    race_plot  # plot the bar chart for 2010
  })
  
  
  # Page: Race and House
  # **********************************************************************
  # filter the data corresponds to the input
  house <- reactive({
    data <- census_joined %>% 
      filter(race_type == input$house_race)
    data
  })
  
  output$house_text <- renderText({
    text <- "This page allows you to explore the relation between race and house occupied situation in a particular region from 2000 to 2010. Each dot represents a region in Seattle. The horizontal axis represents the precentage change of population of the selected race, and the vertical axis represents the percentage change of occupied housing units from 2000 to 2010. The direction of the slope indicates the type of correlation between those two variables(ex. positive, neutral or negative)"
  })
  
  # generate the plot for rafce and housing 
  output$house_plot <- renderPlot({
    house_plot <- ggplot(data = house(), mapping = aes(x = percentage_changed_pop, y = percentage_changed_housing)) +
                    geom_point(aes(color = region_name), show.legend = FALSE) +
                    geom_abline()
    house_plot
  })
    
    
  # Page: Source and Reflection Page
  # **********************************************************************
  # Insert a hyperlink for the data source
  url_source <- a("Seattle Department of Construction & Inspections", href = "http://www.seattle.gov/sdci")
  output$source <- renderUI({
      tagList("Data Source:", url_source)
  })
  
  # Q & A 
  # Ask reflection question
  output$Q <- renderText({
    text <- "Q: What additional data point will make this data presentation better?"
    text
  })
  
  # output the answer
  output$A <- renderText({
    text <- "A: First, I’ll be able to tell a better story if there are census data in multiple years, such that, instead of making a bar chart representing the change of percentage from one time point to another, I could make a series of scatter plot and then using a curve fit to show the trend. (Unfortunately the 1990 census data from the raw data file is not directly comparable to the 2000 and 2010 data.) A trend curve could tell us about if in a specific time period, a selected race (ex. Asian) increased rapidly, or hit a trough, which might be valuable for the user to draw insight with other information. Second, I believe that the presentation will be more interesting if I’m also provided with the economic situation of regions in Seattle. This is because I’m able to make a scatter plot showing the relationship between the economic situation of a particular region and the population of a selected race in that region, to see whether there’s a correlation between them. "
    text
  })
  
  
  # Page: Source and Reflection Page
  # **********************************************************************
  # Insert a hyperlink for my linkedin
  url_linkedin <- a("link", href = "https://www.linkedin.com/in/kun-qian-944478172/")
  output$linkedin <- renderUI({
    tagList("LinkedIn", url_linkedin)
  })
 
}
