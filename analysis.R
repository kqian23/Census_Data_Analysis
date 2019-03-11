library("dplyr")
library("tidyr")
library("shiny")
library("ggrepel")
library("plotly")

# read raw data from xlsx file in the data folder
library(readxl)
census_all <- read_excel("data/Census 2010 Urban Centers and Villages Change from 1990 (1).xlsx", 
                         skip = 5)
census_all <- census_all %>% slice(1:43)    # remove description at the bottom


# split the data by years and then clean them up
# **********************************************************************
neigh_info <- census_all %>%    # extract first two columns
  select(1:2)

# extract columns of 2000 from raw data set and then reconstruct structure
census_2000 <- census_all %>% 
  select(23:47) %>%   
  select(-c(2,10:22)) %>% 
  mutate(region_name = neigh_info$NEIGH_NAME, region_num = neigh_info$NEIGH_NO)   # add the neighborhood columns
census_2000 <- census_2000[c(12,13,1:11)]   # reorder the columns
# rename the columns
colnames(census_2000)[3:13] <- c("total_population_2000", "white", "black", "american.indian_alaska.native",
                                 "asian", "native.awaiian_pacific.islander", "other_single_race",
                                 "two_or_more_races", "total_housing_units", "occupied_housing_units", "vacant_housing_units")


# extract columns of 2010 from raw data set and then reconstruct structure
census_2010 <- census_all %>% 
  select(48:72) %>% 
  select(-c(2,10:22))%>% 
  mutate(region_name = neigh_info$NEIGH_NAME, region_num = neigh_info$NEIGH_NO)  # add the neighborhood columns
census_2010 <- census_2010[c(12,13,1:11)]  # reorder the columns
# rename the columns
colnames(census_2010)[3:13] <- c("total_population_2010", "white", "black", "american.indian_alaska.native",
                                 "asian", "native.awaiian_pacific.islander", "other_single_race",
                                 "two_or_more_races", "total_housing_units", "occupied_housing_units", "vacant_housing_units")


# **********************************************************************
# Prepare Data for different visualizations
  # Prepare for Bar Chart
  census_2000_gathered <- census_2000 %>%   # reconstruct the structure
    gather(key = race_type, value = population_2000, -c(1,2,3,11:13)) %>% 
    mutate(occupied_housing_percentage_2000 = round(occupied_housing_units/total_housing_units * 100, 2)) %>% 
    mutate(percentage_in_region_2000 = round(population_2000/total_population_2000 * 100, 2)) %>% 
    select(1,7,8,9,10)
  
  census_2010_gathered <- census_2010 %>%   # reconstruct the structure
    gather(key = race_type, value = population_2010, -c(1,2,3,11:13)) %>% 
    mutate(occupied_housing_percentage_2010 = round(occupied_housing_units/total_housing_units * 100, 2)) %>% 
    mutate(percentage_in_region_2010 = round(population_2010/total_population_2010 * 100, 2)) %>% 
    select(1,7,8,9,10)
  
  # join two data frames
  census_joined <- census_2000_gathered %>% 
    mutate(population_2010 = census_2010_gathered$population_2010, 
    percentage_in_region_2010 = census_2010_gathered$percentage_in_region_2010,
    occupied_housing_percentage_2010 = census_2010_gathered$occupied_housing_percentage_2010) %>% 
    select(c(1,2,3,6,5,7,4,8)) %>% 
    mutate(percentage_changed_pop = round(percentage_in_region_2010 - percentage_in_region_2000, 2)) %>% 
    mutate(percentage_changed_housing = round(occupied_housing_percentage_2010 - occupied_housing_percentage_2000, 2)) 


  
# **********************************************************************
# Prepare for Change Plot
  