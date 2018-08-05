# exploration.R
# This file contains support functions for the API

library(tidyr)
library(ggplot2)

names.analy <- read.csv('./data/names3.csv')
name.trends <- read.csv('./ssa_names/ssa_names_years.csv')

GetSuggestions <- function(search_) {
  # TODO
}

GetSimilarNames <- function(name_) {
  # TODO
}

# For a given name_,  returns whether or not it exists in dataset.
NameExists <- function(name_) {
  names.analy %>% 
    filter(tolower(name) == tolower(name_)) %>%
    nrow(.) > 0
}

# For a given name_, returns a dataframe containing how many people having that
# name are born each year. name_ is case-insensitive.
GetNameTrend <- function(name_) {
  name.trends %>%
    filter(tolower(name) == tolower(name_)) %>%
    arrange(year)
}

# Given a dataframe with columns year and count, returns a dataframe where year
# column contains all years in range (min.year, max.year) with missing values
# in count filled in with NA.
FixChronological <- function(df, min.year = min(df$year), max.year = max(df$year)) {
  df <- select(df, year, count)
  
  for(i in min.year:max.year) {
    if(!i %in% df$year) {
      df <- add_row(df,
                    year = i,
                    count = NA)
    }
  }
  
  arrange(df, year)
}
