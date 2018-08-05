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

#' Get distribution function for a column in a dataframe such that percentiles
#' can be calculated.
#' 
#' @param df The dataframe
#' @param col.name Name of the column
#' @examples
#' GetLengthPercentile <- GetDistFunction(names.analy, 'length')
#' GetLengthPercentile(2) # 0.002009762
#' GetLengthPercentile(6) # 0.5350412
#' GetLengthPercentile(9) # 0.9730951
GetDistFunction <- function(df, col.name) {
  ecdf(df[[col.name]])
}

#' Pre-calculate distribution functions for numeric columns in a dataframe.
#' 
#' @param df The dataframe
GetCachedDistFunctions <- function(df) {
  is.numeric.cols <- lapply(df, is.numeric)
  numeric.cols <- names(numeric.cols)[unlist(numeric.cols)]
  
  result <- lapply(numeric.cols, GetDistFunction, df = df)
  names(result) <- numeric.cols
  
  result
}

dist.functions <- GetCachedDistFunctions(names.analy)

GetPercentileScores <- function(v) {
  ecdf(v)(v)
}

FindNames <- function(vowel.factor = 0,
                      syllable.factor = 0,
                      symmetry.factor = 0,
                      masculine.factor = 0,
                      trendy.factor = 0,
                      common.factor = 0,
                      classic.factor = 0,
                      vowel.ending.factor = 0,
                      double.letters.factor = 0) {
  names.analy %>%
    mutate(score =
            (vowel.factor * GetPercentileScores(vowels / length)) +
            (syllable.factor * dist.functions$syllables(syllables)) +
            (symmetry.factor * dist.functions$symmetry(symmetry)) +
            (trendy.factor * dist.functions$trendiness.score(trendiness.score)) +
            (classic.factor * dist.functions$classic.score(classic.score))) %>% 
    arrange(-score)
}

# Which name do you find more attractive?
# or
# Find names with... (hover for more info)
#  more vowels, less vowels, no preference
#  more syllables, less syllables, no preference
#  more symmetry, less symmetry, no preference
#  more masculine, more feminine, no preference
#  more common, less common, no preference
#  more trendy, less trendy, no preference
#  more classic, less classic, no preference
#  more vowel endings, less vowel endings, no preference
#  more double-letters, less double-letters, no preference
#  randomize all

# vowel endings
# Ending in (ette)
# Double-lettes (nicollette, alessandra)
# UrbanDictionary?
names.analy %>% 
  arrange(-symmetry) %>% 
  filter(count > 10) %>% 
  View
