# exploration.R
# This file contains support functions for the API

library(tidyr)
library(ggplot2)

names.analy <- read.csv('./data/names4.csv')
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

#' Score names with a particular column as parameter.
#' 
#' @param column Column of interest as vector
#' @param factor The significance of this score, where greater absolute values
#'  are more significant.
GetScore <- function(column, factor = 0) {
  if(factor == 0) {
    return(0)
  }
  
  return(factor * column)
}

#' Score names with the evaluated result of a function as a parameter.
#' 
#' @param func Function to evaluate. Only evaluated if \code{factor} is not zero.
#' @param factor The significance of this score, where greater absolute values
#'  are more significant.
GetScoreEval <- function(func, factor = 0) {
  if(factor == 0) {
    return(0)
  }
  
  GetScore(func(), factor)
}

#' Return page of results for dataframe.
#' 
#' @param n Number of results to return.
#' @param n Number of results to return.
#' @param page Page of results to return.
GetPage <- function(df, n = 20, page = 1) {
  if(nrow(df) <= n) return(df)
  
  df %>% 
    head(n * page) %>% 
    tail(n)
}

FixDuplicates <- function(df, n = 20, page = 1) {
  return(df)
  
  need.to.fix <- df %>% 
    GetPage(n, page) %>% 
    pull(name) %>% 
    unique() %>% 
    length() < n
  
  print(paste('need.to.fix =', need.to.fix))
  
  if(!need.to.fix) return(df)
  
  # Get double usual range for worst-case scenario where all names are doubled.
  df <- df %>% 
    head(n * page + n) %>% 
    tail(n + n)
  
  df %>% 
    group_by(name) %>%
    summarize(n = n()) %>% 
    mutate(duplicated = n > 1,
           count = mean(count),
           score = mean(score),
           trendiness.score = mean(trendiness.score),
           classic.score = mean(classic.score)
           )
}

FindNames(vowel.factor = 1,
          double.letters.factor = -1,
          common.factor = 2,
          androgynity.factor = 100)

#' Find names matching preferences according to priority. Positive priorities
#' are prioritized, negative priorities are diminished, zero priorities are
#' not considered.
#' 
#' @param vowel.factor Vowel concentration priority.
#' @param syllable.factor Syllables priority.
#' @param masculine.factor Masculinity prioritiy.
#' @param androgynity.factor Androgynity priority.
#' @param trendy.factor Trendiness priority.
#' @param common.factor Commonality priority.
#' @param classic.factor Classicness priority.
#' @param vowel.ending.factor Vowel ending priority.
#' @param double.letters.factor Double letters priority.
#' @param count.cutoff Minimum number of occurrances of name to be included.
#' @param n Number of results to return.
#' @param page Page of results to return.
FindNames <- function(vowel.factor = 0,
                      syllable.factor = 0,
                      symmetry.factor = 0,
                      masculine.factor = 0,
                      androgynity.factor = 0,
                      trendy.factor = 0,
                      common.factor = 0,
                      classic.factor = 0,
                      vowel.ending.factor = 0,
                      double.letters.factor = 0,
                      count.cutoff = 0,
                      n = 20,
                      page = 1) {
  names.analy %>%
    filter(count > count.cutoff) %>% 
    mutate(score =
             GetScore(vowel.score.perc, vowel.factor) +
             GetScore(syllables.perc, syllable.factor) +
             GetScore(symmetry.perc, symmetry.factor) +
             GetScore(masculinity.perc, masculine.factor) +
             GetScore(androgynity.perc, androgynity.factor) +
             GetScore(trendiness.score.perc, trendy.factor) +
             GetScore(count.perc, common.factor) +
             GetScore(classic.score.perc, classic.factor) +
             GetScore(function() { ifelse(ends.in.vowel, 1, 0) }, vowel.ending.factor) +
             GetScore(doubleness.perc, double.letters.factor)) %>% 
    FixDuplicates(n, page) %>% 
    arrange(-score) %>% 
    head(n * page) %>% 
    tail(n)
}

FindNames(vowel.factor = 1,
          double.letters.factor = -1,
          common.factor = 2,
          androgynity.factor = 100)

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
# names.analy %>% 
#   arrange(-symmetry) %>% 
#   filter(count > 10) %>% 
#   View

GetNamesWithSuffix <- function(df, suffix) {
  df %>%
    filter(endsWith(
      str_to_lower(name),
      str_to_lower(suffix)
    ))
}
