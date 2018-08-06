library(tidyr)
library(dplyr)

names.analy <- read.csv('./data/names4.csv') # used for FindNames
name.trends <- read.csv('./ssa_names/ssa_names_years.csv') # used for trends
stats.cache <- read.csv('./data/cache/stats.csv') # used for stats

#' For a given name_, returns whether or not it exists in dataset.
#' 
#' @param name_ Name to check for.
NameExists <- function(name_) {
  stats.cache %>% 
    filter(name == name_) %>%
    nrow(.) > 0
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