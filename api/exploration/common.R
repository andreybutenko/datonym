library(tidyr)
library(ggplot2)

names.analy <- read.csv('./data/names4.csv')
name.trends <- read.csv('./ssa_names/ssa_names_years.csv')
stats.cache <- read.csv('./data/cache/stats.csv')

# For a given name_, returns whether or not it exists in dataset.
NameExists <- function(name_) {
  names.analy %>% 
    filter(tolower(name) == tolower(name_)) %>%
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

#' Merge rows that are gendered duplicates (i.e. combine Riley male and Riley female).
#' Rows keep value of higher-scoring row of duplicate pair, except count which is summed.
#' 
#' @param df The dataframe.
#' @param n Number of results to be displayed.
#' @param page Page of results to be displayed.
MergeGenderedDuplicates <- function(df, n = 20, page = 1) {
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
  
  encountered.names <- c()
  duplicate.names <- c()
  remove.indices <- c()
  
  # If a duplicate name is found, keep the first one because it is highest-scoring
  for(i in 1:nrow(df)) {
    row <- df[i,]
    name <- as.character(row$name)
    
    if(!name %in% encountered.names) {
      encountered.names <- c(name, encountered.names)
    }
    else {
      remove.indices <- c(i, remove.indices)
      duplicate.names <- c(name, duplicate.names)
    }
  }
  
  df %>% 
    # We want to sum the counts to be accurate
    group_by(name) %>% 
    summarize(count = sum(count)) %>% 
    as.data.frame() %>% 
    left_join(df, by = 'name') %>% 
    
    # Keep count.x
    select(-count.y) %>% 
    rename(count = count.x) %>% 
    
    # Remove duplicates
    mutate(index = row_number()) %>% 
    filter(!index %in% remove.indices) %>% 
    select(-index) %>% 
    mutate(gender = ifelse(name %in% duplicate.names, 'A', as.character(gender)))
}