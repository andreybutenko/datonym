##' For a given name, returns data from cache.
##' 
##' @param name_ Name to get statistics on.
GetNameStats <- function(name_) {
  stats.cache %>% 
    filter(name == name_)
}

# For a given name_, returns a dataframe containing how many people having that
# name are born each year.
GetNameTrend <- function(name_) {
  name.trends %>%
    filter(name == name_) %>%
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
