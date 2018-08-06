##' For a given name, returns data from cache.
##' 
##' @param name_ Name to get statistics on.
GetNameStats <- function(name_) {
  stats.cache %>% 
    filter(name == name_)
}

# For a given name_, returns a dataframe containing how many people having that
# name are born each year. name_ is case-insensitive.
GetNameTrend <- function(name_) {
  name.trends %>%
    filter(tolower(name) == tolower(name_)) %>%
    arrange(year)
}
