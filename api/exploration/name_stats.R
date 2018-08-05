##' For a given name, returns a list of statistics on that name. If both males
##' and females have that name, keys with conflicting data will be prepended
##' with 'm.' for the male value and 'f.' for the female value. Otherwise,
##' keys will just be the column name with nothing prepended.
##' 
##' @param name_ Name to get statistics on.
GetNameStats <- function(name_) {
  df <- names.analy %>% 
    filter(name == name_)
  
  if(nrow(df) == 1) {
    return(purrr::transpose(df)[[1]])
  }
  
  m.df <- filter(df, gender == 'M')
  f.df <- filter(df, gender == 'F')
  
  col.names <- colnames(m.df)
  
  for(col.name in col.names) {
    if(pull(m.df, col.name) != pull(f.df, col.name)) {
      m.df[[paste0('m.', col.name)]] <- pull(m.df, col.name)
      m.df[[paste0('f.', col.name)]] <- pull(f.df, col.name)
      select(m.df, -one_of(col.name))
    }
  }
  
  return(purrr::transpose(m.df)[[1]])
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
