# plumber.R
# https://www.rplumber.io/docs

source('./api/exploration/common.R')
source('./api/exploration/find_names.R')
source('./api/exploration/name_stats.R')

# Emit an error with given status code and msg
EmitError <- function(res, status, msg) {
  res$status <- status
  list(error = jsonlite::unbox(msg)) %>% 
    return()
}

#' Return data on name.
#' 
#' 
#' @param name The name
#' @get /name/<name_>
function(res, name_) {
  name <- name_
  
  if(missing(name)) {
    return(EmitError(res, 400, 'Requests to /trend must include ?name parameter'))
  }
  
  if(!NameExists(name)) {
    return(EmitError(res, 404, paste0('No entries with name "', name, '" found')))
  }
  
  trend.df <- GetNameTrend(name)
  
  list(
    stats = GetNameStats(name),
    
    trends = list(
      male = trend.df %>% 
        filter(gender == 'M') %>% 
        FixChronological(min.year = min(trend.df$year),
                         max.year = max(trend.df$year)),
      
      female = trend.df %>% 
        filter(gender == 'F') %>% 
        FixChronological(min.year = min(trend.df$year),
                         max.year = max(trend.df$year))
    )
  )
  
}

#' Return information on popularity of name over time.
#' @param name The name
#' @get /trend
function(res, name) {
  if(missing(name)) {
    return(EmitError(res, 400, 'Requests to /trend must include ?name parameter'))
  }
  
  if(!NameExists(name)) {
    return(EmitError(res, 404, paste0('No entries with name "', name, '" found')))
  }
  
  trend.df <- GetNameTrend(name)
  
  list(
    male = trend.df %>% 
      filter(gender == 'M') %>% 
      FixChronological(min.year = min(trend.df$year),
                       max.year = max(trend.df$year)),
    
    female = trend.df %>% 
      filter(gender == 'F') %>% 
      FixChronological(min.year = min(trend.df$year),
                       max.year = max(trend.df$year))
  )
}
