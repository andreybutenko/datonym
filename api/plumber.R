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
#' @param name The name
#' @get /name/<name_>
function(res, name_) {
  name <- name_
  
  if(missing(name)) {
    return(EmitError(res, 400, 'Requests to /trend must include ?name parameter'))
  }
  
  name <- tolower(name)
  
  if(!NameExists(name)) {
    return(EmitError(res, 404, paste0('No entries with name "', name, '" found')))
  }
  
  trend.df <- GetNameTrend(name)
  
  list(
    stats = GetNameStats(name),

    trends = list(
      male = trend.df %>%
        filter(gender == 'M') %>%
        select(-name, -gender),

      female = trend.df %>%
        filter(gender == 'F') %>%
        select(-name, -gender)
    )
  )
  
}

#' Return list of names matching preferences. See \link{FindNames} documentation
#' for information on parameters.
#' 
#' @get /find
function(res,
         vowel_factor = 0,
         syllable_factor = 0,
         symmetry_factor = 0,
         masculine_factor = 0,
         androgynity_factor = 0,
         trendy_factor = 0,
         common_factor = 0,
         classic_factor = 0,
         vowel_ending_factor = 0,
         double_letters_factor = 0) {
  
  vowel.factor <- as.numeric(vowel_factor)
  syllable.factor <- as.numeric(syllable_factor)
  symmetry.factor <- as.numeric(symmetry_factor)
  masculine.factor <- as.numeric(masculine_factor)
  androgynity.factor <- as.numeric(androgynity_factor)
  trendy.factor <- as.numeric(trendy_factor)
  common.factor <- as.numeric(common_factor)
  classic.factor <- as.numeric(classic_factor)
  vowel.ending.factor <- as.numeric(vowel_ending_factor)
  double.letters.factor <- as.numeric(double_letters_factor)
  
  FindNames(vowel.factor = vowel.factor,
            syllable.factor = syllable.factor,
            symmetry.factor = symmetry.factor,
            masculine.factor = masculine.factor,
            androgynity.factor = androgynity.factor,
            trendy.factor = trendy.factor,
            common.factor = common.factor,
            classic.factor = classic.factor,
            vowel.ending.factor = vowel.ending.factor,
            double.letters.factor = double.letters.factor)
}
