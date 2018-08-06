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
                      n = 30,
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
    arrange(-score) %>%
    GetPage(n, page) %>% 
    group_by(name) %>%
    summarize(count = sum(count),
              gender = ifelse(n() == 1, as.character(gender), 'A'),
              score = max(score)) %>%
    ungroup() %>%
    select(name, gender, score) %>%
    as.data.frame()
}

#' Score names with a particular column as parameter.
#' 
#' @param column Column of interest as vector
#' @param factor The significance of this score, where greater absolute values
#'  are more significant.
GetScore <- function(column, factor = 0) {
  if(factor == 0 || is.na(factor)) {
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
