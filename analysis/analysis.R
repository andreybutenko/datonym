# analysis.R
# This script performs operations over the dataset to create interesting,
# insightful columns.

library(stringr)
library(tidyr)

# Config ----

kClassicCutoff = 2000 # year to split at
kTrendinessAge = 3 # years ago to split at

# Load data ----

names.analy <- read.csv('./ssa_names/ssa_names.csv', stringsAsFactors = F)
names.years <- read.csv('./ssa_names/ssa_names_years.csv', stringsAsFactors = F)

# Helpers ----

# Given a dataframe, a year cutoff, and a column of interest (as a string),
# return a  new dataframe where each row has a "new" and "old" column that
# is the average value of that column before and after the year cutoff.
# In the returned d ataframe, the col.name column has value (old - new)
SplitAtYear <- function(df, cutoff, col.name) {
  df %>% 
    mutate(old = as.numeric(year) < cutoff) %>% 
    group_by(name, gender, old) %>% 
    summarize(count = mean(count)) %>% 
    mutate(age = ifelse(old, 'old', 'new')) %>% 
    ungroup() %>% 
    select(name, gender, age, count) %>% 
    spread(key = age, value = count) %>% 
    mutate(
      new = ifelse(is.na(new), 0, new),
      old = ifelse(is.na(old), 0, old)
    ) %>% 
    mutate(score = old - new) %>% 
    rename(!!col.name := 'score')
}

# Given a character x, return whether or not it is a vowel.
IsVowel <- function(x) {
  sapply(x, function(ch) {
    return(str_to_lower(ch) %in% c("a", "e", "i", "o", "u", "y"))
  })
}

# Syllables ----

# Given a string str, returns approximate number of syllables.
CountSyllables <- function(str) {
  nchar(gsub('[^X]', '', gsub('[aeiouy]+', 'X', tolower(str))))
}

names.analy$syllables <- CountSyllables(names.analy$name) %>%
  pmax(1)

# Symmetry ----

# Given a vector of strings x, returns a vector with the symmetry score of each
# string, where the score is the percentage of pairs that are matching.  Additionally,
# vowels in corresponding places are worth half a point.
# "anna" -> 1       100% of the letters have matching pairs.
#                   (2 corresponding pair) / (2 total pairs) = 1
# "andrey" -> 0.166 No matching letters, but corresponding vowels (a/y) are present.
#                   (0.5 corresponding pairs) / (3 total pairs) = 0.166...
# "izzy" -> 0.75    There are matching z's (50%) and corresponding vowels (i/y).
#                   (1.5 corresponding pairs) / (2 total pairs) = 0.75
# "david" -> 0.6    Works with odd-length names. Matching d's and corresponding vowels (a/i).
#                   The "v" in the center is worth half a pair, but isn't considered matching.
#                   (1.5 corresponding pairs) / (2.5 total pairs) = 0.6
GetSymmetryScore <- function(x) {
  sapply(x, function(str) {
    str <- tolower(str)
    
    iterations <- str_length(str) / 2
    
    matches <- 0
    
    for(i in 1:iterations) {
      j = str_length(str) + 1 - i
      
      first <- substr(str, i, i)
      second <- substr(str, j, j)
      
      if(first == second) {
        matches = matches + 1
      }
      else if(IsVowel(first) && IsVowel(second)) {
        matches = matches + 0.5
      }
    }
    
    return(matches / iterations)
  })
}

names.analy$symmetry <- GetSymmetryScore(names.analy$name)

# Length ----

names.analy$length <- str_length(names.analy$name)

# Vowels ----

names.analy$vowels <- IsVowel(strsplit(names.analy$name, "")) %>% 
  lapply(sum) %>% 
  unlist()

names.analy$vowel.score <- names.analy$vowels / names.analy$length

names.analy$ends.in.vowel <- substr(names.analy$name,
                                    str_length(names.analy$name),
                                    str_length(names.analy$name)) %>% 
  IsVowel()

# Trendiness ----
# Trendiness is determined by the frequency of the name over the last
# kTrendinessAge years compared to before. Greater values are more
# popular now compared to before.

trendiness.cutoff <- names.years$year %>%
  unique() %>% 
  as.numeric() %>% 
  .[length(.) + 1 - kTrendinessAge]

names.analy$trendiness.score <- SplitAtYear(names.years, trendiness.cutoff, 'trendiness.score') %>% 
  select(name, gender, trendiness.score) %>% 
  left_join(select(names.analy, -one_of('trendiness.score')), by = c('name', 'gender')) %>% 
  pull(trendiness.score) * -1 # invert

# Classic ----
# Trendiness is determined by comparing the frequency of the name
# before and after kClassicCutoff. Greater values were more popular
# before compared to now.

names.analy$classic.score <- SplitAtYear(names.years, kClassicCutoff, 'classic.score') %>% 
  select(name, gender, classic.score) %>% 
  left_join(select(names.analy, -one_of('classic.score')), by = c('name', 'gender')) %>% 
  pull(classic.score)

# Masculine/Feminine ----
# Calculate masculinity and androgynity scores for names.

#' Gets a masculinity score for names, where names that are more common in males
#' are closer to 1 and names that are more common in women are closer to 1.
#' 
#' @param gender Gender of name ('M' or 'F')
#' @param count Count of name for that gender
#' @param total.count Total count of name across genders
GetMasculinity <- function(gender, count, total.count) {
  male.count <- ifelse(gender == 'M', count, total.count - count)
  return(male.count / total.count)
}

#' Get a androgynity score for a name, where names that are common in both
#' males and females are closer to 1, and other names are closer to 0.
#' 
#' @param masculinity Masculinity score, see \link{GetMasculinity}
GetAndrogynity <- function(masculinity) {
  (0.5 - abs(masculinity - 0.5)) * 2
}

names.analy <- names.analy %>% 
  group_by(name) %>% 
  summarize(total.count = sum(count)) %>% 
  left_join(names.analy, by = 'name') %>% 
  mutate(masculinity = GetMasculinity(gender, count, total.count),
         androgynity = GetAndrogynity(masculinity)) %>% 
  select(-total.count)

# Double Letters ----
# Determines whether names have double-letters (Nicollette, Alessandra)

GetDoubleness <- function(x) {
  sapply(x, function(str) {
    str <- tolower(str)
    
    iterations <- str_length(str) - 1
    
    matches <- 0
    
    for(i in 1:iterations) {
      curr.ch <- substr(str, i, i)
      next.ch <- substr(str, i + 1, i + 1)
      
      if(curr.ch == next.ch) {
        matches = matches + 1
      }
    }
    
    return(matches)
  })
}

names.analy$doubleness <- GetDoubleness(names.analy$name)

# Percentiles ----
# Create columns with percentile values of all numeric columns

GetPercentileScores <- function(v) {
  ecdf(v)(v)
}

is.numeric.cols <- lapply(names.analy, is.numeric)
numeric.cols <- names(is.numeric.cols)[unlist(is.numeric.cols)]

for(col.name in numeric.cols) {
  names.analy[[paste0(col.name, '.perc')]] <- GetPercentileScores(names.analy[[col.name]])
}

# Save ----

write.csv(names.analy, 'data/names4.csv', row.names = F)
