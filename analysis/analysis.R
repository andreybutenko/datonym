# analysis.R
# This script performs operations over the dataset to create interesting,
# insightful columns.

library(stringr)
library(tidyr)

# Config ----

kClassicCutoff = 2000 # year to split at
kTrendinessAge = 3 # years ago to split at

# Load data ----

names <- read.csv('./ssa_names/ssa_names.csv', stringsAsFactors = F)
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

names$syllables <- CountSyllables(names$name) %>%
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

names$symmetry <- GetSymmetryScore(names$name)

# Length ----

names$length <- str_length(names$name)

# Vowels ----

names$vowels <- IsVowel(strsplit(names$name, "")) %>% 
  lapply(sum) %>% 
  unlist()

# Trendiness ----
# Trendiness is determined by the frequency of the name over the last
# kTrendinessAge years compared to before. Greater values are more
# popular now compared to before.

trendiness.cutoff <- names.years$year %>%
  unique() %>% 
  as.numeric() %>% 
  .[length(.) + 1 - kTrendinessAge]

names$trendiness.score <- SplitAtYear(names.years, trendiness.cutoff, 'trendiness.score') %>% 
  select(name, gender, trendiness.score) %>% 
  left_join(select(names, -one_of('trendiness.score')), by = c('name', 'gender')) %>% 
  pull(trendiness.score) * -1 # invert

# Classic ----
# Trendiness is determined by comparing the frequency of the name
# before and after kClassicCutoff. Greater values were more popular
# before compared to now.

names$classic.score <- SplitAtYear(names.years, kClassicCutoff, 'classic.score') %>% 
  select(name, gender, classic.score) %>% 
  left_join(select(names, -one_of('classic.score')), by = c('name', 'gender')) %>% 
  pull(classic.score)

# Save ----

write.csv(names, 'data/names3.csv', row.names = F)

# Search by suffix ----

GetNamesWithSuffix <- function(df, suffix) {
  df %>%
    filter(endsWith(
      str_to_lower(name),
      str_to_lower(suffix)
    ))
}


# Searches ----
names %>% 
  filter(length > 2, count > 20) %>% 
  group_by(name) %>% 
  arrange(-symmetry, syllables, length, -vowels) %>% 
  View()

names %>% 
  filter(length > 2) %>% 
  arrange(-classic.score)
