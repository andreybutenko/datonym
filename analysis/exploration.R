# exploration.R
# This file contains support functions for the API


GetSuggestions <- function(search_) {
  # TODO
}

GetSimilarNames <- function(name_) {
  # TODO
}













# Which name do you find more attractive?
# or
# Find names with... (hover for more info)
#  more vowels, less vowels, no preference
#  more syllables, less syllables, no preference
#  more symmetry, less symmetry, no preference
#  more masculine, more feminine, no preference
#  more common, less common, no preference
#  more trendy, less trendy, no preference
#  more classic, less classic, no preference
#  more vowel endings, less vowel endings, no preference
#  more double-letters, less double-letters, no preference
#  randomize all

# vowel endings
# Ending in (ette)
# Double-lettes (nicollette, alessandra)
# UrbanDictionary?
# names.analy %>% 
#   arrange(-symmetry) %>% 
#   filter(count > 10) %>% 
#   View

GetNamesWithSuffix <- function(df, suffix) {
  df %>%
    filter(endsWith(
      str_to_lower(name),
      str_to_lower(suffix)
    ))
}
