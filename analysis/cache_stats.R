library(tidyr)
library(dplyr)
library(parallel)
library(pbapply)

# helpful
# https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
# http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/

names.analy <- read.csv('./data/names4.csv', stringsAsFactors = F)

##' For a given name, returns all data from analysis file, merging conflicting values from
##' multi-gender names by appending '.m' or '.f'
##' 
##' @param name_ Name to get statistics on.
GetNameStats <- function(name_) {
  df <- filter(names.analy, name == name_)
  
  if(nrow(df) == 1) {
    return(select(df, -gender))
  }
  
  m.df <- filter(df, gender == 'M') %>% 
    select(-gender)
  
  f.df <- filter(df, gender == 'F') %>% 
    select(-gender, -ends.in.vowel, -doubleness, -doubleness.perc,
           -syllables, -syllables.perc, -symmetry, -symmetry.perc,
           -length, -length.perc, -vowel.score, -vowel.score.perc,
           -masculinity, -masculinity.perc, -androgynity, -androgynity.perc,
           -vowels, -vowels.perc)
  
  left_join(m.df, f.df, by = 'name', suffix = c('.m', '.f'))
}

names <- unique(names.analy$name)

# Set up cluster for multithreading
# Takes a long time. Even with 7 cores, takes 5+ minutes
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, 'names.analy')
clusterEvalQ(cluster, library(dplyr))

# Distribute work, then combine into a single dataframe
stats.list <- pblapply(names, GetNameStats, cl = cluster)
stats.df <- bind_rows(stats.list)

# Clean up
stopCluster(cluster)
write.csv(stats.df, './data/cache/stats.csv', row.names = F)
