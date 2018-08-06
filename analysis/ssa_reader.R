# ssa_reader.R
# This script combines all the annual name statistic datasets provided by the SSA
# into two singular CSV files that are easier to work with directly.

# ssa_names_years.csv (columns: name, gender, year, count)
# Compiled datset on how many individuals had a given name/gender/year set.
# Essentially combines all the SSA datasets and adds a year column.
# This dataset is good if you want data on popularity over time.

# ssa_names.csv (columns: name, gender, count)
# Compiled dataset on how many individuals had a given name/gender pair across
# all datasets. For example, there are two separate rows for Alex: one for male
# and one for female. This dataset is good if you want data on overall
# popularity, with no respect for time.

library(dplyr)
library(stringr)

files <- list.files('./ssa_names/', pattern = '.txt')

# Given the file name of a SSA dataset, extracts its contents into a dataframe,
# parses the year out of the file name, and adds it as a column.
GetYearData <- function(file.name) {
  year.data <- read.csv(paste0('./ssa_names/', file.name), stringsAsFactors = F, header = F)
  colnames(year.data) <- c('name', 'gender', 'count')
  year.data$year = str_extract(file.name, "[0-9]+")
  return(year.data)
}

names.years <- data.frame()

for(file in files) {
  names.years <- rbind(names.years, GetYearData(file))
}

names.years %>% 
  mutate(name = tolower(name)) %>% 
  write.csv('./ssa_names/ssa_names_years.csv', row.names = F)

# Sums counts for each name/gender pair to create ssa_names.csv
names <- names.years %>% 
  group_by(name, gender) %>% 
  summarize(count = sum(count))

write.csv(names, '../ssa_names/ssa_names.csv', row.names = F)
