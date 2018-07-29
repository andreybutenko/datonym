# exploration.R
# This file is just for fun, and contains some helpful functions for exploring
# the data set.

library(ggplot2)

PlotNameTrend <- function(name_) {
  ggplot(data = GetNameTrend(name_)) +
    geom_point(aes(x = year, y = count, color = gender))
}

GetNameTrend <- function(name_) {
  names.years %>%
    filter(name == name_) %>%
    arrange(year)
}

