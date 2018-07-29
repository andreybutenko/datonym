# Datonym

A project enabling you to find names that suit your preferences. Useful for developing stories, naming children, or just exploring.

I know I have preferences for certain kinds of names. This project enables myself and others to make those preferences concrete.

When it's done, it'll be live [here](https://datonym.me).

## Data source

Data is from the [SSA's dataset](https://www.ssa.gov/oact/babynames/limits.html).

It contains frequency of names of individuals with a Social Security Number born in a particular year.

Names with less than five occurrences are excluded, but that is okay for our purposes. Don't feel bad if your name is missing, it means you're special!

## Technology used

- [R](https://www.r-project.org/)
  - [dplyr](https://dplyr.tidyverse.org/) for analysis
  - [plumber](https://www.rplumber.io/) for api


