###############################################################################
## Deaths and Population, by SA2
## Author: Felicity Splatt
## Date: 2020-03-20
###############################################################################

setwd("C:/Users/fsplatt/Desktop/Bushfire smoke modelling")

library(readxl)
library(magrittr)

source("population_SA2.R")
source("deaths_SA2.R")

## compare why my SA2s are slightly different

## extract 4-digit SA2
deaths_SA2_codes <- substring(ds_deaths_SA2$code, 6, 9)

## hmm, suspicious, they're supposedly the same
## which means there must be a duplicate row somewhere?
setdiff(deaths_SA2_codes, ds_population_SA2$SA2_code)
setdiff(ds_population_SA2$SA2_code, deaths_SA2_codes)


## compare the population numbers

## pull out the ID and number we need from each
a <- ds_population_SA2[, c("SA2_code", "Population_2018_Persons")]
b <- ds_deaths_SA2[, c("code", "2018_Population")]

## convert the code so they match
b$code <- substring(ds_deaths_SA2$code, 6, 9)

## merge the two dataset
## the numbers agree
## something weird about SA2 code 1011
ds_combined <- merge(
  a
  ,b
  ,by.x = "SA2_code"
  ,by.y = "code"
)

## two problems:
## 1. the SA2 codes aren't exactly the same - there is probably a duplicate
## somewhere
## that join isn't working, something to do with code 1011?


## on the other hand, the population numbers agree (as we expect), so we can
## just use the ds_deaths_SA2 dataset because it has everything

rm(a, b, ds_combined, deaths_SA2_codes)
