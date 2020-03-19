###############################################################################
## Population figures
## Author: Felicity Splatt
## Date: 2020-03-15
###############################################################################

setwd("C:/Users/fsplatt/Desktop/Bushfire smoke modelling")

library(readxl)

## geographic plotting libraries
library(rgdal)
library(tmap)
library(ggmap)

##-----------------------------------------------------------------------------

## Population (Males, Females, Persons), by LGA, 2018

# https://www.abs.gov.au/AUSSTATS/abs@.nsf/mf/3235.0

ds <- read_xls(
  path = "32350ds0004_lga_summary_statistics_2018.xls"
  ,sheet = "Table 1"
  ,range = "A6:G552"
  ,col_names = TRUE
  ,col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric")
  )
## note we're retaining LGA code as a character string for nwo


## table header had two rows, we're collapsing this into one
names(ds)[1:4] <- ds[1,1:4]

## remove spacing lines from table
ds <- ds[3:dim(ds)[1],]


## make colnames coding friendly
names(ds) <- gsub("(\\s+|\\/)", "_", names(ds))

##-----------------------------------------------------------------------------

## https://blog.exploratory.io/making-maps-for-australia-states-and-local-government-areas-in-r-d78edb506f37


## Digital shape files
## https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202019?OpenDocument


# Load Australian State and Territories shapefile data
LGA_2019 <- readOGR(dsn = file.path(getwd(), "1270055003_lga_2019_aust_shp"), layer = "LGA_2019_AUST")

plot(LGA_2019)

##-----------------------------------------------------------------------------

## compare overlap - amazing, they actually match

setdiff(LGA_2019$LGA_CODE19, ds$LGA_code)
setdiff(ds$LGA_code, LGA_2019$LGA_CODE19)

length(LGA_2019$LGA_CODE19)
length(ds$LGA_code)

##-----------------------------------------------------------------------------

## plot chloropleth map

## https://rpubs.com/JuanPabloHA/MapsVignetteJP

## join the datasets together
LGA_2019@data <- left_join(LGA_2019@data, ds, by = c("LGA_CODE19" = "LGA_code"))

## plot all of Australia
qtm(LGA_2019, "Persons")

## plot only NSW

## create a logical vector with NSW LGAs flagged (only)
NSW <- ds$S_T_code == 1

## plot only NSW
qtm(LGA_2019[NSW,], "Persons")
