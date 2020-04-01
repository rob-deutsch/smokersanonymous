###############################################################################
## Deaths, SA2
## Author: Felicity Splatt
## Date: 2020-03-20
###############################################################################

## Deaths (Males, Females, Persons), by SA2
## download the "Table 4" csv from:
## https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3302.02018?OpenDocument

## ranges manually set, which isn't very nice
## also could neaten up this reading in process considerably, I imagine

ds1 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.1"
  ,range = "A8:AC706"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds2 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.2"
  ,range = "A8:AC554"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds3 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.3"
  ,range = "A8:AC638"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds4 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.4"
  ,range = "A8:AC217"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds5 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.5"
  ,range = "A8:AC306"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds6 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.6"
  ,range = "A8:AC128"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds7 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.7"
  ,range = "A8:AC89"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

ds8 <- read_xls(
  path = "33020do004_2018.xls"
  ,sheet = "Table_4.8"
  ,range = "A8:AC149"
  ,col_names = FALSE
  ,col_types = c(
  "text", "text"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric", "guess"
  ,"numeric", "numeric", "numeric"
  )
)

## put all these datasets together
ds <- rbind(ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8)
rm(ds1, ds2, ds3, ds4, ds5, ds6, ds7, ds8)

## remove the blank spacing columns
ds <- ds[, c(1:5, 7:9, 11:13, 15:17, 19:21, 23:25, 27:29)]

## create column headers
## v manual, could have read them in
years <- c(rep("2012", 3), rep("2013", 3), rep("2014", 3), rep("2015", 3), rep("2016", 3), rep("2017", 3), rep("2018", 3))
metric <- rep(c("Population", "Deaths", "Standardised_death_rate"), 3)

names(ds) <- c(
"code"
,"name"
,paste(years, metric, sep = "_")
)

rm(years, metric)

ds_deaths_SA2 <- ds[grep("\\d{9}", ds$code),]

rm(ds)

##-----------------------------------------------------------------------------

## create population and death numbers for 2019 and 2020
## linear regression over the years we have available

## design matrix
design_matrix <- as.matrix(cbind(1, 2012:2018))

population <-  grep("+.Population", names(ds_deaths_SA2))
deaths <- grep("+.Deaths", names(ds_deaths_SA2))

## we want standardised death rate, not deaths
death_rate <- grep("+.Standardised_death_rate", names(ds_deaths_SA2))


## response matrix
response_matrix_population <- ds_deaths_SA2[,population] %>% as.matrix() %>% t()

response_matrix_deaths <- ds_deaths_SA2[,deaths] %>% as.matrix() %>% t()
response_matrix_deaths[is.na(response_matrix_deaths)] <- 0

response_matrix_death_rate <- ds_deaths_SA2[,death_rate] %>% as.matrix() %>% t()
response_matrix_death_rate[is.na(response_matrix_death_rate)] <- 0

## extract fit coefficients

reg_P <- lm.fit(design_matrix, response_matrix_population)$coefficients %>% t() %>% as.data.frame()
reg_D <- lm.fit(design_matrix, response_matrix_deaths)$coefficients %>% t() %>% as.data.frame()
reg_DR <- lm.fit(design_matrix, response_matrix_death_rate)$coefficients %>% t() %>% as.data.frame()


rm(design_matrix, response_matrix_population, response_matrix_deaths, response_matrix_death_rate)

## create numbers for 2019 and 2020
## I'm doing this manually but there is probably a neater way

ds_deaths_SA2$`2019_Population` <- reg_P$x1 + 2019*reg_P$x2 
ds_deaths_SA2$`2019_Deaths` <- reg_D$x1 + 2019*reg_D$x2

ds_deaths_SA2$`2020_Population` <- reg_P$x1 + 2020*reg_P$x2 
ds_deaths_SA2$`2020_Deaths` <- reg_D$x1 + 2020*reg_D$x2 


ds_deaths_SA2$`2019_Standardised_death_rate` <- reg_DR$x1 + 2019*reg_DR$x2
ds_deaths_SA2$`2020_Standardised_death_rate` <- reg_DR$x1 + 2020*reg_DR$x2


rm(reg_D, reg_P, reg_DR)

## assess if these linear fits are reasonable

death_rate <- grep("+.Standardised_death_rate", names(ds_deaths_SA2))

## randomly select a few to look at
## seems reasonable
for (i in 1:5){
  plot(2012:2020, ds_deaths_SA2[sample(dim(ds_deaths_SA2)[1], 1),death_rate])
  }

rm(death_rate, deaths, i, population)
