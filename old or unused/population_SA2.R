###############################################################################
## Population, SA2
## Author: Felicity Splatt
## Date: 2020-03-20
###############################################################################

## Population (Males, Females, Persons), by SA2
## download the csv from:
## https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3235.02018?OpenDocument


## reads in some of the data
## you'll need to modify the range if you want more than just SA2
ds <- read_xls(
  path = "32350ds0002_sa2_summary_statistics_2013_2018.xls"
  ,sheet = "Table 2"
  ,range = "A17:I3704"
  ,col_names = TRUE
  ,col_types = c(
    "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric"
    )
  )

## add column names
colnames(ds) <- c(
  "GCCSA_code"
  ,"SA4_code"
  ,"SA3_code"
  ,"SA2_code"
  ,"other_name"
  ,"SA2_name"
  ,"Population_2018_Males"
  ,"Population_2018_Females"
  ,"Population_2018_Persons"
)

## just take the SA2 rows
ds_population_SA2 <- ds[!is.na(ds$SA2_code), c("SA2_code", "SA2_name", "Population_2018_Males", "Population_2018_Females", "Population_2018_Persons")]

rm(ds)
