
#### ---- Load libraries and set up parameters ---- ####

library(data.table)
library(stringr)
library(ggplot2)
library(sf)
library(biogeo)

folder <- "N:\\Perils20\\R&D\\Smoke\\Data\\"
crs_code <- 4326
crs_projected <- 3577

#### ---- Read in the data ---- ####

smoke <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\Smoke NSW Nov19 Feb20.xlsx', startRow = 2, colNames = T)
setDT(smoke)

baseline_smoke <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\Smoke NSW Baseline.xlsx', startRow = 2, colNames = T)
setDT(baseline_smoke)

stations_loc <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\air-quality-monitoring-sites-summary.xlsx', colNames = T, 
                                    sheet = 'Monitoring stations list')
setDT(stations_loc)

# Shapefiles for states and SA2 Units
states <- st_read('M://finpoint19//R&D//Spatial Data//ABS_Shapefiles//State//STE_2016_AUST.shp')
NSW <- states[states$STE_NAME16 == "New South Wales", ]

rm(states)

sa2 <- st_read('N:\\Perils20\\Climate\\Smoke\\Data\\1270055001_sa2_2016_aust_shape\\SA2_2016_AUST.shp')
sa2_NSW <- sa2[sa2$STE_NAME16 == "New South Wales",]

# Island off the coast of Australia - makes mapping & geospatial calcs more difficult so I have removed
sa2_NSW <- sa2_NSW[sa2_NSW$SA2_MAIN16 != '108031161',]

sa2_codes <- unique(sa2_NSW$SA2_MAIN16)

rm(sa2)
gc()

# Deaths & Population
deaths <- fread('N:\\Perils20\\Climate\\Smoke\\Data\\Processed Data\\std_deaths_SA2.csv')

# Sense check of population of NSW
NSW_pop20 <- sum(deaths[code %in% unique(sa2_NSW$SA2_MAIN16),]$`2020_Population`) #8200327
NSW_pop19 <- sum(deaths[code %in% unique(sa2_NSW$SA2_MAIN16),]$`2019_Population`) #8086339

deaths <- deaths[code %in% sa2_codes,]

#### ---- Clean up the data ---- ####
new_col_names_base <- sapply(str_split(colnames(baseline_smoke), '.PM2.5'), `[`, 1)
colnames(baseline_smoke) <- new_col_names_base

new_col_names_smoke <- sapply(str_split(colnames(smoke), '.PM2.5'), `[`, 1)
colnames(smoke) <- new_col_names_smoke

baseline_smoke[, Date:= as.Date(Date, format = "%d/%m/%Y")]
smoke[, Date:= as.Date(Date, format = "%d/%m/%Y")]

# Go up to the end of Feb 2020
smoke <- smoke[Date < as.Date("2020-03-01"),]

baseline_smoke <- melt(baseline_smoke, id.vars = 'Date', variable.name = 'location', value.name = 'avg_PM2.5')
smoke <- melt(smoke, id.vars = 'Date', variable.name = 'location', value.name = 'avg_PM2.5')

# Get geocoding for stations
stations_loc <- janitor::clean_names(stations_loc)

stations_loc <- stations_loc[, c('nsw_air_quality_monitoring_aqmn_site', 'aqmn_region', 'sub_region_where_applicable', 'latitude_south', 
                                 'longitude_east', 'status')]

# Correct names so it is possible to use these merge
smoke[,location:= gsub('\\.', ' ', tolower(location))]
smoke[location == 'wagga wagga nth', location:= 'wagga wagga north']
smoke[location == 'albion park sth', location:= 'albion park south']

locations <- unique(smoke$location)

stations_loc[, location:= trimws(tolower(nsw_air_quality_monitoring_aqmn_site), which = 'both')]

# Manual corrections - should not be necessary but I can't work out why R doesn't fix this above
stations_loc[74, location:= 'prospect'] 
stations_loc[3, location:= 'camberwell']

# This error also occurs with lat/long: fix as well
stations_loc[location == 'camberwell', latitude_south:= "32°28'20"] 
stations_loc[location == 'camberwell', longitude_east:= "151°5'31"] 

# Cut down to stations for which we have smoke observations
stations_smoke <- stations_loc[location %in% locations,]

# Clean up geocoding: Latitude
stations_smoke[, latitude_south:=  trimws(gsub(' ', '', latitude_south), which = 'both')]
stations_smoke[, latitude_south:=  gsub("'", "'", latitude_south)]

latitude1 <- as.data.table(stringr::str_split_fixed(stations_smoke$latitude_south, '°', n = 2))
latitude1[47, V2:= "6'11.52''"]
latitude1[47, V1:= 35]
latitude1[, V1:= as.integer(V1)]

latitude2 <- as.data.table(stringr::str_split_fixed(latitude1$V2, "'", n = 2))
latitude2[, V2:= gsub('"', '', V2)]
latitude2[, V2:= gsub("'", '', V2)]
latitude2[, V2:= as.numeric(gsub(""", '', V2))]
latitude2[, V1:= as.integer(V1)]

latitude <- cbind(latitude1[,1],latitude2)
colnames(latitude) <- c('V1', 'V2', 'V3')
latitude[, latitude_clean:= dms2dd(V1, V2, V3, ns = "S")]

stations_smoke[, latitude:= latitude$latitude_clean]


# Clean up geocoding: Longitude
stations_smoke[, longitude_east:=  trimws(gsub(' ', '', longitude_east), which = 'both')]

longitude1 <- as.data.table(stringr::str_split_fixed(stations_smoke$longitude_east, '°', n = 2))
longitude1[47, V2:= "21'35.28''"]
longitude1[47, V1:= 147]
longitude1[, V1:= as.integer(V1)]

longitude1[, V2:= gsub(''', "'", V2)]

longitude2 <- as.data.table(stringr::str_split_fixed(longitude1$V2, "'", n = 2))
longitude2[, V2:= gsub('"', '', V2)]
longitude2[, V2:= gsub("'", '', V2)]
longitude2[, V2:= as.numeric(gsub(""", '', V2))]
longitude2[, V1:= as.integer(V1)]

longitude <- cbind(longitude1[,1],longitude2)
colnames(longitude) <- c('V1', 'V2', 'V3')
longitude[, longitude_clean:= dms2dd(V1, V2, V3, ns = "E")]

stations_smoke[, longitude:= longitude$longitude_clean]
stations_smoke[, longitude:= dms2dd(degrees_long, minutes_long, seconds_long, ns = "E")]

stations_smoke[, c('latitude_south', 'longitude_east'):= NULL]
stations_smoke <- stations_smoke[!duplicated(location)]

fwrite(stations_smoke, 'N:\\Perils20\\Climate\\Smoke\\Data\\Output\\stations.csv')

#### ---- Exposure assessment ---- ####

#### Identification of smokey days: Days where the pm2.5 concentration exceeds the 99th percentile of the base period

# EDA to get understanding of base period

# Number of non-null observations in baseline period 
baseline_smoke[, .(no_observations = sum(!is.na(avg_PM2.5))), by = location]

# Plot to assess continuity of observations
ggplot(baseline_smoke[location %in% c('LIVERPOOL', 'CHULLORA', 'EARLWOOD', 'RICHMOND', 'PARRAMATTA.NORTH', 'CAMDEN', 'MUSWELLBROOK', 'SINGLETON', 'WAGGA.WAGGA.NTH')], 
       aes(x = Date , y = avg_PM2.5)) +
  geom_line() +
  facet_wrap(.~location)

# Get 99th percentile of base period
perc99 <- baseline_smoke[, quantile(avg_PM2.5, 0.99, na.rm = T)]

# Flag observations considered "smokey" i.e. above 99th percentile of base period
smoke[avg_PM2.5 >= perc99, smokey_flag:= 1]
smoke[avg_PM2.5 < perc99, smokey_flag:= 0]

#### Estimation of the average smoke concentration at monitor locations 

# Average concentration by location for smokey and non nsmokey days
smoke_related_PM2.5_conc <- smoke[!is.na(smokey_flag), .(mean_avg_PM2.5 = mean(avg_PM2.5)), by = c('location', 'smokey_flag')]

smoke_related_PM2.5_conc <- dcast(smoke_related_PM2.5_conc, location ~ smokey_flag)

#### Estimation of the population weighted mean concentrations

# Create rectangle covering NSW
NSW_area <- st_sfc(st_polygon(list(rbind(c(140.98, -28.15), c(153.65, -28.15), c(153.65, -37.5),
                                            c(140.98, -37.5), c(140.98, -28.15)))))

plot(NSW_area)

# Create grid across this rectangle
NSW_grid <- st_make_grid(NSW_area, n = 1000, crs = crs_code, what = 'centers')
st_crs(NSW_grid) <- crs_code

# Cut down to grid points in NSW: remove those in sea for faster calculations
NSW_grid <- st_join(st_sf(NSW_grid), st_transform(NSW, crs_code), join = st_intersects)
NSW_grid <- NSW_grid[!is.na(NSW_grid$STE_CODE16),]

NSW_grid_proj <- st_sf(st_transform(NSW_grid, crs = crs_projected))
NSW_grid_proj$id <- 1:nrow(NSW_grid_proj)

# Merge station information with average smoke for each
stations_smoke <- merge(stations_smoke, smoke_related_PM2.5_conc[, c('location', '0', '1')], by = 'location')

stations_smoke_sf <- st_as_sf(stations_smoke, coords = c('longitude', 'latitude'), crs = crs_code)
stations_smoke_proj <- st_transform(stations_smoke_sf, crs = crs_projected)

distances <- st_distance(stations_smoke_proj, NSW_grid_proj)

distances_df <- as.data.table(data.frame(matrix(distances, nrow = nrow(stations_smoke_proj), ncol = nrow(NSW_grid_proj))))
distances_df[, location:= stations_smoke_proj$location]

setcolorder(distances_df, neworder = c('location', paste0('X', 1:nrow(NSW_grid_proj))))

distances_melted <- melt(distances_df, id.vars = 'location', value.name = 'distance', variable.name = 'id')
distances_melted[, id:= as.integer(gsub('X', '', id))]

distances_melted <- merge(distances_melted, stations_smoke[, c('location', 'latitude', 'longitude', '0', '1')],
                          by = 'location', all.x = T)

NSW_grid_DT <- as.data.table(st_coordinates(NSW_grid)[, c('X', 'Y')])
NSW_grid_DT[,id:= .I]

distances_melted <- merge(distances_melted, NSW_grid_DT, by = 'id' )

#### ---- Get weighted average by distance for each point ---- ####

distances_melted[, distance_wght:= (1/distance)^2]
distances_melted[, total:= sum(distance_wght), by = id]
distances_melted[, proportion:= distance_wght / total]

distances_melted[, c('distance_wght', 'total'):= NULL]

smoke[, month:= month(Date)]
smoke[, week:= week(Date)]

smoke_complete <- smoke[!is.na(avg_PM2.5),]

# Too big to merge all at once 
smoke_dist1 <- merge(smoke_complete[week <= 46 & week >= 44,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts1 <- smoke_dist1[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist1)

smoke_dist2 <- merge(smoke_complete[week <= 50 & week > 46,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts2 <- smoke_dist2[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist2)

smoke_dist3 <- merge(smoke_complete[week <= 53 & week > 50,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts3 <- smoke_dist3[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist3)

smoke_dist4 <- merge(smoke_complete[week <= 3 & week >= 1,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts4 <- smoke_dist4[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist4)

smoke_dist5 <- merge(smoke_complete[week <= 6 & week > 3,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts5 <- smoke_dist5[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist5)

smoke_dist6 <- merge(smoke_complete[week <= 9 & week > 6,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_pts6 <- smoke_dist6[!is.na(id), .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
rm(smoke_dist6)
gc()

smoke_pts <- rbind(smoke_pts1, smoke_pts2, smoke_pts3, smoke_pts4, smoke_pts5, smoke_pts6)

rm(smoke_pts1, smoke_pts2, smoke_pts3, smoke_pts4, smoke_pts5, smoke_pts6)
gc()

smoke_pts <- merge(smoke_pts, NSW_grid_DT, by = 'id', all.x = T)

ggplot(smoke_pts[Date == as.Date("2019-12-10"),], aes(x = X, y = Y, colour = weighted_smoke, fill = weighted_smoke)) +
  geom_point() +
  coord_fixed() +
  viridis::scale_colour_viridis(option = "magma", direction = -1) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, guide = 'none') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = 'Interpolated PM2.5 \nConcentration') +
  ggtitle("Air Quality in Sydney on the 10th of December 2019") 
  
# Get average non-smokey PM 2.5 concentration for each grid points 
non_smokey_baseline <- distances_melted[, .(non_smoky_weighted = sum(`0` * proportion, na.rm = T)), by = c('id')]


#### ---- SA2 level analysis ---- ####

# Check which SA2 unit each grid point intersects with 
intersections <- as.data.table(st_join(NSW_grid, st_transform(sa2_NSW, crs_code), join = st_intersects))
intersections <- cbind(intersections[, SA2_MAIN16], as.data.table(st_coordinates(NSW_grid)))
setnames(intersections, 'V1', 'sa2_code')
intersections[, id:= .I]

smoke_pts_sa2 <- merge(smoke_pts[, -c('X', 'Y')], intersections, by = 'id', all.x = T)
smoke_pts_sa2 <- merge(smoke_pts_sa2, non_smokey_baseline, by = 'id', all.x = T)

# Summarise all points in each SA2 unit
smoke_sa2 <- smoke_pts_sa2[, .(mean_weighted_smoke = mean(weighted_smoke), mean_weighted_non_smoke = mean(non_smoky_weighted)),
                           by = c('sa2_code', 'Date')]

#### ---- For map ---- ####

# Take mean across November to February
smoke_sa2_means_all_time <- smoke_sa2[,.(mean_smoke_all_time = mean(mean_weighted_smoke)), by = sa2_code]

# Merge with spatial object for map
sa2_NSW_means <- merge(sa2_NSW, smoke_sa2_means_all_time, by.x = 'SA2_MAIN16', by.y = 'sa2_code')

sa2_NSW_by_day <- merge(sa2_NSW, smoke_sa2, by.x = 'SA2_MAIN16', by.y = 'sa2_code')

# Only plots 1 day
ggplot(sa2_NSW_by_day[sa2_NSW_by_day$Date == as.Date('2019-12-10'),], aes(colour = mean_weighted_smoke , fill = mean_weighted_smoke )) +
  geom_sf() +
  coord_sf(xlim = c(150, 152), ylim = c(-33, -35)) +
  viridis::scale_colour_viridis(option = "magma", direction = -1) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, guide = 'none') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = 'Interpolated PM2.5 \nConcentration') +
  ggtitle("Air Quality in NSW on the 10th of December 2019") 
  
 

#### ---- Calculate the number of attributable deaths ---- ####

# Get SA2 data into a non-spatial format
sa2_NSW_DT <- as.data.table(sa2_NSW_by_day)[, -c('SA3_CODE16', 'SA3_NAME16', 'SA4_CODE16', 'SA4_NAME16', 'GCC_CODE16', 'AREASQKM16', 'geometry')]

# Change is difference between smokey and non-smokey days
sa2_NSW_DT[, change:= mean_weighted_smoke - mean_weighted_non_smoke] 
sa2_NSW_DT[, SA2_MAIN16:= as.character(SA2_MAIN16)]

# Merge on death data
deaths_19_20 <- deaths[, c('code', '2019_Standardised_death_rate', '2020_Standardised_death_rate', '2019_Population', '2020_Population')]

# If standardised death rate or population is zero, attributable deaths must be zero as well:
# add a very small amount to allow smoke to have an effect
deaths_19_20[`2019_Standardised_death_rate` <= 0, `2019_Standardised_death_rate`:= 0.0001] 
deaths_19_20[`2020_Standardised_death_rate` <= 0, `2020_Standardised_death_rate`:= 0.0001]
deaths_19_20[`2019_Population` <= 0, `2019_Population`:= 1] 
deaths_19_20[`2020_Population` <= 0, `2020_Population`:= 1]

deaths_19_20[, std_death_per_day_19:= (`2019_Standardised_death_rate`)/365/1000] # Mortality rate
deaths_19_20[, std_death_per_day_20:= (`2020_Standardised_death_rate`)/365/1000]
deaths_19_20[, code:= as.character(code)]

setnames(deaths_19_20, c('2019_Population', '2020_Population'), c('pop19', 'pop20'))

# Taken from epidemiological studies
beta_all_cause_mortality <- 0.0012

sa2_NSW_DT <- merge(sa2_NSW_DT, deaths_19_20[, c('code', 'std_death_per_day_19', 'std_death_per_day_20', 
                                                 'pop19', 'pop20')], by.x = 'SA2_MAIN16', by.y = 'code', all.x = T)


#### Calculate attributable deaths:
# Attributable number = baseline incidence x (exp(beta * change in PM 2.5 concentration) - 1) x Population

# Calculate attributable deaths (separately for 2020 and 2019 since population is different)
sa2_NSW_DT[Date <= as.Date("2019-12-31"), attr_death:= std_death_per_day_19 * (exp(change * beta_all_cause_mortality) - 1) * pop19] 
sa2_NSW_DT[Date > as.Date("2019-12-31"), attr_death:= std_death_per_day_20 * (exp(change * beta_all_cause_mortality) - 1) * pop20]


sa2_deaths <- sa2_NSW_DT[, .(total_attr_deaths = sum(attr_death), pop = mean(pop20)), by = SA2_MAIN16]

sa2_deaths[, deaths_perc:= total_attr_deaths/pop]
sa2_deaths[, deaths_per_10k:= deaths_perc*10000]

# Total deaths attributable to smoke in NSW
sum(sa2_deaths$total_attr_deaths) # 155

mean(sa2_deaths$deaths_per_10k, na.rm = T) #0.2721114

# Merge with spatial object for maps
sa2_deaths_sf <- merge(sa2_NSW, sa2_deaths, by = 'SA2_MAIN16', all.x = T)
sa2_deaths_sf <- merge(sa2_deaths_sf, smoke_sa2_means_all_time, by.x = 'SA2_MAIN16', by.y = 'sa2_code', all.x = T)

# For the 2 SA2 units that are so small they don't have gridpoints in them: fill with overall mean
overall_mean_death <- mean(sa2_deaths_sf$deaths_per_10k, na.rm = T)
sa2_deaths_sf[sa2_deaths_sf$SA2_NAME16 %in% c('Acacia Gardens', 'Lilli Pilli - Port Hacking - Dolans Bay'), 'deaths_per_10k'] <- overall_mean_death

overall_mean_death_perc <- mean(sa2_deaths_sf$deaths_perc, na.rm = T)
sa2_deaths_sf[sa2_deaths_sf$SA2_NAME16 %in% c('Acacia Gardens', 'Lilli Pilli - Port Hacking - Dolans Bay'), 'deaths_per_10k'] <- overall_mean_death_perc

st_write(sa2_deaths_sf, 'N:\\Perils20\\Climate\\Smoke\\Data\\Output\\SA2_NSW_Deaths.shp', delete_layer = T)



