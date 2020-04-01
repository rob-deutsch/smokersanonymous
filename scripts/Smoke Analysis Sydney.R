
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

smoke <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\Sydney Nov19 - Mar20.xlsx', startRow = 2, colNames = T)
setDT(smoke)

baseline_smoke <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\Baseline.xlsx', colNames = T)
setDT(baseline_smoke)

stations_loc <- openxlsx::read.xlsx('N:\\Perils20\\Climate\\Smoke\\Data\\Raw Data\\air-quality-monitoring-sites-summary.xlsx', colNames = T, 
                                    sheet = 'Monitoring stations list')
setDT(stations_loc)

# Shapefiles for states and SA2 Units
states <- st_read('M://finpoint19//R&D//Spatial Data//ABS_Shapefiles//State//STE_2016_AUST.shp')

NSW <- states[states$STE_NAME16 == "New South Wales", ]
rm(states)

sa2 <- st_read('N:\\Perils20\\Climate\\Smoke\\Data\\1270055001_sa2_2016_aust_shape\\SA2_2016_AUST.shp')

gc()

# Deaths & Population
deaths <- fread('N:\\Perils20\\Climate\\Smoke\\Data\\Processed Data\\std_deaths_SA2.csv')


#### ---- Clean up the data ---- ####
new_col_names_base <- sapply(str_split(colnames(baseline_smoke), '.PM2.5'), `[`, 1)
colnames(baseline_smoke) <- new_col_names_base

new_col_names_smoke <- sapply(str_split(colnames(smoke), '.PM2.5'), `[`, 1)
colnames(smoke) <- new_col_names_smoke

baseline_smoke[, Date:= as.Date(Date, format = "%d/%m/%Y")]
smoke[, Date:= as.Date(Date, format = "%d/%m/%Y")]
smoke <- smoke[Date < as.Date("2020-03-01"),]

baseline_smoke <- melt(baseline_smoke, id.vars = 'Date', variable.name = 'location', value.name = 'avg_PM2.5')
smoke <- melt(smoke, id.vars = 'Date', variable.name = 'location', value.name = 'avg_PM2.5')

## Get geocoding for stations
stations_loc <- janitor::clean_names(stations_loc)

stations_loc <- stations_loc[, c('nsw_air_quality_monitoring_aqmn_site', 'aqmn_region', 'sub_region_where_applicable', 'latitude_south', 
                                 'longitude_east', 'status')]

smoke[,location:= gsub('\\.', ' ', tolower(location))]

locations <- unique(smoke$location)

stations_loc[, location:= trimws(tolower(nsw_air_quality_monitoring_aqmn_site), which = 'both')]
stations_loc[74, location:= 'prospect'] # Manual correction - should be fixed above - not sure why not

stations_smoke <- stations_loc[location %in% locations,]

stations_smoke[, latitude_south:= gsub(' ', '', latitude_south)]
stations_smoke[, degrees_lat:= as.integer(substr(latitude_south, 1, 2))]
stations_smoke[, minutes_lat:= as.integer(substr(latitude_south, 4, 5))]
stations_smoke[, seconds_lat:= as.integer(substr(latitude_south, 7, 8))]

stations_smoke[, latitude:= dms2dd(degrees_lat, minutes_lat, seconds_lat, ns = "S")]


stations_smoke[, longitude_east:= gsub(' ', '', longitude_east)]
stations_smoke[, degrees_long:= as.integer(substr(longitude_east, 1, 3))]
stations_smoke[, minutes_long:= as.integer(substr(longitude_east, 5, 6))]
stations_smoke[, seconds_long:= as.integer(substr(longitude_east, 8, 9))]

stations_smoke[, longitude:= dms2dd(degrees_long, minutes_long, seconds_long, ns = "E")]

stations_smoke[, c('latitude_south', 'longitude_east', 'degrees_lat', 'minutes_lat', 'seconds_lat', 'degrees_long', 'minutes_long', 'seconds_long'):= NULL]



#### ---- Exposure assessment ---- ####

#### Identification of smokey days: Days where the pm2.5 concentration exceeds the 99th percentile of the base period

## EDA to get understanding of base period

# Number of non-null observations in baseline period 
baseline_smoke[, .(no_observations = sum(!is.na(avg_PM2.5))), by = location]

# Plot to assess continuity of observations
ggplot(baseline_smoke[location %in% c('LIVERPOOL', 'CHULLORA', 'EARLWOOD', 'RICHMOND', 'PARRAMATTA.NORTH', 'CAMDEN')], 
       aes(x = Date , y = avg_PM2.5)) +
  geom_line() +
  facet_wrap(.~location)

# Get 99th percentile of base period
perc99 <- baseline_smoke[, quantile(avg_PM2.5, 0.99, na.rm = T)]

# Flag observations considered "smokey"
smoke[avg_PM2.5 >= perc99, smokey_flag:= 1]
smoke[avg_PM2.5 < perc99, smokey_flag:= 0]

#### Estimation of the average smoke concentration at monitor locations 
smoke_related_PM2.5_conc <- smoke[!is.na(smokey_flag), .(mean_avg_PM2.5 = mean(avg_PM2.5)), by = c('location', 'smokey_flag')]

smoke_related_PM2.5_conc <- dcast(smoke_related_PM2.5_conc, location ~ smokey_flag)

#### Estimation of the population weighted mean concentrations

sydney_area <- st_sfc(st_polygon(list(rbind(c(150, -34.6), c(151.8,-34.6), c(151.8, -32.8),c(150, -32.8), c(150, -34.6)))))

sydney_grid <- st_make_grid(sydney_area, n = 500, crs = crs_code, what = 'centers')
st_crs(sydney_grid) <- crs_code

# Cut down to grid points in NSW: remove those in sea for faster calculations
grid_NSW <- st_join(st_sf(sydney_grid), st_transform(NSW, crs_code), join = st_intersects)
grid_NSW <- grid_NSW[!is.na(grid_NSW$STE_CODE16),]

NSW_grid_proj <- st_sf(st_transform(grid_NSW, crs = crs_projected))
NSW_grid_proj$id <- 1:nrow(NSW_grid_proj)

stations_smoke <- merge(stations_smoke, smoke_related_PM2.5_conc[, c('location', '0', '1')], by = 'location')

stations_smoke_sf <- st_as_sf(stations_smoke, coords = c('longitude', 'latitude'), crs = crs_code)
stations_smoke_proj <- st_transform(stations_smoke_sf, crs = crs_projected)

distances <- st_distance(stations_smoke_proj, NSW_grid_proj)

distances_df <- as.data.table(data.frame(matrix(distances, nrow = nrow(stations_smoke_proj), ncol = nrow(NSW_grid_proj))))
distances_df[, location:= stations_smoke$location]

setcolorder(distances_df, neworder = c('location', paste0('X', 1:nrow(NSW_grid_proj))))

distances_melted <- melt(distances_df, id.vars = 'location', value.name = 'distance', variable.name = 'id')
distances_melted[, id:= as.integer(gsub('X', '', id))]

distances_melted <- merge(distances_melted, stations_smoke[, c('location', 'latitude', 'longitude', '0', '1')],
                          by = 'location', all.x = T)

NSW_grid <- st_transform(NSW_grid_proj, crs = crs_code)
NSW_grid_DT <- as.data.table(cbind(st_coordinates(NSW_grid)[, c('X', 'Y')], NSW_grid$id))
setnames(NSW_grid_DT, 'V3', 'id')

distances_melted <- merge(distances_melted, NSW_grid_DT, by = 'id' )

#### ---- Get weighted average by distance for each point ---- ####

distances_melted[, distance_wght:= (1/distance)^2]
distances_melted[, total:= sum(distance_wght), by = id]
distances_melted[, proportion:= distance_wght / total]

distances_melted[, c('distance_wght', 'total'):= NULL]

smoke[, month:= month(Date)]

smoke_dist_Nov <- merge(smoke[month == 11,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_dist_Dec <- merge(smoke[month == 12,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_dist_Jan <- merge(smoke[month == 1,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)
smoke_dist_Feb <- merge(smoke[month == 2,], distances_melted[, -c('X', 'Y')], by = 'location', all.x = T, allow.cartesian = T)

smoke_pts_Nov <- smoke_dist_Nov[, .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
smoke_pts_Dec <- smoke_dist_Dec[, .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
smoke_pts_Jan <- smoke_dist_Jan[, .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]
smoke_pts_Feb <- smoke_dist_Feb[, .(weighted_smoke= sum(avg_PM2.5 * proportion, na.rm = T)), by = c('Date', 'id')]

rm(smoke_dist_Nov, smoke_dist_Dec, smoke_dist_Jan, smoke_dist_Feb)
gc()

smoke_pts <- rbind(smoke_pts_Nov, smoke_pts_Dec, smoke_pts_Jan, smoke_pts_Feb)

rm(smoke_pts_Nov, smoke_pts_Dec, smoke_pts_Jan, smoke_pts_Feb)
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


# Cut down to SA2 in NSW
sa2_NSW <- st_join(sa2, NSW, join = st_intersects)
sa2_NSW <- sa2_NSW[!is.na(sa2_NSW$STE_CODE16.y), 'SA2_MAIN16']
sa2_NSW <- st_transform(sa2_NSW, crs_code)

intersections <- as.data.table(st_join(grid_NSW, sa2_NSW, join = st_intersects))
intersections <- cbind(intersections[, SA2_MAIN16], as.data.table(st_coordinates(grid_NSW)))

setnames(intersections, 'V1', 'sa2_code')

intersections[, id:= .I]

smoke_pts_sa2 <- merge(smoke_pts[, -c('X', 'Y')], intersections, by = 'id', all.x = T)
smoke_pts_sa2 <- merge(smoke_pts_sa2, non_smokey_baseline, by = 'id', all.x = T)

smoke_sa2 <- smoke_pts_sa2[, .(mean_weighted_smoke = mean(weighted_smoke), mean_weighted_non_smoke = mean(non_smoky_weighted)),
                           by = c('sa2_code', 'Date')]

sa2_NSW <- merge(sa2_NSW, smoke_sa2, by.x = 'SA2_MAIN16', by.y = 'sa2_code')

ggplot(sa2_NSW, aes(colour = mean_weighted_smoke, fill = mean_weighted_smoke)) +
  geom_sf() +
  coord_sf(xlim = c(150, 152), ylim = c(-33, -35)) +
  viridis::scale_colour_viridis(option = "magma", direction = -1) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, guide = 'none') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  labs(colour = 'Interpolated PM2.5 \nConcentration') +
  ggtitle("Air Quality in Sydney on the 10th of December 2019") 
  
 

#### ---- Calculate the number of attributable deaths ---- ####

# Attributable number = baseline incidence x (exp(beta * change in PM 2.5 concentration ) - 1) x Population

# Get SA2 data into a non-spatial format
sa2_NSW_DT <- as.data.table(sa2_NSW)
sa2_NSW_DT[, geometry:= NULL]
sa2_NSW_DT[, change:= mean_weighted_smoke - mean_weighted_non_smoke]
sa2_NSW_DT[, SA2_MAIN16:= as.character(SA2_MAIN16)]

# Merge on death data
deaths_19_20 <- deaths[, c('code', '2019_Standardised_death_rate', '2020_Standardised_death_rate', '2019_Population', '2020_Population')]
deaths_19_20[`2019_Standardised_death_rate` < 0, `2019_Standardised_death_rate`:= 0]
deaths_19_20[`2020_Standardised_death_rate` < 0, `2020_Standardised_death_rate`:= 0]

deaths_19_20[, std_death_per_1000_per_day_19:= (`2019_Standardised_death_rate`)/365]
deaths_19_20[, std_death_per_1000_per_day_20:= (`2020_Standardised_death_rate`)/365]
deaths_19_20[, code:= as.character(code)]

setnames(deaths_19_20, c('2019_Population', '2020_Population'), c('pop19', 'pop20'))

beta_all_cause_mortality <- 0.0012

sa2_NSW_DT <- merge(sa2_NSW_DT, deaths_19_20[, c('code', 'std_death_per_100k_per_day_19', 'std_death_per_100k_per_day_20', 
                                                 'pop19', 'pop20')], by.x = 'SA2_MAIN16', by.y = 'code', all.x = T)

sa2_NSW_DT[Date <= as.Date("2019-12-31"), attr_death:= std_death_per_1000_per_day_19 * (exp(change * beta_all_cause_mortality) - 1) *
             pop19/1000]

sa2_NSW_DT[Date > as.Date("2019-12-31"), attr_death:= std_death_per_1000_per_day_20 * (exp(change * beta_all_cause_mortality) - 1) * 
             pop20/1000]

sa2_deaths <- sa2_NSW_DT[, .(deaths= sum(attr_death)), by = SA2_MAIN16]



sa2_deaths_sf <- merge(sa2_NSW, sa2_deaths, by = 'SA2_MAIN16')

ggplot(sa2_deaths_sf, aes(colour = deaths, fill = deaths)) +
  geom_sf() +
  coord_sf(xlim = c(150, 152), ylim = c(-33, -35)) +
  viridis::scale_colour_viridis(option = "magma", direction = -1) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, guide = 'none') +
  theme(panel.background = element_rect(fill = 'white'), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank(), legend.position = c(0.85, 0.2)) +
  labs(colour = 'Deaths Attributable to Smoke Exposure \n(1 Nov 2019 - 29 Feb 2020)') +
  ggtitle("Deaths in Sydney Attributable to Smoke Exposure during the 2019/20 Bushfires") 


