# prep_data.R

# This script reads all the data and saves it as an Rdata file for loading in
# htsViewer.Rmd.

# Load development version of tmrtools
devtools::load_all('C:/Users/matt.landis/OneDrive - Resource Systems Group, Inc/Git/tmrtools/')

library(DBI)
library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(stringr)


params = list(
  codebook_path = 'Q:/Projects/CA/SANDAG/19063_SB1_TNC_Ridehailing/8_Final_Deliverables/2_Dataset_Documentation/Consolidated_SB1_TNC_Study_Codebook_27February2020.xlsx',
  dbname = 'tnc_bayarea',
  data_source = 'pops',
  study_region_shp = "Q:/Projects/CA/SANDAG/19063_SB1_TNC_Ridehailing/5_Data_Analysis/1_Data/GeographicData&ShapeFiles/MTC/Transportation_Analysis_Zones/Transportation_Analysis_Zones.shp"
)


# load varvals ================
vv_path = file.path('data', paste0(params$dbname, '_varvals.Rdata'))

if ( file.exists(vv_path) ){
  load(vv_path)  
} else {
  varvals <- read_codebook(params$codebook_path, label_col='label_mtc')
  setnames(varvals, 'label_value', 'value_label')
  setnames(varvals, c('value_label', 'label'), c('value_plus_label', 'value_label'))
  
  # read in variable labels and logic
  varnames = read_codebook(codebook_path=params$codebook_path, varvals=FALSE)
  
  saveRDS(varvals, file.path('data', paste0(params$dbname, '_varvals.rds')))
  saveRDS(varnames, file.path('data', paste0(params$dbname, '_varnames.rds')))
  
  save(varvals, varnames, file=vv_path)
}

# Load data here
dir.create('data', showWarnings=FALSE)

data_file = file.path('data', paste0(params$dbname, '_data.Rdata'))

  
# Data location
# data_source = file.path(q_dir, '8_Final_Deliverables', '1_Data', 'MTC',
#                        'BayArea_dataset_20191031')
stopifnot(params$data_source == 'pops' | dir.exists(params$data_source))

if ( params$data_source == 'pops' ){
  # get data from POPS
  con = connect_to_pops(params$dbname)
  
  message('Reading household')
  hh = dbGetQuery(con = con, "select * from ex_hh") %>% data.table()
  
  message('Reading person')
  person = dbGetQuery(con = con,  "select * from ex_person") %>% data.table()
  
  message('Reading vehicle')
  vehicle = dbGetQuery(con = con, "select * from ex_vehicle") %>% data.table()
  
  message('Reading day')
  day = dbGetQuery(con = con, "select * from ex_day") %>% data.table()
  
  message('Reading trip')
  trip = dbGetQuery(con = con, "select * from ex_trip") %>% data.table()
  
  trip[, trip_id := as.numeric(trip_id)]
  
  message('Reading location')
  location = dbGetQuery(con = con, "select * from ex_location") %>% data.table()
  
  DBI::dbDisconnect(con)
  
} else {
  options(datatable.integer64 = 'numeric')
  message('Reading household')
  hh = fread(file.path(params$data_source, 'ex_hh.tsv'))
  #household[, hh_id := as.character(hh_id)]
  
  message('Reading person')
  person = fread(file.path(params$data_source, 'ex_person.tsv'))
  # person[, `:=`(hh_id = as.character(hh_id),
  #               person_id = as.character(person_id))]
  #
  message('Reading vehicle')
  vehicle = fread(file.path(params$data_source, 'ex_vehicle.tsv'))
  
  message('Reading day')
  day = fread(file.path(params$data_source, 'ex_day.tsv'))
  # day[, `:=`(hh_id = as.character(hh_id),
  #            person_id = as.character(person_id))]
  #
  message('Reading trip')
  trip = fread(file.path(params$data_source, 'ex_trip.tsv'))
  trip[, `:=`(# hh_id = as.character(hh_id),
    # person_id = as.character(person_id),
    # trip_id = as.character(trip_id),
    depart_time = ymd_hms(depart_time),
    arrive_time = ymd_hms(arrive_time),
    survey_complete_time = ymd_hms(survey_complete_time))]
  
  message('Reading location')
  location = fread(file.path(params$data_source, 'ex_location.tsv'))
  location[, `:=`(hh_id = as.character(hh_id),
    person_id = as.character(person_id),
    trip_id = as.character(trip_id),
    collected_at = ymd_hms(collected_at))]
}

# Prepare the trip data ---------------------------------------------------

location = location %>%
  mutate(hh_id = as.character(hh_id),
    person_id = as.character(person_id)) %>%
  data.table()

trip = trip %>%
  mutate(hh_id = as.character(hh_id),
    person_id = as.character(person_id),
    trip_id = as.character(trip_id)) %>%
  data.table()

day = day %>%
  mutate(hh_id = as.character(hh_id),
    person_id = as.character(person_id)) %>%
  data.table()

person = person %>%
  mutate(hh_id = as.character(hh_id)) %>%
  data.table()

hh = hh %>%
  mutate(hh_id = as.character(hh_id)) %>%
  data.table()

# Create polylines out of locations 
# TODO: Use map matching to make these routes

crs_lonlat = 4326
loc_sf = location %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(crs_lonlat)

trip_lines = loc_sf %>%
  arrange(trip_id, collected_at_imputed) %>%
  select(trip_id, geometry) %>%
  dplyr::group_by(trip_id) %>%
  dplyr::summarize(do_union=FALSE) %>%
  st_cast('LINESTRING')

D = 100
D2 = 150
M = 300

home_distance  = 150  # meters
long_dwell = 5        # hours

keep_cols = c('d_purpose_category_imputed', 'd_purpose_imputed',
  'mode_type', 'd_location_type', 'dwell_hr',
  'depart_time', 'duration', 'speed_mph_imputed',
  'travel_date_dow', 'day_complete',
  'trip_num', 'trip_id', 'person_id')

trip1 = trip %>%
  mutate(person_id = as.character(person_id)) %>%
  left_join(day %>% 
      select(hh_id, person_id, day_num, day_complete) %>%
      mutate(person_id = as.character(person_id)),
    by = c('person_id', 'day_num'), suffix=c('', '_day')) %>%
  left_join(hh %>% select(hh_id, reported_home_lon, reported_home_lat),
    by = 'hh_id') %>%
  left_join(person %>%
      mutate(person_id = as.character(person_id)) %>%
      select(person_id, age, worker, student, school_type, 
        smartphone_type,
        work_lon, work_lat, school_lon, school_lat),
    by = 'person_id') %>%
  group_by(person_id, day_num) %>%
  mutate(
    days_first_trip = as.integer(trip_num == min(trip_num)),
    days_last_trip = as.integer(trip_num == max(trip_num))) %>%
  ungroup() %>%
  arrange(person_id, trip_num) %>% 
  group_by(person_id) %>%
  mutate(
    #trip_id =as.numeric(trip_id),
    day_ends_at_home = as.integer(
      days_last_trip == 1 & d_distance_home <= D2 & dwell_time_min >= M)
    # d_purpose_cat1 = case_when(d_purpose_category == 1 ~ '1 Home',
    #                            d_purpose_category == 2 ~ '2 Work',
    #                            d_purpose_category == 4 ~ '3 School',
    #                            TRUE ~ '4 Other'),
    # d_purpose_cat_imp1 = case_when(d_purpose_category_imputed == 1 ~ '1 Home',
    #                                d_purpose_category_imputed == 2 ~ '2 Work',
    #                                d_purpose_category_imputed == 4 ~ '3 School',
    #                                TRUE ~ '4 Other')
  ) %>%
  ungroup() %>% 
  setDT()

trip1[, day_trip_num := 1:.N, by = .(person_id, day_num)]
trip1[, depart_time := format(depart_time_imputed, '%H:%M:%S')]
trip1[, distance_km := round(distance / 0.621371, 2)]
trip1[, dwell_hr := round(dwell_time_min / 60, 2)]
trip1[, is_near_home := 1 * (d_distance_home < home_distance)]
trip1[, is_long_dwell := 1 * (dwell_hr >= long_dwell/60)]


trip2 = factorize_df(df=trip1[, keep_cols, with=FALSE],
  varvals,
  verbose=FALSE)
setDT(trip2)

trip2[, person_id := as.character(person_id)]

sf_trip = merge(trip2, trip_lines, by = 'trip_id', all.x=TRUE) %>%
  as.data.frame() %>%
  st_as_sf() # %>%
# as('Spatial')

saveRDS(hh, file.path('data', paste0(params$dbname, '_hh.rds')))
saveRDS(person, file.path('data', paste0(params$dbname, '_person.rds')))
saveRDS(day, file.path('data', paste0(params$dbname, '_day.rds')))
saveRDS(sf_trip, file.path('data', paste0(params$dbname, '_trip.rds')))
saveRDS(location, file.path('data', paste0(params$dbname, '_location.rds')))

save(
  hh,
  person,
  day,
  sf_trip,
  location,
  D, D2, M, home_distance, long_dwell,
  file = data_file)

# Save labeled data

hh_labeled = factorize_df(
  hh[,
    .(hh_id, num_people, rent_own, income_detailed, income_aggregate, res_type,
      reported_home_lon, reported_home_lat)],
  vals_df=varvals[!is.na(label)],
  value_label_colname='label',
  extra_labels = '_All categorical variables_',
  verbose=FALSE)

# anonymize the data

hh_labeled[, hh_id := sample(hh_id, size=length(hh_id))]

idx = sample(1:nrow(hh_labeled), size=nrow(hh_labeled))
hh_labeled[, `:=`(
  reported_home_lon = round(reported_home_lon[idx], 3),
  reported_home_lat = round(reported_home_lat[idx], 3)
)]

saveRDS(hh_labeled, file.path('data', paste0(params$dbname, '_hh_labeled.rds')))


person_labeled = factorize_df(
  person[, .(person_id, age, gender, employment, worker, student, education, work_lon, work_lat)],
  vals_df=varvals[!is.na(label)],
  value_label_colname='label',
  extra_labels = '_All categorical variables_',
  verbose=FALSE)

saveRDS(person_labeled, file.path('data', paste0(params$dbname, '_person_labeled.rds')))



# spatial data ----------------------------------------------------------------

study_boundary_file = file.path('data', paste0(params$dbname, '_study_boundary.gpkg'))


crs_lonlat = 4326
crs_equal_area = 5070  # US Albers Equal Area (needed for st_intersection)

# Load study area
taz_sf = st_read(dsn=dirname(params$study_region_shp), layer=basename(dirname(params$study_region_shp)))

# Dissolve internal boundaries
sb = st_union(taz_sf)
sb = st_transform(sb, crs_equal_area)

# Get rid of holes 
# https://stackoverflow.com/questions/52654701/removing-holes-from-polygons-in-r-sf
sb2 = st_sfc(
  st_multipolygon(x=lapply(sb[[1]], function(x) x[1]))
)
st_crs(sb2) = st_crs(sb)
sb3 = st_buffer(sb2, dist=0)  # Clean up invalid geometry
sb3 = st_transform(sb3, crs_lonlat)

st_write(sb3, study_boundary_file)


