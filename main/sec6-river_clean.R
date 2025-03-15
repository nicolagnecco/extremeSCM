source("main/load_packages.R")

# Define functions
dfr2tibble <- function(dfr){
  ## dataframe -> tibble
  ## convert dataframe to tibble and rename column "Date" to date

  dfr %>%
    mutate(Date = as_date(Date)) %>%
    rename(date = Date) %>%
    as_tibble()
}

# Import dataset
load("data/StsTSs.RData")
load("data/StsInfo.RData")


# Select the 31 stations that are in the AOAS paper plus station 19 (renamed 32)
StsChos <- c(c(1:47)[-c(16,30,31,34,42,43,44,45,46,47,3,1,2,29,18,19)], 19)

NoSt <- length(StsChos)

StsTSsChos <- StsTSs[StsChos]
StsInfoChos <- StsInfo[StsChos,] %>%
  mutate(id_old = StsChos,
         id = 1:NoSt)


# clean data and find common dates
river_dat <- map(.x = StsTSsChos, .f = dfr2tibble) %>%
  reduce(.f = inner_join, by = "date") %>%
  filter(month(date) %in% c(6, 7, 8),
         year(date) < 2010)

# rename columns (according to AOAS paper)
colnames(river_dat)[2:(NoSt + 1)] <- paste0("station_", 1:NoSt)

saveRDS(object = river_dat,
        file = "data/river_clean_full.rds")
saveRDS(StsInfoChos, "data/StsInfoChose.rds")

# select stations
station_names <- c(11, 9, 21, 7, 19, 14, 26, 23, 28, 1, 13, 32)
river_dat_clean <- (river_dat[, c(1, station_names + 1)]) %>% 
  pivot_longer(cols = -date, 
               names_to = "id", 
               values_to = "rainfall")
NoSt <- length(station_names)

station_info <- StsInfoChos %>%
  filter(id %in% station_names) %>%
  rename(name = RivNames,
         lat = Lat,
         lon = Long,
         ave_vol = AveVol) %>% 
  mutate(id = paste0("station_", id))


# clean workspace
saveRDS(object = river_dat_clean %>% left_join(station_info, by = "id"),
        file = "data/river_clean.rds")
