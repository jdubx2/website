library(dplyr) 
library(lubridate)
library(ggplot2)
library(geosphere)

options(lubridate.fasttime = TRUE)


strava <- read.csv("strava/strava_rides.csv")
strava <- filter(strava, Activity != "Hike")




strava <- strava %>%
  group_by(File) %>%
  mutate(long2 = ifelse(is.na(lag(Longitude)),Longitude,lag(Longitude)), 
         lat2 = ifelse(is.na(lag(Latitude)),Latitude,lag(Latitude))) %>%
  rowwise() %>%
  mutate(dist = geosphere::distHaversine( c(Longitude, Latitude), 
                                          c(long2, lat2)))


run_df <- filter(strava, Activity == 'Run')
ride_df <- filter(strava, Activity == 'Ride')

run_df <- mutate(run_df, Timestamp = ymd_hms(Timestamp))
ride_df <- mutate(ride_df, Timestamp = ymd_hms(Timestamp))

#saveRDS(run_df, 'run_df.rds')
saveRDS(ride_df, 'ride_df.rds')

run_df <- readRDS('strava/run_df.rds')


run_sumry <- run_df %>%
  group_by(File) %>%
  summarise(dist = sum(dist) / 1609.34,
            start = min(Timestamp),
            end = max(Timestamp),
            mins = end - start,
            hrs = as.numeric(mins) / 60,
            mph = dist/hrs)

run_sumry %>%
  filter(mph < 200) %>%
  ggplot(aes(x = dist, y = mins)) +
  geom_point()


run_df <- run_df %>%
  group_by(File) %>%
  mutate(run_prof = dist - ifelse(is.na(lag(dist)), 0 , lag(dist)))

run_df %>%
  filter(run_prof >= -1 & run_prof <= 1) %>%
  group_by(X) %>%
  summarise(run_prof = mean(run_prof)) %>%
  ggplot() +
  geom_smooth(aes(x = X, y = run_prof), span = .1, method = 'loess')


strava_smry %>%
  group_by(Activity, File) %>%
  summarise()
