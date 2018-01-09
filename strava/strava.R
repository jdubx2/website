library(dplyr) 
library(lubridate)
library(ggplot2)
library(geosphere)
library(ggthemes)
library(extrafont)


strava <- read.csv("D:/strava/strava_rides.csv")
strava <- filter(strava, Activity != "Hike")


strava <- strava %>%
  filter(Activity %in% c('Run', 'Ride')) %>% 
  group_by(File) %>%
  mutate(long2 = ifelse(is.na(lag(Longitude)),Longitude,lag(Longitude)), 
         lat2 = ifelse(is.na(lag(Latitude)),Latitude,lag(Latitude))) %>%
  rowwise() %>%
  mutate(dist = geosphere::distHaversine( c(Longitude, Latitude), 
                                          c(long2, lat2)))

strava_final <- mutate(strava, Timestamp = ymd_hms(Timestamp))

saveRDS(strava_final, 'strava_final.rds')

#------------------------------------------------------------------------#

strava <- readRDS('strava_final.rds')

summary <- strava %>%
  group_by(File, Activity) %>%
  summarise(miles = sum(dist) / 1609.34,
            start = min(Timestamp),
            end = max(Timestamp),
            mins = as.numeric(difftime(end, start, units = 'mins')),
            hrs = as.numeric(mins) / 60,
            mph = miles/hrs)

ride_ols <- summary(lm(hrs ~ miles, data = filter(summary, Activity == 'Ride')))
run_ols <- summary(lm(hrs ~ miles, data = filter(summary, Activity == 'Run')))
rsq_df <- data.frame(Act = c('Ride','Run'), rsq = c(round(ride_ols$r.squared,2),round(run_ols$r.squared,2)),
                     x = c(45,23), y = c(4.5,4.7))

scatter <- summary %>% 
  ggplot(aes(x = miles, y = hrs))+
  geom_point(aes(fill = Activity), shape = 21, size = 2, alpha = .6, color = '#211e1e') +
  geom_line(aes(color = Activity), method = 'lm', stat = 'smooth', size = .5, linetype='dashed', alpha = .9) +
  geom_text(data = rsq_df, aes(x = x, y = y, label = paste0('R² ',rsq), color = Act), 
            family = 'Calibri', fontface = 'bold', size = 2) +
  scale_fill_manual(values = c('dodgerblue','orangered')) +
  scale_color_manual(values = c('dodgerblue','orangered')) +
  scale_x_continuous(limits = c(0,50)) +
  theme_hc(bgcolor = "darkunica") +
  theme(axis.text = element_text(color='gray80'),
        panel.grid.major.y = element_line(color='gray25', size = .3),
        panel.grid.major.x = element_line(color='gray25', size = .3),
        text = element_text(family ='Calibri',size = 11),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'))

ggsave(file="strava_scat.svg", plot=scatter, width=7, height=3)
