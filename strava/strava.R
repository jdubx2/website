library(dplyr) 
library(lubridate)
library(ggplot2)
library(geosphere)
library(ggthemes)
library(extrafont)
library(tidyr)


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

strava <- strava %>% 
  group_by(File) %>% 
  mutate(elev_chg = Elevation - ifelse(is.na(lag(Elevation)), Elevation, lag(Elevation)),
         time_lag = if_else(is.na(lag(Timestamp)), Timestamp, lag(Timestamp)),
         time_chg = as.numeric(difftime(Timestamp, time_lag, units = 'mins')),
         waypoint = row_number(),
         power = (time_chg / dist))


summary <- strava %>%
  group_by(File, Activity) %>%
  summarise(miles = sum(dist) / 1609.34,
            start = min(Timestamp),
            end = max(Timestamp),
            mins = as.numeric(difftime(end, start, units = 'mins')),
            hrs = as.numeric(mins) / 60,
            mph = miles/hrs,
            elev_gain = sum(elev_chg[elev_chg > 0]),
            elev_loss = sum(elev_chg[elev_chg < 0]))

ride_ols <- summary(lm(hrs ~ miles, data = filter(summary, Activity == 'Ride')))
run_ols <- summary(lm(hrs ~ miles, data = filter(summary, Activity == 'Run')))
rsq_df <- data.frame(Act = c('Ride','Run'), rsq = c(round(ride_ols$r.squared,2),round(run_ols$r.squared,2)),
                     x = c(45,23), y = c(4.5,4.7))

scatter <- summary %>% 
  ggplot(aes(x = miles, y = hrs))+
  geom_point(aes(fill = Activity), shape = 21, size = 1, alpha = .8, color = '#211e1e', stroke = .1) +
  geom_line(aes(color = Activity), method = 'lm', stat = 'smooth', size = .4, linetype='dashed', alpha = .8) +
  geom_text(data = rsq_df, aes(x = x, y = y, label = paste0(Act,'s\n R² ',rsq), color = Act), 
            family = 'Calibri', fontface = 'bold', size = 2.5) +
  scale_fill_manual(values = c('deepskyblue2','darkolivegreen2')) +
  scale_color_manual(values = c('deepskyblue2','darkolivegreen2')) +
  scale_x_continuous(limits = c(0,50), expand = c(.01,.01)) +
  theme_hc(bgcolor = "darkunica") +
  theme(axis.text = element_text(color='gray80'),
        panel.grid.major.y = element_line(color='gray25', size = .3),
        panel.grid.major.x = element_line(color='gray25', size = .3),
        text = element_text(family ='Calibri',size = 9),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.margin = margin(.2, .5, .1, .1, "cm")) +
  guides(fill = F, color = F) +
  labs(x = 'Miles Travelled', y = 'Hours')

ggsave(file="strava_scat.svg", plot=scatter, width=3.5, height=2.5)


violin <- summary %>% 
  select(File, Activity, elev_gain, elev_loss) %>% 
  gather(type, value, -c(File,Activity)) %>% 
  ggplot(aes(x = Activity, y = value, fill = Activity)) +
    geom_violin(alpha = .8, color = 'gray25') +
    scale_fill_manual(values = c('deepskyblue2','darkolivegreen2')) +
    theme_hc(bgcolor = "darkunica") +
    scale_y_continuous(breaks = seq(-1000,1000,250)) +
    theme(axis.text = element_text(color='gray80'),
          panel.grid.major.y = element_line(color='gray25', size = .3),
          panel.grid.major.x = element_blank(),
          text = element_text(family ='Calibri',size = 9),
          panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.margin = margin(.1, .5, 0, .1, "cm")) +
    guides(color = F, fill = F) +
    labs(x = '', y = 'Average Elevation Gain/Loss (Meters)')

ggsave(file="strava_violin.svg", plot=violin, width=3.5, height=2.5)

lines <- summary %>% 
  mutate(year = year(start),
         month = ifelse(month(start) %%3 != 0, 3 - month(start)%%3 + month(start), month(start)),
         yrmo = ymd(paste(year,month,'01', sep = '-'))) %>% 
  group_by(yrmo, Activity) %>% 
  summarise(miles = sum(miles)) %>% 
  filter(yrmo != ymd('2017-12-01')) %>% 
  ggplot(aes(x = yrmo, y = miles)) +
    geom_line(aes(color = Activity), alpha = .8) +
    geom_point(aes(fill = Activity), shape = 21, color = '#211e1e', stroke = .1, size = 1, alpha = .9) +
    scale_y_continuous(limits = c(0,400)) +
    scale_color_manual(values = c('deepskyblue2','darkolivegreen2')) +
    scale_fill_manual(values = c('deepskyblue2','darkolivegreen2')) +
    labs(x = '', y = 'Miles Travelled') +
    scale_x_date(date_breaks = '3 months',
                 labels = function(x) ifelse(month(x) == 3, paste0('Q1\n',year(x)),
                                                                       ifelse(month(x) == 6, 'Q2',
                                                                              ifelse(month(x) == 9, 'Q3','Q4')))) +
    theme_hc(bgcolor = "darkunica") +
    theme(axis.text = element_text(color='gray80'),
          panel.grid.major.y = element_line(color='gray25', size = .3),
          panel.grid.major.x = element_line(color='gray25', size = .3),
          text = element_text(family ='Calibri',size = 9),
          panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          legend.position = c(.9,.85),
          legend.title = element_blank(),
          legend.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.margin = margin(.2, .5, .1, .1, "cm"))
   # guides(color = F, fill = F)

ggsave(file="strava_lines.svg", plot=lines, width=3.5, height=2.5)


check <- filter(strava, File == '20150815-214909-Ride.gpx')


box <- summary %>% 
  ggplot(aes(x = Activity, y = mph, fill = Activity)) +
    geom_boxplot(color = 'gray25', alpha = .9, outlier.colour = NULL, outlier.shape = 21, outlier.size = 1) +
    scale_fill_manual(values = c('deepskyblue2','darkolivegreen2')) +
    scale_y_continuous(limits = c(0,20), breaks = seq(0,18,3)) +
    theme_hc(bgcolor = "darkunica") +
    theme(axis.text = element_text(color='gray80'),
          panel.grid.major.y = element_line(color='gray25', size = .3),
          panel.grid.major.x = element_blank(),
          text = element_text(family ='Calibri',size = 9),
          panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          legend.background = element_rect(fill = '#211e1e', color = '#211e1e'),
          plot.margin = margin(0, .5, 0, .1, "cm")) +
    guides(fill = F) +
  labs(y = 'Average MPH',x = '')

ggsave(file="strava_box.svg", plot=box, width=3.5, height=2.5)
