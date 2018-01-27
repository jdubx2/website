library(dplyr)
library(ggplot2)

yelp <- read.csv('D:/yelp_df_exp.csv', stringsAsFactors = F)

yelp <- yelp %>% 
  mutate(Parent = ifelse(Parent == 'U.K.', 'Other',
                          ifelse(Parent == 'Mongolian', 'Other',Parent)))

yelp %>% 
  filter(Parent != 'Other') %>% 
  ggplot(aes(x = weightedScore, color = Parent2, fill = Parent2))+
  geom_vline(xintercept = .4) +
  geom_density(alpha = .8) +
  facet_wrap(~Parent, ncol = 5)


library(geojsonio)

json <- geojson_json(yelp[1:200,c(10,11,16)], lat = 'yelpLatitude', lon = 'yelpLongitude')

write(json, 'geo.json')


