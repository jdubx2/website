yelp <- read.csv('E:/yelp_df_exp.csv')


table(yelp$Parent2)

yelp %>% 
  ggplot(aes(x = weightedScore, group = Parent, color = Parent2))+
  geom_density(alpha = .5)+
  facet_grid(Parent2~.)

a <- 5; s <- 2; n <- 20; error <- qt(0.975,df=n-1)*s/sqrt(n)
left <- a-error; right <- a+error; left; right

library(geojsonio)

json <- geojson_json(yelp[1:200,c(10,11,16)], lat = 'yelpLatitude', lon = 'yelpLongitude')

write(json, 'geo.json')

# {
#   "type": "Point",
#   "coordinates": [
#     -105.01621,
#     39.57422
#     ]
# }
