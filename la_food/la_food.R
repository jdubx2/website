library(dplyr)
library(ggplot2)
library(extrafont)
library(ggthemes)

yelp <- read.csv('data/yelp_df_exp.csv', stringsAsFactors = F)

yelp <- yelp %>% 
  mutate(Parent = ifelse(Parent == 'U.K.', 'Other',
                          ifelse(Parent == 'Mongolian', 'Other',Parent)))

densitys <- yelp %>% 
  filter(Parent != 'Other',
         !(Parent2 == 'Markets' & Parent == 'Healthy')) %>% 
  ggplot(aes(x = weightedScore, fill = Parent2))+
  geom_density(alpha = .7) +
  geom_vline(xintercept = .4, color = 'gray80', linetype = 'dashed', size = .2) +
  facet_wrap(~Parent, ncol = 5) +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_brewer(type = 'qual', palette = 'Set1') +
  theme(axis.text = element_text(color='gray80'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family ='Calibri Light',size = 9),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        legend.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        strip.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        strip.text = element_text(color = 'gray80', size = 9, family = 'Calibri Light'),
        plot.margin = margin(.3, .5, 0, .1, "cm")) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = 'Weighted Score',y = '', fill = '')

ggsave(file="density.svg", plot=densitys, width=8, height=4.5)


library(geojsonio)

json <- geojson_json(yelp[,c(10,11,12,16)], lat = 'yelpLatitude', lon = 'yelpLongitude')

write(json, 'data/geo.json')


