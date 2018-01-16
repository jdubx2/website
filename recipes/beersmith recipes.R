
library(dplyr)
library(ggplot2)

recipes <- read.csv("data/bsrecipes.csv")

# clean and select fields of interest
recipes <- recipes %>%
  group_by(Rec_ID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(ABV_pct = as.numeric(gsub("%","",ABV)),
         Bitterness_ibu = as.numeric(gsub(" IBUs","",Bitterness)),
         Color_srm = as.numeric(gsub(" SRM","",Color))) %>%
  select(ABV_pct, Bitterness_ibu, Color_srm, Style_Master, Rec_Type = Type)

# remove unrealistic recipes
recipes <- recipes %>%
  filter(ABV_pct >= 2 & ABV_pct <= 25,
         Bitterness_ibu > 0 & Bitterness_ibu <= 150,
         Color_srm < 100)

srm_map <- function(srm){
  srm_col <- ifelse(srm < 2.6,"0 - 2.5",
                    ifelse(srm < 3.6, "2.5 - 3.5",
                           ifelse(srm < 5, "3.5 - 5",
                                  ifelse(srm < 7.5, "5 - 7.5",
                                         ifelse(srm < 10.5, "7.5 - 10.5",
                                                ifelse(srm < 13.5, "10.5 - 13.5",
                                                       ifelse(srm < 16.5, "13.5 - 16.5",
                                                              ifelse(srm < 19, "16.5 - 19",
                                                                     ifelse(srm < 22, "19 - 22",
                                                                            ifelse(srm < 27, "22 - 27",
                                                                                   ifelse(srm < 35, "27 - 35",
                                                                                          "35+")))))))))))
  return(srm_col)
}

srm_pal <- c("#ffff45","#ffe93e","#fed849","#ffa846","#f49f44","#d77f59","#94523a","#804541","#5b342f","#4c3b2b","#38302e","#31302c")


library(ggrepel)
library(ggthemes)
library(extrafont)


#Aggregate beer styles for plotting
style_df <- recipes %>%
  group_by(Style_Master) %>%
  summarise(count = n(),
            abv = mean(ABV_pct),
            ibu = mean(Bitterness_ibu),
            srm = mean(Color_srm)) %>%
  arrange(desc(count)) %>%
  mutate(srm_cat = factor(srm_map(srm), levels = c("0 - 2.5","2.5 - 3.5","3.5 - 5","5 - 7.5",
                                                         "7.5 - 10.5","10.5 - 13.5","13.5 - 16.5","16.5 - 19",
                                                         "19 - 22","22 - 27","27 - 35","35+")),
         label_cat = ifelse(srm<13,'1','0'))


style_df %>% 
  arrange(desc(count)) %>% 
  mutate(share = round(count / sum(count),2),
         cum_share = cumsum(share)) %>% 
  filter(cum_share <= .85)

#bar plot
bar <- style_df %>% 
  ggplot(aes(x = reorder(Style_Master,-count), y =count, fill = srm_cat)) +
  geom_bar(stat="identity") +
  geom_vline(xintercept = 13.5, color = 'gray30', linetype = 'dashed', size = .5) +
  geom_text(aes(x = 11.5, y = 5250, label = '85%'), family = 'Calibri', color = 'gray30', size = 3) +
  geom_text(aes(x = 15.5, y = 5250, label = '15%'), family = 'Calibri', color = 'gray30', size = 3) +
  geom_segment(aes(x = 12.5, xend=10, y = 4770, yend = 4770), size = .5,  
               arrow = arrow(length = unit(0.2, "cm")), color = 'gray30') +
  geom_segment(aes(x = 14.5, xend=17, y = 4770, yend = 4770), size = .5,  
               arrow = arrow(length = unit(0.2, "cm")), color = 'gray30') +
  theme_minimal() +
  theme(text = element_text(family = 'Calibri', size = 10),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks.x = element_line(color = 'gray80'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color='gray80', size = .3),
        plot.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
        panel.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
        legend.position = c(.8,.55),
        legend.background = element_rect(fill = '#f5f5f5', color = 'gray80'),
        legend.key.size = unit(.35,'cm'),
        axis.title.x = element_blank()) +
  scale_y_continuous(breaks = c(seq(0,7000,1000)),labels= function(x) paste0(x/1000,'K'), 
                     limits = c(0,7000), expand = c(0,0)) +
  scale_fill_manual(values = srm_pal) +
  labs(x="", y="Number of Recipes", fill="Avg Recipe SRM")


ggsave('recipe_bar.svg', bar, device='svg', height = 3.5, width= 7)
  
  
#scatter plot
scatter <- recipes %>%
  mutate(srm_cat = factor(srm_map(Color_srm), levels = c("0 - 2.5","2.5 - 3.5","3.5 - 5","5 - 7.5",
                                                         "7.5 - 10.5","10.5 - 13.5","13.5 - 16.5","16.5 - 19",
                                                         "19 - 22","22 - 27","27 - 35","35+"))) %>%
 ggplot(aes(x=Bitterness_ibu,y=ABV_pct)) +
    stat_density2d(aes(alpha=..level..,fill=srm_cat), geom = 'polygon', color = NA) +
    geom_jitter(aes(fill = srm_cat),alpha=.5, size=1, shape = 21, stroke = 0, color = '#f5f5f5', show.legend = F) +
    geom_label_repel(data = filter(style_df, count > 200), aes(x = ibu, y  = abv, label = Style_Master, fill = srm_cat, color = label_cat), 
                     segment.size = .5, show.legend = F, segment.color = 'gray25', segment.alpha = .8, size = 2) +
    scale_y_continuous(limits = c(0,15), breaks = c(seq(0,21,3))) +
    scale_x_continuous(limits = c(0,120), breaks = c(seq(0,150,15)), expand = c(0,2)) +
    scale_color_manual(values = c('#f5f5f5','black')) +
    scale_fill_manual(values = srm_pal) +
    labs(x="Bitterness (IBU's)", y="ABV %",fill = "Recipe SRM") +
    guides(colour = guide_legend(override.aes = list(size=5, alpha = .8))) +
    theme_minimal() +
    #theme_hc(bgcolor = "darkunica") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color='gray80', size = .3),
          panel.grid.major.y = element_line(color='gray80', size = .3),
          #axis.line = element_line(color='gray65'),
          plot.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
          panel.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
          text = element_text(family = 'Calibri', size=10),
          axis.text = element_text(color='gray25')) +
    scale_alpha_continuous(range = c(0,.7)) +
    guides(alpha = F, color = F) 


ggsave('recipe_scatter.png', scatter, device='png', height = 3.5, width= 7)

###########################



