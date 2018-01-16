

library(dplyr)
library(ggplot2)

recipes <- read.csv("bsrecipes.csv")

recipes <- recipes %>%
  group_by(Rec_ID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(ABV_pct = as.numeric(gsub("%","",ABV)),
         Bitterness_ibu = as.numeric(gsub(" IBUs","",Bitterness)),
         Color_srm = as.numeric(gsub(" SRM","",Color))) %>%
  dplyr::select(ABV_pct, Bitterness_ibu, Color_srm, Style_Master, Rec_Type = Type)

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


### anotate beer styles

style_df <- recipes %>%
  group_by(Style_Master) %>%
  summarise(count = n(),
            abv = mean(ABV_pct),
            ibu = mean(Bitterness_ibu),
            srm = mean(Color_srm)) %>%
  arrange(desc(count)) %>%
  filter(count > 200) %>%
  mutate(srm_cat = factor(srm_map(srm), levels = c("0 - 2.5","2.5 - 3.5","3.5 - 5","5 - 7.5",
                                                         "7.5 - 10.5","10.5 - 13.5","13.5 - 16.5","16.5 - 19",
                                                         "19 - 22","22 - 27","27 - 35","35+")),
         label_cat = ifelse(srm<13,'1','0'))
  
  
library(ggrepel)  
  

scatter <- recipes %>%
  mutate(srm_cat = factor(srm_map(Color_srm), levels = c("0 - 2.5","2.5 - 3.5","3.5 - 5","5 - 7.5",
                                                         "7.5 - 10.5","10.5 - 13.5","13.5 - 16.5","16.5 - 19",
                                                         "19 - 22","22 - 27","27 - 35","35+"))) %>%
 ggplot(aes(x=Bitterness_ibu,y=ABV_pct)) +
    stat_density2d(aes(alpha=..level..,fill=srm_cat), geom = 'polygon', color = NA) +
    geom_jitter(aes( fill = srm_cat),alpha=.4, size=.9, shape = 21, stroke = 0, color = 'white') +
    geom_label_repel(data = style_df, aes(x = ibu, y  = abv, label = Style_Master, fill = srm_cat, color = label_cat), 
                     segment.size = .7, show.legend = F, segment.color = 'white', segment.alpha = .8) +
    scale_y_continuous(limits = c(0,15), breaks = c(seq(0,21,3))) +
    scale_x_continuous(limits = c(0,120), breaks = c(seq(0,150,15)), expand = c(0,2)) +
    scale_color_manual(values = c('white','black')) +
    scale_fill_manual(values = srm_pal) +
    labs(x="Bitterness (IBU's)", y="ABV %",fill = "Recipe SRM") +
    guides(colour = guide_legend(override.aes = list(size=5, alpha = .8))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color='gray90', size=.5),
          axis.line = element_line(color='gray65'),
          #axis.ticks = element_line(color='gray65'),
          plot.background = element_rect(fill = 'white', color = NA),
          axis.text = element_text(size=13)) +
    scale_alpha_continuous(range = c(0,.7)) +
    guides(alpha = F, color = F) 


ggsave('scatter.png', scatter, device='png', height = 8, width= 14, bg='transparent')

theme_set(theme_gray())
plot_grid(bar,scatter, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))

###########################





ggplot(recipes, aes(x = "", y = Bitterness_ibu))+
  geom_boxplot() +
  scale_y_continuous(breaks=c(seq(0,150,10)), limits = c(1,150))

recipes %>%
  group_by(Style_Master) %>%
  summarise(avg_srm = mean(Color_srm),
            avg_bitterness = mean(Bitterness_ibu),
            count = n()) %>%
  mutate(srm_cat = factor(srm_map(avg_srm), 
                          levels = c("0 - 2.5","2.5 - 3.5",
                                     "3.5 - 5","5 - 7.5",
                                     "7.5 - 10.5","10.5 - 13.5",
                                     "13.5 - 16.5","16.5 - 19",
                                     "19 - 22","22 - 27",
                                     "27 - 35","35+"))) %>%
  filter(count >10) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(plotmax = max(count),
         cumsum = cumsum(count),
         datamax = max(cumsum),
         pct = cumsum / datamax,
         pareto = plotmax * pct) %>%
  ggplot(aes(x = reorder(Style_Master,-count), y =count, fill = srm_cat)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(breaks = c(seq(0,6500,500)),labels= scales::comma) +
  scale_fill_manual(values = srm_pal) +
  labs(x="", y="Number of Recipes", fill="Avg Recipe SRM") +
  geom_line(aes(x=reorder(Style_Master,-count),y=pareto, group = 1),
            show.legend = FALSE, alpha = .7, color="midnightblue") +
  geom_point(aes(x=reorder(Style_Master,-count),y=pareto),size=.9,
             show.legend = FALSE, alpha = .9,  color="midnightblue") +
  geom_text(aes(label = "80.5%",x = "Red Ale", y = 5340.237), 
            vjust = -1.4, hjust = .4, size = 3.1, color="midnightblue") +
  geom_point(aes(x = "Red Ale", y = 5470.237),color="midnightblue", 
             show.legend = FALSE, shape = 25, fill="midnightblue", size = 1.5)



