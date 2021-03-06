library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(RMySQL)
library(tidyr)
library(scales)
library(RColorBrewer)
library(extrafont)

data <- read_excel("la_food/data/lac_violation_data.xlsx", sheet="lac_violation_data")
names(data) <- gsub(' ','_',names(data))
data$SITE_ZIP <- as.character(data$SITE_ZIP)


data2 <- data %>%
  group_by(NAME,ACTIVITY_DATE,SITE_CITY,SITE_ZIP) %>%
  summarise(SCORE = mean(SCORE),
            VIOLATIONS = sum(COUNT)) %>%
  filter(SCORE>60 & VIOLATIONS<40)

data3 <- data2 %>%
  group_by(NAME,SITE_CITY,SITE_ZIP) %>%
  arrange(desc(ACTIVITY_DATE)) %>%
  slice(1) %>%
  select(NAME,SITE_CITY:VIOLATIONS)

summary(lm(VIOLATIONS ~ SCORE, data=data3))

scatter <- data3 %>%
  ggplot(aes(x=VIOLATIONS,y=SCORE))+
  geom_jitter(alpha=.1,width=.2,height=.5, color = 'deepskyblue2', size = .5) +
   geom_smooth(method='lm', se=F, color = 'darkolivegreen2', linetype='dashed', size = .5) +
   geom_text(aes(x=17,y=82,label='R² .71'), color = 'darkolivegreen2', family = 'Calibri', size=4.5) +
  scale_y_continuous(breaks=c(seq(75,100,5)), limits = c(75,100)) +
  scale_x_continuous(breaks=c(seq(0,20,5)), limits = c(0,20))+
  theme(panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text = element_text(color='gray80', size = 16),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.title.x = element_text(margin = margin(.6,0,0,0, unit = 'cm')),
        axis.ticks.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'),
        plot.margin = margin(.7,0,.7,.7, unit = 'cm'),
        # panel.grid.major.y = element_line(color = 'gray25', size = .1),
        text = element_text(color = 'gray80', family = 'Calibri', size = 18))+
  labs(x = 'Number of Violations', y = 'Health Score')

histo <- data3 %>%
  ggplot(aes(x=VIOLATIONS))+
  geom_histogram(bins=35,colour="#211e1e",fill="deepskyblue2")+
  scale_x_continuous(breaks=c(seq(0,20,5))) +
  scale_y_continuous(breaks = seq(0,6000,3000), expand = c(0,0), limits = c(0,6200), 
                     labels = function(x) ifelse(x!=0, paste0(x/1000,'k'),0))+
  labs(y="Count", x = 'Number of Violations')+
  theme(panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text = element_text(color='gray80', size = 16),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.title.x = element_text(margin = margin(.6,0,0,0, unit = 'cm')),
        axis.ticks.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'),
        plot.margin = margin(.7,.7,.7,.7, unit = 'cm'),
        # panel.grid.major.y = element_line(color = 'gray25', size = .1),
        text = element_text(color = 'gray80', family = 'Calibri', size = 18)) +
  coord_cartesian(xlim = c(0,20))

histo2 <- data3 %>% 
  ggplot(aes(x=SCORE))+
  geom_histogram(bins=40,colour="#211e1e",fill="deepskyblue2")+
  scale_x_continuous(breaks=c(seq(75,100,5))) +
  scale_y_continuous(breaks = seq(0,6000,3000), expand = c(0,0), limits = c(0,6500), 
                     labels = function(x) ifelse(x!=0, paste0(x/1000,'k'),0))+
  labs(y="Count", x = 'Health Score')+
  theme(panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text = element_text(color='gray80', size = 16),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.ticks.y = element_line(color = 'gray80'),
        axis.title.x = element_text(margin = margin(.6,0,0,0, unit = 'cm')),
        axis.ticks.x = element_line(color = 'gray80'),
        plot.margin = margin(.7,.7,.7,.7, unit = 'cm'),
        # panel.grid.major.y = element_line(color = 'gray25', size = .1),
        text = element_text(color = 'gray80', family = 'Calibri', size = 18)) +
  # coord_flip(xlim = c(75,100))
  coord_cartesian(xlim = c(75,100))

# plot_grid(histo,scatter, align = "v", nrow = 2, rel_heights = c(1/3, 2/3))
# 
# plot_grid(scatter,histo2, ncol = 2, rel_widths = c(2/3,1/3))
# 
# plot_grid(p1, histo2, ncol = 2, rel_widths = c(2/3,1/3), rel_heights = c(2/3,1/3))

ggsave('la_food/eda_1.png', scatter, device='png', height = 5, width= 6)
ggsave('la_food/eda_2.png', histo, device='png', height = 2.5, width= 5)
ggsave('la_food/eda_3.png', histo2, device='png', height = 2.5, width= 5)

############################################################################

mydb = dbConnect(MySQL(), user='root', password='43703016', dbname='scraping', host='localhost')

setOldClass(c("grouped_df", "tbl_df", "data3"))

dbWriteTable(mydb, 
             name='la_health', 
             value=data3, 
             field.types=list(NAME="VARCHAR(100)", 
                              SITE_CITY="VARCHAR(50)", 
                              SITE_ZIP="VARCHAR(20)", 
                              SCORE="INT(7)", 
                              VIOLATIONS="INT(7)"), 
             row.names=FALSE)

############################################################################
dbClearResult(dbListResults(mydb)[[1]])
############################################################################

yelp <- dbSendQuery(mydb, 'SELECT * FROM yelp_data')
yelp_df = fetch(yelp, n=-1)


yelp_df <- yelp_df %>%
  filter(matchScore > 50) %>%
  mutate(yelpid = paste(yelpName,yelpCity,yelpZip, sep="")) %>%
  group_by(yelpid) %>%
  arrange(desc(matchScore)) %>%
  slice(1) %>%
  ungroup() %>%
  select(index:yelpZip)

ggplot(yelp_df,aes(x=laScore,y=yelpRating))+
  geom_jitter(alpha=.1,width=1,height=.5) +
  geom_smooth()


categories <- yelp_df %>%
  select(yelpCat1:yelpCat3) %>%
  gather(cat,name) %>%
  select(name) %>%
  group_by(name) %>%
  summarise(count = n())

write.excel(categories)
catlist <- read.excel()

lm(data3$VIOLATIONS ~ data3$SCORE)

coeff_score_b <- 1.1659144
coeff_score_a <- 1/(coeff_score_b^100)
coeff_vio_yint <- 62.201
coeff_vio_score <- -.6121
coeff_reviews_a <- .1162
coeff_reviews_b <- 1.0898

yelp_df <- yelp_df %>%
  mutate(transfScore = coeff_score_a * (coeff_score_b ^ laScore),
         transfVio1 = coeff_vio_yint + (coeff_vio_score * laScore),
         transfVio2 = (transfVio1 - laViolations) / transfVio1 / 10,
         transfRate = yelpRating / 5,
         transfRev = coeff_reviews_a * log(coeff_reviews_b * yelpReviews),
         healthScore = transfScore + transfVio2,
         weightedScore = (healthScore + (transfRate * transfRev)) / 2) %>%
  select(index:yelpZip,weightedScore)


boxp1 <- yelp_df %>%
  select(weightedScore,yelpCat1:yelpCat3) %>%
  gather(col,Category,-weightedScore) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent)) %>%
  group_by(Parent) %>%
  mutate(mscore = median(weightedScore)) %>%
  ggplot(aes(x=reorder(Parent,mscore),y=weightedScore)) +
    geom_boxplot(colour="dodgerblue", fill="black") +
    scale_y_continuous(limits = c(0,.8)) +
    coord_flip()

boxp2 <- yelp_df %>%
  select(weightedScore,yelpCat1:yelpCat3) %>%
  gather(col,Category,-weightedScore) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent)) %>%
  group_by(Category) %>%
  mutate(mscore = median(weightedScore)) %>%
  ggplot(aes(x=reorder(Category,mscore),y=weightedScore, fill=Parent2)) +
    geom_boxplot() +
    scale_y_continuous(limits = c(0,.8)) +
    scale_fill_brewer(palette="Set1") +
    labs(x = "", y= "Weighted Score", fill = "") +
    theme(axis.text.y = element_text(size=9),
          legend.position="top",
          legend.text = element_text(size=9)) +
    coord_flip()

regions <- read.excel()


tilep1 <- yelp_df %>%
  select(yelpCity,weightedScore,yelpCat1:yelpCat3) %>%
  gather(colName,Category,-c(yelpCity,weightedScore)) %>%
  left_join(catlist, by ="Category") %>%
  filter(!is.na(Parent)) %>%
  left_join(regions, by = "yelpCity") %>%
  group_by(Region,Parent2) %>%
  summarise(meanScore = mean(weightedScore), countscore = n()) %>%
  ungroup() %>%
  ggplot(aes(x=Parent2,y=Region)) +
    geom_tile(aes(fill = countscore), colour = "white") + 
    scale_fill_gradient(low = "white", high = "dodgerblue",na.value = 'white')+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    theme(panel.background=element_rect(fill="white", colour="white")) +
    labs(fill = "Mean Score", y = "", x = "")


plot_grid(boxp1,tilep1, align = "h", ncol = 2, rel_widths = c(1/3, 2/3))


yelp_df %>%
  filter(yelpCity == "SANTA MONICA", yelpCat1 %in% catlist$Category) %>%
  arrange(desc(weightedScore)) %>%
  slice(1:20) %>%
  select(yelpName,yelpRating,yelpReviews,laScore,laViolations,weightedScore)

###############################################################

cities <- yelp_df %>%
  group_by(yelpCity) %>%
  summarise(count = n())

parents <- yelp_df %>%
  select(yelpCat1:yelpCat3) %>%
  gather(colName,Category) %>%
  left_join(catlist, by ="Category") %>%
  filter(!is.na(Parent)) %>%
  group_by(Parent) %>%
  summarise(count = n())

write.excel(parents)

yelp_df %>%
  select(weightedScore,yelpCat1:yelpCat3) %>%
  gather(col,Category,-weightedScore) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent)) %>%
  ggplot(aes(x=weightedScore, fill=Parent2)) +
    geom_density(position="fill",alpha=.7) +
    scale_fill_brewer(palette="Set1") +
    labs(x = "Weighted Score", y = "Density", fill = "")

yelp_df %>%
  select(weightedScore,yelpCat1:yelpCat3) %>%
  gather(col,Category,-weightedScore) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent)) %>%
  group_by(Category) %>%
  mutate(mscore = median(weightedScore)) %>%
  ggplot(aes(x=reorder(Category,mscore),y=weightedScore, fill=Parent2)) +
    geom_boxplot(outlier.colour = "black",outlier.size = 1) +
    scale_y_continuous(limits = c(0,.8)) +
    scale_fill_brewer(palette="Set1") +
    labs(x = "", y= "Weighted Score", fill = "") +
    theme(axis.text.y = element_text(size=7.5)) +
    theme_void() +
    guides(fill=FALSE)

eq = function(x){x*x*x}
eq2 = function(x){log(x)}
eq3 = function(x){62.2 + -.6121*x}
eq4 = function(x){x}
hsp <- ggplot(data.frame(x=c(50, 100)), aes(x=x)) + 
  stat_function(fun=eq, geom="line", color = 'white') + 
  labs(title="1. Health Score", y="Review Score Impact", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        text = element_text(family = 'Calibri', color = 'gray80'),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text.x = element_text(color = 'gray80'),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'))
rcp <- ggplot(data.frame(x=c(0, 10000)), aes(x=x)) + 
  stat_function(fun=eq2, geom="line", color = 'white') + 
  scale_x_continuous(labels = function(x) paste0(x/1000,'k'))+
  labs(title="4. Review Count", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        text = element_text(family = 'Calibri', color = 'gray80'),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text.x = element_text(color = 'gray80'),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'))
vcp <- ggplot(data.frame(x=c(0, 25)), aes(x=x)) + 
  stat_function(fun=eq3, geom="line", color = 'white') + 
  labs(title="2. Violation Count", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        text = element_text(family = 'Calibri', color = 'gray80'),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text.x = element_text(color = 'gray80'),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'))
yrp <- ggplot(data.frame(x=c(0, 5)), aes(x=x)) + 
  stat_function(fun=eq4, geom="line", color = 'white') + 
  labs(title="3. Yelp Rating", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        text = element_text(family = 'Calibri', color = 'gray80'),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        axis.text.x = element_text(color = 'gray80'),
        axis.line.x = element_line(color = 'gray80'),
        axis.line.y = element_line(color = 'gray80'),
        axis.ticks.x = element_line(color = 'gray80'))

final <- plot_grid(hsp,vcp,yrp,rcp, ncol=4)

rel_widths = c(.245,.245,.245,.265)

ggsave('bootstrap/img/food_metric.svg', final, device='svg', height = 3, width= 10)

plot_grid(pgp,wsp, align = "v", nrow = 2, rel_heights = c(.3,.7))

yelp_df2 <- yelp_df %>%
  mutate(Category = yelpCat1) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent))

write.csv(yelp_df2,"yelp_df.csv")
