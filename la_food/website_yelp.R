library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(RMySQL)
library(tidyr)
library(scales)
library(RColorBrewer)

data <- read_excel("lac_violation_data.xlsx", sheet="lac_violation_data")
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

scatter <- data3 %>%
  ggplot(aes(x=SCORE,y=VIOLATIONS))+
  geom_jitter(alpha=.1,width=.2,height=.5) +
  geom_smooth(method='lm') +
  scale_y_continuous(breaks=c(seq(0,40,5))) +
  scale_x_continuous(breaks=c(seq(0,100,5)))

histo <- data3 %>%
  ggplot(aes(x=SCORE))+
  geom_histogram(bins=40,colour="gray",fill="black")+
  scale_x_continuous(breaks=c(seq(0,100,5))) +
  labs(y="COUNT")

theme_set(theme_gray())
plot_grid(histo,scatter, align = "v", nrow = 2, rel_heights = c(1/3, 2/3))

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

wsp <- yelp_df %>%
  ggplot(aes(x=weightedScore)) +
  geom_histogram(bins=60,colour="dodgerblue",fill="black") +
  labs(x= "Weighted Score", y="Count")

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

eq = function(x){x*x*x*x*x*x}
eq2 = function(x){log(log(x))}
eq3 = function(x){62.2 + -.6121*x}
eq4 = function(x){x}
hsp <- ggplot(data.frame(x=c(50, 100)), aes(x=x)) + 
  stat_function(fun=eq, geom="line") + 
  labs(title="1. Health Score", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5))
rcp <- ggplot(data.frame(x=c(0, 10000)), aes(x=x)) + 
  stat_function(fun=eq2, geom="line") + 
  labs(title="4. Review Count", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5))
vcp <- ggplot(data.frame(x=c(0, 25)), aes(x=x)) + 
  stat_function(fun=eq3, geom="line") + 
  labs(title="2. Violation Count", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5))
yrp <- ggplot(data.frame(x=c(0, 5)), aes(x=x)) + 
  stat_function(fun=eq4, geom="line") + 
  labs(title="3. Yelp Rating", y="", x="") +
  theme(axis.text.y = element_blank(),plot.title = element_text(hjust = 0.5))

pgp <- plot_grid(hsp,vcp,yrp,rcp, align = "h", ncol = 4, rel_heights = c(.25,.25,.25,.25))

plot_grid(pgp,wsp, align = "v", nrow = 2, rel_heights = c(.3,.7))

yelp_df2 <- yelp_df %>%
  mutate(Category = yelpCat1) %>%
  left_join(catlist, by = "Category") %>%
  filter(!is.na(Parent))

write.csv(yelp_df2,"yelp_df.csv")
