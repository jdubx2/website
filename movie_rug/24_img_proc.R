library(bmp)
library(dplyr)
library(tidyr)
library(ggplot2)

library(stringr)

library(grid)
library(gridExtra)

setwd("~/Jesse/24")


image_list <- list()

for(idx in seq_along(list.files())) {
  
  image <- read.bmp(list.files()[idx])
  image_list[[idx]] <- data.frame(red = round(mean(image[,,1]),0), 
                                  green = round(mean(image[,,2]),0), 
                                  blue = round(mean(image[,,3]),0))
  
}

image_df <- bind_rows(image_list)


image_df %>%
  gather(key,value) %>%
  ggplot(aes(x = value, fill = key))+
  geom_density(alpha=.5)

image_df2 <- image_df %>%
  mutate(hex = rgb(red/255,green/255,blue/255),
         row_num = row_number())

col <- as.character(image_df2$hex)
names(col) <- as.character(image_df2$hex)

plot <- image_df2 %>%
  ggplot(aes(x = row_num, y = 1, fill = hex, color = hex))+
  geom_col()+
  scale_fill_manual(values = col) +
  scale_color_manual(values = col) +
  guides(color=F,fill=F) +
  theme_void()


ggsave(file="24.svg", plot=plot, width=15, height=5)

ggsave(file="24_2.png", plot=plot2, bg='transparent', width=5, height=5)

plot2 <- plot + coord_polar()
