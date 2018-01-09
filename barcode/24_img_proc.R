library(bmp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(extrafont)

setwd("~/Jesse/24")

#extract RGB from images in working direcctory and record mean values in dataframe
image_list <- list()
for(idx in seq_along(list.files())) {
  
  image <- read.bmp(list.files()[idx])
  image_list[[idx]] <- data.frame(red = round(mean(image[,,1]),0), 
                                  green = round(mean(image[,,2]),0), 
                                  blue = round(mean(image[,,3]),0))
}
image_df <- bind_rows(image_list)

#plot of RGB color densities
dens <- image_df %>%
  gather(key,value) %>%
  ggplot(aes(x = value, fill = key))+
  geom_density(alpha=.6, color = 'gray20') +
  scale_fill_manual(values = c('dodgerblue','green1', 'firebrick3')) +
  theme_hc(bgcolor = "darkunica") +
  theme(axis.text = element_text(color='gray80'),
        panel.grid.major.y = element_line(color='gray25', size = .3),
        panel.grid.major.x = element_line(color='gray25', size = .3),
        text = element_text(family ='Calibri',size = 11),
        panel.background = element_rect(fill = '#211e1e', color = '#211e1e'),
        plot.background = element_rect(fill = '#211e1e', color = '#211e1e')) +
  guides(fill = F) +
  labs(x = 'RGB Value', y = 'Density')

ggsave(file="24_dens.svg", plot=dens, width=7, height=2)

#convert rgb to hex & add row number for x-axis ordering
image_df2 <- image_df %>%
  mutate(hex = rgb(red/255,green/255,blue/255),
         row_num = row_number())

#create hex lookup array for plotting
col <- as.character(image_df2$hex)
names(col) <- as.character(image_df2$hex)

#plot barcode
plot <- image_df2 %>%
  ggplot(aes(x = row_num, y = 1, fill = hex, color = hex))+
  geom_col()+
  scale_fill_manual(values = col) +
  scale_color_manual(values = col) +
  guides(color=F,fill=F) +
  theme_void()

ggsave(file="24.svg", plot=plot, width=15, height=5)

#plot circular barcode using polar coordinates
plot2 <- plot + coord_polar()

ggsave(file="24_2.png", plot=plot2, bg='transparent', width=5, height=5)


#plotting code for blog entry
library(grid)
library(gridExtra)

plots <- grid.arrange(plot + theme(plot.background = element_rect(fill = '#211e1e', color = '#211e1e')),
             plot2 + theme(plot.background = element_rect(fill = '#211e1e', color = '#211e1e')), ncol=2)

ggsave(file="24_barcodes.png", plot=plots, width=8, height=2, bg = "transparent")

