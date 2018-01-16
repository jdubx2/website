ingredients <- read.csv("bsingredients.csv")


ingredients2 <- ingredients %>%
  group_by(Rec_ID,Ing_Type,Ingredient_simple) %>%
  summarise(count = 1)

ingredient_nodes <- ingredients2 %>%
  group_by(Ingredient_simple,Ing_Type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ingredient_nodes <- ingredient_nodes %>%
  filter(count >20, 
         Ing_Type %in% c("Grain","Hops","Yeast"),
         Ingredient_simple != "None",
         !(Ingredient_simple == "Brewer" & Ing_Type == "Grain"),
         !(Ingredient_simple == "Crystal" & Ing_Type == "Grain"))

ingredient_nodes %>%
  ggplot(aes(x=Ing_Type,y=count))+
  geom_boxplot() +
  scale_y_continuous(limits = c(0,18000))

node_vector <- ingredient_nodes$Ingredient_simple

ingredient_edges <- ingredients2 %>%
  filter(Ingredient_simple %in% node_vector) %>%
  group_by(Rec_ID) %>%
  mutate(ing_num = cumsum(count),
         max_ing = max(ing_num)) %>%
  ungroup() %>%
  select(Ingredient_simple,ing_num,max_ing)
  
edge_list <- list()
elpos <- 1
sequence <- c(1:nrow(ingredient_edges))
sequence <- c(1:30)

for (i in seq_along(sequence)){
  pos = 0
  while (ingredient_edges$ing_num[[i+pos]] < ingredient_edges$max_ing[[i+pos]]) {
    edge_list[[elpos]] <- c(as.character(ingredient_edges$Ingredient_simple[[i]]),
                            as.character(ingredient_edges$Ingredient_simple[[i+pos+1]]))
    pos <- pos + 1
    elpos <- elpos + 1
  }
}

edgedf <- data.frame(matrix(unlist(edge_list), nrow=588502, byrow=T))

edgedf <- edgedf %>%
  group_by(X1,X2) %>%
  summarise(count = n()) %>%
  select(from = X1, to = X2, weight = count)

write.csv(edgedf, "beersmithedges.csv")

ingredient_nodes <- ingredient_nodes %>%
  mutate(Ing_Type_N = ifelse(Ing_Type == "Grain", 1,
                             ifelse(Ing_Type == "Hops", 2, 3)))

hop_lookup <- read.excel()
aa_lookup <- read.excel()

hop_nodes <- ingredient_nodes %>%
  filter(Ing_Type == "Hops") %>%
  select(ingredient = Ingredient_simple, count) %>%
  left_join(hop_lookup, by = "ingredient") %>%
  filter(!is.na(ing_map)) %>%
  group_by(ing_map) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  select(ingredient = ing_map, count) %>%
  left_join(aa_lookup, by ="ingredient")

hop_edges <- edgedf %>%
  left_join(hop_lookup, by = c("from" = "ingredient")) %>%
  left_join(hop_lookup, by = c("to" = "ingredient")) %>%
  filter(!(is.na(ing_map.x)) & !(is.na(ing_map.y))) %>%
  mutate(from1 = ifelse(as.character(ing_map.x) < as.character(ing_map.y),as.character(ing_map.x),as.character(ing_map.y)),
         to1 = ifelse(from1 == as.character(ing_map.x),as.character(ing_map.y),as.character(ing_map.x))) %>%
  group_by(from1,to1) %>%
  summarise(weight = sum(weight)) %>%
  ungroup %>%
  arrange(desc(weight)) %>%
  group_by(from1) %>%
  mutate(from_max = max(weight)) %>%
  ungroup() %>%
  group_by(to1) %>%
  mutate(to_max = max(weight),
         keep = ifelse(weight > 300 | weight == from_max | weight == to_max, 1, 0)) %>%
  filter(keep == 1)
  
hop_nodes <- hop_nodes %>%
  filter(count > 500)

hop_edges <- hop_edges %>%
  filter(from1 %in% hop_nodes$ingredient & to1 %in% hop_nodes$ingredient)


library(igraph)
library(RColorBrewer)

net <- graph_from_data_frame(d=hop_edges,vertices=hop_nodes, directed = F)

net <- simplify(net, remove.multiple = F, remove.loops = T)

colrs <- c(brewer.pal(9, "Greens")[c(3:8)])

#####
V(net)$label.font <- 2
V(net)$label <- NA
#####

V(net)$color <- colrs[V(net)$aa_group]
V(net)$size <- sqrt(V(net)$count) * 250
V(net)$frame.color <- "gray80"
V(net)$label.family <- "Helvetica"
V(net)$label.color <- "gray1"

E(net)$color <- "gray48"
E(net)$width <- sqrt(E(net)$weight)/3.5/1.5
E(net)$curved <- .2

l <- layout_with_fr(net)
l <- layout_in_circle(net)
l <- layout.fruchterman.reingold(net) * 1.8

par(bg = 'gray')
plot(net, rescale = TRUE)

plot(net, layout = l, rescale = FALSE, ylim=c(50,750),xlim=c(100,1150), asp = 0)
legend(x=150, y=175, c("3% - 5.5%","5.5% - 8%", "8% - 10.5%", "10.5% - 13%", "13% - 15.5%", "15.5% - 18%"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1, title="Average Alpha Acid %", title.adj=.4)

tkid <- tkplot(net, layout = l)
l <- tkplot.getcoords(tkid)

##########################################

grain_lookup <- read.excel()
lovi_lookup <- read.excel()

grain_nodes <- ingredient_nodes %>%
  filter(Ing_Type == "Grain") %>%
  select(ingredient = Ingredient_simple, count) %>%
  left_join(grain_lookup, by = "ingredient") %>%
  filter(!is.na(ing_map)) %>%
  group_by(ing_map) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  select(ingredient = ing_map, count) %>%
  left_join(lovi_lookup, by ="ingredient")

grain_edges <- edgedf %>%
  left_join(grain_lookup, by = c("from" = "ingredient")) %>%
  left_join(grain_lookup, by = c("to" = "ingredient")) %>%
  filter(!(is.na(ing_map.x)) & !(is.na(ing_map.y))) %>%
  mutate(from1 = ifelse(as.character(ing_map.x) < as.character(ing_map.y),as.character(ing_map.x),as.character(ing_map.y)),
         to1 = ifelse(from1 == as.character(ing_map.x),as.character(ing_map.y),as.character(ing_map.x))) %>%
  group_by(from1,to1) %>%
  summarise(weight = sum(weight)) %>%
  ungroup %>%
  arrange(desc(weight)) %>%
  group_by(from1) %>%
  mutate(from_max = max(weight)) %>%
  ungroup() %>%
  group_by(to1) %>%
  mutate(to_max = max(weight),
         keep = ifelse(weight > 1000 | weight == from_max | weight == to_max, 1, 0)) %>%
  filter(keep == 1)


#GRAIN IGRAPH

net2 <- graph_from_data_frame(d=grain_edges,vertices=grain_nodes, directed = F)

net2 <- simplify(net2, remove.multiple = F, remove.loops = T)

colrs2 <- srm_pal[c(1,3,5,7,9,11)]

#####
V(net2)$label.font <- 2
V(net2)$label <- NA
#####

V(net2)$color <- colrs2[V(net2)$lovibond_scale]
V(net2)$size <- sqrt(V(net2)$count) * 250  /2
V(net2)$frame.color <- "gray80"
V(net2)$label.family <- "Helvetica"
V(net2)$label.color <- ifelse(V(net2)$lovibond_scale == 6 | V(net2)$lovibond_scale == 5,"goldenrod","gray1")

E(net2)$color <- "gray48"
E(net2)$width <- sqrt(E(net2)$weight)/3.5/4
E(net2)$curved <- .2


par(bg = 'gray')
plot(net2, rescale = TRUE)

plot(net2, layout = l2c, rescale = FALSE, ylim=c(50,750),xlim=c(100,1150), asp = 0)
legend(x=125, y=800, c("< 3L","2L - 10L", "10L- 30L", "30L - 150L", "150L - 350L", "> 350L"), pch=21,
       col="#777777", pt.bg=colrs2, pt.cex=2, cex=.8, bty="n", ncol=1, title="Lovibond Scale", title.adj=.1)

tkid2 <- tkplot(net2, layout=l2)
l2 <- tkplot.getcoords(tkid2)
l2c <- tkplot.getcoords(tkid2)

plot(net2, layout = l2c, rescale = FALSE, ylim=c(100,600),xlim=c(50,950), asp = 0)
legend(x=40, y=170, c("< 3L","2L - 10L", "10L- 30L", "30L - 150L", "150L - 350L", "> 350L"), pch=21,
       col="#777777", pt.bg=colrs2, pt.cex=2, cex=.8, bty="n", ncol=1, title="Lovibond Scale", title.adj=.1)
