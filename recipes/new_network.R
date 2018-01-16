
library(dplyr)
library(ggplot2)
library(tidyr)

ingredients <- read.csv("bsingredients.csv", stringsAsFactors = F)
recipes <- read.csv("bsrecipes.csv", stringsAsFactors = F)


### Remove Duplicates / select cols for join
recipes <- recipes %>%
  group_by(Rec_ID) %>%
  filter(row_number() == 1) %>%
  select(Rec_ID, Style_Master)


##3 Join style_master to ingredients, fix names
ingredients <- ingredients %>%
  left_join(recipes, by = 'Rec_ID') %>%
  filter(!(is.na(Style_Master))) %>%
  select(ing_type = Ing_Type, ing = Ingredient, ing_simple = Ingredient_simple,
         rec_id = Rec_ID, style = Style_Master)


### imported style_df dataframe from beersmith_recipes.r to filter out frequent styles 
style_df <- select(style_df, Style_Master)


### join style_df table to filter out low volume styles
### remove combos with less than 20 appearances to prune nodes & filter some edge cases
ingredient_nodes <- ingredients %>%
  group_by(ing_simple, ing_type, style) %>%
  filter(ing_type %in% c("Grain","Hops","Yeast")) %>%
  summarise(count = n()) %>%
  inner_join(style_df, by = c('style' = 'Style_Master')) %>%
  arrange(desc(count)) %>%
  filter(count > 20,
         ing_simple != "None",
         !(ing_simple == "Brewer" & ing_type == "Grain"),
         !(ing_simple == "Crystal" & ing_type == "Grain"))


### Manual cleanup in excel to merge simmilar nodes
# in_cleanup <- ingredient_nodes %>%
#   group_by(ing_simple, ing_type) %>%
#   summarise()
# 
# write.csv(in_cleanup, 'in_cleanup.csv', row.names = F)
in_cleanup <- read.csv('in_cleanup.csv', stringsAsFactors = F)


### join cleaned ingredient column, recalc the count, spread to table format by style
ingredient_nodes <- ingredient_nodes %>%
  left_join(in_cleanup, by = c('ing_simple','ing_type')) %>%
  filter(ing_final != '') %>%
  group_by(ing_final, ing_type, style) %>%
  summarise(count = sum(count)) %>%
  spread(style, count)

### check for overallping names regardless of ing_type
ingredient_nodes %>% group_by(ing_final) %>% summarise(count = n()) %>% arrange(desc(count))


### filter out ingredients not in node list to prepare list of edges
pre_edge_df <- ingredients %>%
  inner_join(in_cleanup, by = c('ing_simple','ing_type')) %>%
  filter(ing_final != '') %>%
  inner_join(ingredient_nodes, by = 'ing_final') %>%
  select(ing_final, rec_id, style) %>%
  group_by(ing_final, rec_id, style) %>%
  summarise() %>%
  arrange(rec_id,ing_final)


  ### DPLYR Filter too slow (3.6 sec per recipe) create min/max indexing of recipes instead
    rec_index_df<- pre_edge_df %>%
      ungroup() %>%
      mutate(row_num = row_number()) %>%
      group_by(rec_id) %>%
      mutate(min_row = min(row_num),
             max_row = max(row_num)) %>%
      group_by(rec_id, min_row, max_row) %>%
      summarise()


    ### Function for getting combo dataframes and adding to final list
    get_combos <- function(min_idx, max_idx) {
        
          filtered <- pre_edge_df[min_idx:max_idx,]
          rec_style = unique(filtered$style)
          
          combo_matrix <- combn(filtered$ing_final, 2)
          
          combo_df <- as.data.frame(t(combo_matrix), stringsAsFactors = F) %>%
            mutate(style = rec_style)
  
          return(combo_df)
    }
    
    ### Itterate through each rec min/max in rec_index_df and get combos 
    ### add combos to final list and concatenate
    
    combo_list <- list()
    i = 1
    
    for(row_idx in seq_len(nrow(rec_index_df))){
      
      min <- as.numeric(rec_index_df[row_idx, 2])
      max <- as.numeric(rec_index_df[row_idx, 3])
      
      if(max > min){
        combo_list[[i]] <- get_combos(min,max)
        i <- i + 1 
        }
    }
    
    combo_df_final <- bind_rows(combo_list)
    
    ingredient_edges <- combo_df_final %>%
      group_by(V1, V2, style) %>%
      summarise(count = n()) %>%
      spread(style, count)
    
    
### Nodes and edges complete ###
    
### turn NA's to 0
### filter porter only for experimentation
    
    
    ingredient_edges[is.na(ingredient_edges)] <- 0
    ingredient_nodes[is.na(ingredient_nodes)] <- 0
      
    
    porter_edges <- ingredient_edges %>%
      ungroup() %>%
      select(source = V1, target = V2, value = Porter) %>%
      filter(value > 50) %>%
      mutate(value = sqrt(value))
    
    edge_filter <- data.frame(id = unique(gather(porter_edges, nothing, name, -value)$name), stringsAsFactors = F)
    
    porter_nodes <- ingredient_nodes %>%
      ungroup() %>%
      select(id = ing_final, group = ing_type, value = Porter) %>%
      filter(value > 0) %>%
      inner_join(edge_filter, by = 'id')
    
  library(jsonlite)
    
    write_json(list(nodes = porter_nodes, links = porter_edges), 'ing_net.json')
    
    
    cbind(sort(unique(gather(porter_edges, nothing, name, -value)$name)),sort(porter_nodes$id))
    


