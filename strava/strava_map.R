

test <- filter(strava, File == '20130209-203200-Ride.gpx') %>% select(File,Latitude,Longitude)
rides <- filter(strava, Activity == 'Ride') %>% select(File,Latitude,Longitude)


# Sample geojson - linestring
# {
#   "type": "LineString",
#   "coordinates": [[-100, 40], [-105, 45], [-110, 55]]
# }

# Sample geojson - multilinestring
# {
#   "type": "MultiLineString",
#   "coordinates": [
#     [
#       [
#         -105.0214433670044,
#         39.57805759162015
#         ],
#       [
#         -105.02150774002075,
#         39.57780951131517
#         ],
#       ],
#     [
#       [
#         -105.01989841461182,
#         39.574997872470774
#         ],
#       [
#         -105.01959800720215,
#         39.57489863607502
#         ]
#       ]
#     ]
#   }


# linestring (test)
json_vec <- c()
for(row in seq(1,nrow(test))){
  json_vec <- c(json_vec, paste0('[',test$Longitude[row],',',test$Latitude[row],']'))
}

json_string <- paste0('{"type":"LineString","coordinates":[',paste(json_vec, sep='', collapse = ','),']}')

write(json_string, 'gjson_test.json')

# multiline string

test <- filter(rides, File %in% c('20130209-203200-Ride.gpx','20130221-015007-Ride.gpx'))

get_mls_json <- function(df){
  
  final_vec <- c()
  
  for(file in unique(df$File)){
    
    sub_df <- filter(df, File == file)
    json_vec <- c()
    for(row in seq(1,nrow(sub_df))){
      json_vec <- c(json_vec, paste0('[',sub_df$Longitude[row],',',sub_df$Latitude[row],']'))
    }
    
    final_vec <- c(final_vec, paste0('[',paste(json_vec, sep = '', collape = ','),']'))
  }
  #return(paste0('{"type":"MultiLineString","coordinates":[[',final_vec,']]}'))
  return(final_vec)
}

rides_mls <- get_mls_json(test)

write(rides_mls, 'rides_mls.json')






