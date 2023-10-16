#data_transformations.R
library(purrr)

transform_metadata_to_df <- function(stations_metadata) {
  df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData,1, .default=NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>%      
    unnest_wider(latLon)      
  
  return(df)
}


to_iso8601 <- function(datetime, offset_days) {
  # Add the offset
  new_datetime <- datetime + offset_days*24*60*60  # offset_days converted to seconds
  
  # Format as ISO8601 and append "Z"
  return(format(new_datetime, format="%Y-%m-%dT%H:%M:%SZ"))
}

# Test the function
to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)


## transform volumes function

transform_volumes <- function(api_response) {
  # Navigate to the 'edges' section of the response
  edges <- api_response$trafficData$volume$byHour$edges
  
  # Extract necessary information from each node
  df <- purrr::map_dfr(edges, function(edge) {
    tibble(
      from = as_datetime(edge$node$from),
      to = as_datetime(edge$node$to),
      volume = edge$node$total$volumeNumbers$volume
    )
  })
  
  return(df)
}



