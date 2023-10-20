# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-    #name of the test
  function(df) { 
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon") #creates the expected names, so we have something to test the existing column names up against
    
    if (all(colnames(df) == expected_colnames) == TRUE) { #return PASS if colnames in DF equals the expected ones
      print("PASS: Data has the correct columns") 
    } else{
      print("FAIL: Columns do not match the correct specification") #return fail if colnames in the dataframe dont equal the E colnames
    }
  }

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000 #setting up the expected values
    max_expected_rows <- 10000 #so that we can check the expected values up against ours
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows") #if the number of rows is between the min and max expected
    } else if (nrow(df) <= min_expected_rows) { #print pass, if its less print fail, if its higher print fail
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double") 
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
#^ tests wheter the columns are the correct type. using if statements
#to check expected values that we choose up against what our dataset got. 
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }
#^ checkign wheter the amount of missing values are reasonable, the expected
#value we check up against is 200, where the test prints fail if them missing 
#values are above 200. 
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }
#testing wheter the column latestdata got the correct timezone. should be in
# UTC. 

test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }
#a function that takes all the individual tests and run them through one 
# function. 




