library(tidycensus)

# you need to set this yourself, instructions are in the tidycensus
# documentation, or just go to http://api.census.gov/data/key_signup.html
# after getting API key, paste it below and uncomment the line
# census_api_key("INSERTHERE")

# make a new dataframe that contains all of the variables for 2016
# TAKE NOTE: all populations for years other than 2000, 2010, etc. are
# estimated from survey data
v2016 <- load_variables(2015, "acs5")

# Load variables from 2010 census data

v2010 <- load_variables(2010, "sf1")

state_2010 <- get_decennial(geography = "state", variables = "P0010001", year = "2010")

state_2016 <- get_acs(geography = "state", variables = "B19013_001", year = "2016")