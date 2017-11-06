library(tidycensus)

# you need to set this yourself, instructions are in the tidycensus
# documentation, or just go to http://api.census.gov/data/key_signup.html
# after getting API key, paste it below and uncomment the line
# census_api_key("INSERTHERE")

# make a new dataframe that contains all of the variables for 2015
# TAKE NOTE: all populations for years other than 2000, 2010, etc. are
# estimated from survey data
v2015 <- load_variables(2015, "acs5")

# Load variables from 2010 census data instead
v2010 <- load_variables(2010, "sf1")

# Make a dataframe of state level census population data. Notice the count data is in 
# a column called "values"
state_2010 <- get_decennial(geography = "state", variables = "P0010001", year = "2010")

# Make a dataframe of state level survey population data. Notice the count data is in 
# a column called "estimate" Note that while the variable for population is actually
# "B01003_001E" you can leave off the last "E" and it returns the estimate and the 
# margin of error
state_2015 <- get_acs(geography = "state", variables = "B01003_001", year = "2015")

