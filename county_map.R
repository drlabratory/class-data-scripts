# Going to try and plot Missouri population by county

library(tidycensus)
library(maps)
library(scales)
library(stringr)

# This grabs the census survey data from acs for 2015 
mo_pop <- get_acs(geography = "county", 
                  variables = "B01003_001", 
                  year = "2015", 
                  state = "MO")

# This creates a dataframe to map the counties to geographic regions
mapping <- map_data(map = "county", region = "missouri")

# This renames the column name "subregion" to "region" because geom_map
# has a bug (feature?) that it always uses the region column
mapping <- mapping %>%
  select(long,
         lat,
         group,
         order,
         region = subregion)

#we need to change the data in the NAME column to get rid of part of it
mo_pop <- mo_pop %>% 
  separate(NAME, "tmp_county", sep = ", Missouri") %>% 
  separate(tmp_county, "county", sep = " County")

# Now we need to get rid of case and stupid periods
# But! periods are special characters, and need to be "escaped"
# That's what the "\\." does
mo_pop$county <- mo_pop$county %>%  
  str_to_lower() %>% 
  str_replace_all("\\.", "")

# TODO add a join command and plot using geom_polygon in order to draw
# borders. Ugh.

plot <- ggplot(mo_pop, aes(fill = estimate))
# the map = mapping makes sure ggplot knows what to use
plot + 
  geom_map(aes(map_id = county), 
                map = mapping) + 
  expand_limits(x=mapping$long, y=mapping$lat) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(
    high = muted("red"), 
    low = "white",
    name = "Population by County"
  ) +
  theme(legend.position = "right")

