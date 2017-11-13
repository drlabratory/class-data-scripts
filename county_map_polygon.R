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
counties <- map_data(map = "county", region = "missouri")
states <- map_data("state", region = "missouri")

# This renames the column name "subregion" to "region" because geom_map
# has a bug (feature?) that it always uses the region column
counties <- counties %>%
  select(long,
         lat,
         group,
         order,
         region = subregion)

#we need to change the data in the NAME column to get rid of part of it
mo_pop <- mo_pop %>% 
  separate(NAME, "tmp_county", sep = ", Missouri") %>% 
  separate(tmp_county, "county", sep = " County") %>% 
  select(GEOID,
         "region" = county,
         variable,
         "population" = estimate
         )

# Now we need to get rid of case and stupid periods
# But! periods are special characters, and need to be "escaped"
# That's what the "\\." does
mo_pop$region <- mo_pop$region %>%  
  str_to_lower() %>% 
  str_replace_all("\\.", "")

# TODO add a join command and plot using geom_polygon in order to draw
# borders. Ugh.
mo_pop_join <- inner_join(counties, mo_pop, by = "region")

mo_base <- ggplot(data = states, 
                  mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", 
               fill = "gray")

mo_base + 
  geom_polygon(data = mo_pop_join, aes(fill = population), color = "grey") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  scale_fill_gradientn(
    colours = rev(rainbow(7)),
    breaks = c(200, 400, 1000, 10000, 100000, 1000000),
    trans = "log10",
    name = "Population by County"
  )

