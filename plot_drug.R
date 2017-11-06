require(tidycensus) 
require(tidyverse)
require(maps)
require(scales)

# you need to set this yourself, instructions are in the tidycensus
# documentation, or just go to http://api.census.gov/data/key_signup.html
# census_api_key("INSERTHERE")

# Mapping Oxycontin Prescriptions

drug_grep <- function(dat, drugname, QuarterNum = c(1,2,3,4)) {
  dat <- dat %>% rename_all(make.names)
  dat$Number.of.Prescriptions <- as.numeric(dat$Number.of.Prescriptions)
  data_fuzzy <- dat %>% 
    select(State, 
           Product.Name, 
           Number.of.Prescriptions, 
           Suppression.Used, 
           Quarter) %>% 
    filter(grepl(pattern = drugname,x = Product.Name, ignore.case = TRUE), 
           Suppression.Used == "false", 
           Quarter %in% QuarterNum)  %>% 
    group_by(State) %>% 
    summarise(drug_num = sum(Number.of.Prescriptions)) %>% 
    # This renames the column name to whatever the search string is
    rename(!!drugname:=drug_num)
  return(data_fuzzy)
}

plot_drug <- function(dat, drugname, norm = TRUE, limits = NULL, QuarterNum = c(1,2,3,4)) {
  # take a data frame, search for a drug, and plot it, 
  # either raw or normalized 
  dat <- dat %>% rename_all(make.names)
  dat$Number.of.Prescriptions <- as.numeric(dat$Number.of.Prescriptions)
  data_fuzzy <- dat %>% 
    select(State, Product.Name, Number.of.Prescriptions, Suppression.Used, Quarter) %>% 
    filter(grepl(pattern = drugname,x = Product.Name, ignore.case = TRUE), 
           Suppression.Used == "false", Quarter %in% QuarterNum)  %>% 
    group_by(State) %>% summarise(drug_num = sum(Number.of.Prescriptions))
  data_fuzzy$State <-  state.name[match(data_fuzzy$State, state.abb)]
  state_2010 <- get_decennial(geography = "state", variables = "P0010001", year = "2010")
  state_2010 <- state_2010 %>% 
    select(
      State = NAME,
      Population = value
    )
  data_fuzzy <- left_join(data_fuzzy, state_2010, by = 'State')
  data_fuzzy$State <-  tolower(data_fuzzy$State)
  data_fuzzy <- data_fuzzy %>% filter(!is.na(State))
  data_fuzzy$Population <- as.numeric(data_fuzzy$Population)
  data_fuzzy <- mutate(data_fuzzy, drug_norm = drug_num / Population)
  mapping <- map_data("state")
  if (norm) {
    plot <- ggplot(data_fuzzy, aes(fill = drug_norm))
    # the map = mapping makes sure ggplot knows what to use
    plot + geom_map(aes(map_id = State), map = mapping) + 
      expand_limits(x=mapping$long, y=mapping$lat) + 
      coord_fixed(1.3) + 
      scale_fill_gradient(
        limits = limits,
        high = muted("red"), 
        low = "white",
        name = paste("Ratio: Scripts / Pop", drugname)
        ) +
      theme(legend.position = "bottom")
  } else {
    plot <- ggplot(data_fuzzy, aes(fill = drug_num))
    # the map = mapping makes sure ggplot knows what to use
    plot + geom_map(aes(map_id = State), map = mapping) + 
      expand_limits(x=mapping$long, y=mapping$lat) + 
      coord_fixed(1.3) + 
      scale_fill_gradient(
        limits = limits,
        high = "red", 
        low = "white", 
        name = paste("Total Scripts matching", drugname)
        ) +
      theme(legend.position = "bottom")
  }
  
}

