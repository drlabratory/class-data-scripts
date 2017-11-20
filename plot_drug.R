require(tidycensus) 
require(tidyverse)
require(maps)
require(scales)
require(rlang)

# you need to set this yourself, instructions are in the tidycensus
# documentation, or just go to http://api.census.gov/data/key_signup.html
# census_api_key("INSERTHERE")


# This function takes a dataframe made from Medicaid State Data, and returns
# the total number of prescriptions by state. It takes the name of the dataframe
# the string you want to search, and optionally the Quarter
drug_grep <- function(dat, drugname, QuarterNum = c(1,2,3,4), norm = FALSE) {
  dat <- dat %>% rename_all(make.names)
  # removes spaces from column names to X.X format instead of "X X"
  dat$Number.of.Prescriptions <- as.numeric(dat$Number.of.Prescriptions)
  # sometimes it comes in as a character
  # Select what we need, filter on the string, then summarize number of
  # Prescriptions
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

# placeholder until I make this more general. Take two data frames
# then plot them by their state labels
compare_drug <- function(all_dat, search1, search2, joinby = "State", norm = FALSE) { 
  dat1 <- drug_grep(all_dat, search1)
  dat2 <- drug_grep(all_dat, search2)
  drug1 <- colnames(dat1)[2]
  drug2 <- colnames(dat2)[2]
  both <- left_join(dat1, dat2, by = joinby)
  both <- both %>% 
    filter(State != "XX")
  if (norm) {
    state_data <- get_decennial(geography = "state", 
                                variables = "P0010001", 
                                year = "2010")
    state_data <- state_data %>% 
      select(State = NAME, Population = value)
    state_data$State <- state.abb[match(state_data$State, state.name)]
    bothnorm <- left_join(both, state_data, by = joinby)
    bothnorm <- bothnorm %>% 
      filter(State != "DC")
    edrug1 <- sym(drug1)
    edrug2 <- sym(drug2)
    bothnorm <- mutate(bothnorm, 
              norm1 = (!! edrug1) / Population, 
              norm2 = (!! edrug2) / Population)
    bothnorm <- select(bothnorm, State, norm1, norm2)
    colnames(bothnorm)[match("norm1",colnames(bothnorm))] <- drug1
    colnames(bothnorm)[match("norm2",colnames(bothnorm))] <- drug2
  }
  plt <- ggplot(bothnorm, 
                aes_string(x = drug1, 
                           y = drug2, 
                           label = joinby))
  plt
  return(plt)
}

plot_drug <- function(dat, drugname, norm = TRUE, limits = NULL, QuarterNum = c(1,2,3,4)) {
  # take a data frame, search for a drug, and plot it on a map of the US
  # either raw or normalized by state population, the limits argument feeds into
  # ggplot and allows you to plot multiple plots using the same scale limit
  dat <- dat %>% rename_all(make.names)
  dat$Number.of.Prescriptions <- as.numeric(dat$Number.of.Prescriptions)
  data_fuzzy <- dat %>% 
    select(State, Product.Name, Number.of.Prescriptions, Suppression.Used, Quarter) %>% 
    filter(grepl(pattern = drugname,x = Product.Name, ignore.case = TRUE), 
           Suppression.Used == "false", Quarter %in% QuarterNum)  %>% 
    group_by(State) %>% summarise(drug_num = sum(Number.of.Prescriptions))
  # This changes the two letter state code to it's full name
  # This is needed to plot later
  data_fuzzy$State <-  state.name[match(data_fuzzy$State, state.abb)]
  # This grabs state level population data from 2010 census
  state_data <- get_decennial(geography = "state", variables = "P0010001", year = "2010")
  # filtering the data for columns we care about
  # The 'State = NAME' nomenclature renames the NAME column to State
  state_data <- state_data %>% 
    select(
      State = NAME,
      Population = value
    )
  # This joins the Medicaid data to the census data
  # Missing data becomes 'NA' so DC and XX disappear
  data_fuzzy <- left_join(data_fuzzy, state_data, by = 'State')
  data_fuzzy$State <-  tolower(data_fuzzy$State) # needed to plot
  data_fuzzy <- data_fuzzy %>% filter(!is.na(State))
  # this is in case it comes in as char
  data_fuzzy$Population <- as.numeric(data_fuzzy$Population)
  # make a new column normalized by state population
  data_fuzzy <- mutate(data_fuzzy, drug_norm = drug_num / Population)
  # This 
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

