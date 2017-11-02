require(tidycensus) 
require(tidyverse)
require(maps)

census_api_key("b851144a625a1ff4d8e04253547dde222768dd39", install = "TRUE")

# Mapping Oxycontin Prescriptions

plot_drug <- function(dat, drugname, norm = TRUE) {
  # take a data frame, search for a drug, and plot it, 
  # either raw or normalized 
  dat <- dat %>% rename_all(make.names)
  data_fuzzy <- dat %>% 
    select(State, Product.Name, Number.of.Prescriptions, Suppression.Used) %>% 
    filter(grepl(pattern = drugname,x = Product.Name, ignore.case = TRUE), Suppression.Used == "false") %>% 
    group_by(State) %>% summarise(drug_num = sum(Number.of.Prescriptions))
  colnames(data_fuzzy) <- tolower(colnames(data_fuzzy))
  data_fuzzy$state <-  state.name[match(data_fuzzy$state, state.abb)]
  colnames(data_fuzzy)[1] <- "NAME"
  state_2010 <- get_decennial(geography = "state", variables = "P0010001", year = "2010")
  data_fuzzy <- left_join(data_fuzzy, state_2010, by = 'NAME')
  data_fuzzy$NAME <-  tolower(data_fuzzy$NAME)
  data_fuzzy <- data_fuzzy %>% filter(!is.na(NAME))
  data_fuzzy$value <- as.numeric(data_fuzzy$value)
  data_fuzzy <- mutate(data_fuzzy, drug_norm = drug_num / value)
  mapping <- map_data("state")
  if (norm) {
    plot <- ggplot(data_fuzzy, aes(fill = drug_norm))
    # the map = mapping makes sure ggplot knows what to use
    plot + geom_map(aes(map_id = NAME), map = mapping) + 
      expand_limits(x=mapping$long, y=mapping$lat) + 
      coord_fixed(1.3) + 
      scale_fill_gradient(
        high = "red", 
        low = "white", 
        name = paste("Ratio: Scripts / Pop", drugname)
        ) +
      theme(legend.position = "bottom")
  } else {
    plot <- ggplot(data_fuzzy, aes(fill = drug_num))
    # the map = mapping makes sure ggplot knows what to use
    plot + geom_map(aes(map_id = NAME), map = mapping) + 
      expand_limits(x=mapping$long, y=mapping$lat) + 
      coord_fixed(1.3) + 
      scale_fill_gradient(
        high = "red", 
        low = "white", 
        name = paste("Total Scripts matching", drugname)
        ) +
      theme(legend.position = "bottom")
  }
  
}

