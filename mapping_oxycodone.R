library(tidycensus) 
library(tidyverse)

census_api_key("b851144a625a1ff4d8e04253547dde222768dd39", install = "TRUE")

# Mapping Oxycontin Prescriptions

# Read in the 2016 Medicaire Data
all_data <- read_csv("State_Drug_Utilization_Data_2016.csv")
# Make a simple data frame that just has the total OXYCODONE prescriptions
# This misses a number of prescriptions!
all_data <- all_data %>% rename_all(make.names)

oxy_state <- all_data %>% 
  select(State, Product.Name, Number.of.Prescriptions, Suppression.Used) %>% 
  filter(Product.Name == "OXYCODONE", Suppression.Used == "false") %>% 
  group_by(State) %>% summarise(oxy_num = sum(Number.of.Prescriptions))
# The data is in two letter format, convert it to name
# this converts DC and XX to NA
colnames(oxy_state) <- tolower(colnames(oxy_state))
oxy_state$State <-  state.name[match(oxy_state$State, state.abb)]
state_2010 <- get_decennial(geography = "state", variables = "P0010001", year = "2010")
oxy_state <- left_join(oxy_state, states, by = 'state')
oxy_state$name <-  tolower(oxy_state$name)
oxy_state$population <- as.numeric(oxy_state$population)
oxy_state <- mutate(oxy_state, norm_oxy = oxy_num / population)
#oxy_state <- oxy_state %>% filter(!is.na(State))

# This tells ggplot how to draw our map later
mapping <- map_data("state")
# initial plot object, filled with numbr of oxy prescriptions
plot <- ggplot(oxy_state, aes(fill = norm_oxy))
# the map = mapping makes sure ggplot knows what to use
plot + geom_map(aes(map_id = name), map = mapping) + 
  expand_limits(x=map$long, y=map$lat) + 
  coord_fixed(1.3) + scale_fill_gradient(high = "red", low = "blue", 
                                         name = "Number of Prescriptions") +
  theme(legend.position = "bottom")
  

