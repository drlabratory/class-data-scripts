library(tidyverse)

# Download all the Missouri (or whatever) Medicaid State Drug Utilization data
# https://www.medicaid.gov/medicaid/prescription-drugs/state-drug-utilization-data/index.html

# this saves all the csv files in the current directory as a list
files <- list.files(pattern="csv")

# this merges all the csv files in your list into one big dataframe
# PAY CLOSE ATTENTION TO THE ERROR MESSAGES
df <- map_df(files, read_csv)

# rename all the column names to get rid of spaces
df <- df %>% 
  rename_all(make.names)

# Filtering the data to pick out useful columns
filtered_data <-df %>% 
  filter(Suppression.Used == "false") %>% 
  select(Year,
         Quarter,
         Product.Name,
         Units.Reimbursed,
         Number.of.Prescriptions,
         Medicaid.Amount.Reimbursed,
         Non.Medicaid.Amount.Reimbursed,
         Total.Amount.Reimbursed)

write_csv(filtered_data, "Missouri_Data_1991_2017.csv")

# an example way to summarise and filter by product name to do exploratory graphing
year_data <-filtered_data %>% 
  group_by(Product.Name, Year) %>% 
  summarise(Total.Prescriptions = sum(Number.of.Prescriptions), 
            Medicaid = sum(Medicaid.Amount.Reimbursed), 
            Non.Medicaid = sum(Non.Medicaid.Amount.Reimbursed), 
            Total.Reimbursed = sum(Total.Amount.Reimbursed))

write_csv(year_data, "Misouri_Year_Totals.csv")
