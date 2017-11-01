library(tidyverse)

# filtering the package.txt from the NDC

package <- read_tsv("ndctext/package.txt")

# someone put stars where the number of packages should go on 5 rows
# (╯°□°)╯︵ ┻━┻

package$PACKAGEDESCRIPTION <- 
  lapply(package$PACKAGEDESCRIPTION, gsub, pattern = "\\*  ", replacement = "")

# gsub turns character into a list? coerce back into character

package$PACKAGEDESCRIPTION <- as.character(package$PACKAGEDESCRIPTION)

# Typical Entry looks like:
# 30 CAPSULE in 1 BOTTLE (0002-3227-30)
# or
# 60 CAPSULE, DELAYED RELEASE in 1 BOTTLE (0002-3...

tmp_package <- package %>% 
  separate(PACKAGEDESCRIPTION, into = paste0("PACKAGEDESC_", c(1:2)), sep = " in ", remove = FALSE)

tmp_package <- tmp_package %>% separate(PACKAGEDESC_1, c("PACKAGENUMBER", "PACKAGEUNIT"), sep = " ")

# coerce packagenumber into a number
tmp_package$PACKAGENUMBER <- as.numeric(tmp_package$PACKAGENUMBER)

# get rid of trailing (?) commas in entries, then coerce back into character from list
tmp_package$PACKAGEUNIT <- 
  lapply(tmp_package$PACKAGEUNIT, gsub, pattern = ",", replacement = "")
tmp_package$PACKAGEUNIT <- as.character(tmp_package$PACKAGEUNIT)

# State Medicaid Utilization Data doesn't have dashes
tmp_package <- tmp_package %>% 
  separate(NDCPACKAGECODE, c("NDC1", "NDC2", "NDC3")) %>% 
  unite(NDCPACKAGECODE, NDC1, NDC2, NDC3, sep = "")

package <- tmp_package %>% 
  select(PRODUCTID, PRODUCTNDC, NDCPACKAGECODE, PACKAGEDESCRIPTION, PACKAGENUMBER, PACKAGEUNIT)

write_csv(package, "package-txt.csv")