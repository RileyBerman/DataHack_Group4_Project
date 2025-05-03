#Contributor: Min Kim
#Description: Created a table of the bills from CA 2025-2026 Regular Session containing
#the bill numbers, bill titles, sponsors' names, sponsors' party affiliation, sponsors' roles (assembly or senate),
#and links to bill info
#Methodology: Downloaded the dataset from Legiscan and merged the tables using common categories


rm(list=ls())

install.packages("tidyverse")
library("tidyverse")

#Data from CA 2025-2026 Regular Session
bill_info <- read_csv("Downloads/CA 2/2025-2026_Regular_Session/csv/bills.csv")
spons_info <- read_csv("Downloads/CA 2/2025-2026_Regular_Session/csv/sponsors.csv")
ppl_info <- read_csv("Downloads/CA 2/2025-2026_Regular_Session/csv/people.csv")

merged_data <- left_join(spons_info, ppl_info, bill_info, by = "people_id") 
#merged data of the three tables above. Lists the bill number, bill sponsors, sponsors' affiliation party, etc. 
merged_df <- left_join(merged_data, bill_info, by = "bill_id")

#final merged info with bill number, status, title, sponsors' name, party affiliations
Merged_info <- merged_df %>%  select(bill_number, status_desc, title, name, party, role, url, state_link)

#changing the name of the column 
names(Merged_info)[names(Merged_info) == "name"] <- "sponsors"

#We would have to analyze the sponsors' party affiliations for each bill to identify if it's bipartisan?
