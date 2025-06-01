#Contributor: Riley Berman 
#Description: Manipulates bills_SB_AB_bipartisanship from RileyBerman_LegiScan_Analysis.R to obtain bipartisan stats for each politician.
#             Output: bills_statistics_final
#Methodology: Tidyverse manipulating and graphing with ggplot2.

rm(list = ls())

library(tidyverse)
library(janitor)
library(ggplot2)
library(hrbrthemes)
library(tidylog)

env <- new.env()
source("RileyBerman_LegiScan_Analysis.R", local = env)

bills_SB_AB_bipartisanship <- env$bills_SB_AB_bipartisanship
#Extract the desired tibble 
#Remove some unnecessary columns 
bills_SB_AB_bipartisanship <- bills_SB_AB_bipartisanship |> 
  select(bill_id, real_status_desc, session_year, party_sponsors, people_name_sponsors, 
         bipartisan1, bipartisan2, bipartisan3, weighted_bipartisan2, weighted_bipartisan3) |>
  mutate(bills_per_session = n(), .by = session_year)

bills_wider <- bills_SB_AB_bipartisanship |> separate_rows(people_name_sponsors, party_sponsors, sep = ",") |>
  #Extra spacing was messing up grouping
  mutate(people_name_sponsors = str_trim(people_name_sponsors), 
         party_sponsors = str_trim(party_sponsors))
                                   
#Getting individual statistics for each politician
#Note: 369 politicians total ~bills_wider$people_name_sponsors |> unique() |> length()
#Note: Reminder that we don't know if a politician is a sponsor or cosponsor, so we are counting both as sponsors
bills_statistics <- bills_wider |>
  summarize(bills_sponsored = n(), 
            bipartisan1_num = sum(bipartisan1, na.rm = TRUE),
            bipartisan2_num = sum(bipartisan2, na.rm = TRUE),
            weighted_bipartisan2_num = sum(weighted_bipartisan2, na.rm = TRUE),
            #Taking the average score of bipartisan3 of the bills that they are sponsoring
            bipartisan3_num = mean(bipartisan3, na.rm = TRUE),
            weighted_bipartisan3_num = mean(weighted_bipartisan3, na.rm = TRUE),
            introduced_num = sum(real_status_desc == "Introduced", na.rm = TRUE),
            passed_num = sum(real_status_desc == "Passed", na.rm = TRUE),
            failed_num = sum(real_status_desc == "Failed", na.rm = TRUE),
            vetoed_num = sum(real_status_desc == "Vetoed", na.rm = TRUE),
            engrossed_num = sum(real_status_desc == "Engrossed", na.rm = TRUE),
            enrolled_num = sum(real_status_desc == "Enrolled", na.rm = TRUE), 
            bills_per_session = first(bills_per_session),
            #Parties that have NA value are the entities like "Business, Professions and Economic Development"
            party = first(party_sponsors),
            .by = c(people_name_sponsors, session_year)) 

#Create metrics to evaluate a politician's bipartisanship rate
bills_statistics_updated <- bills_statistics |>
  mutate(bipartisan_rate1 = bipartisan1_num/bills_sponsored,
         bipartisan_rate2 = bipartisan2_num/bills_sponsored,
         weighted_bipartisan_rate2 = weighted_bipartisan2_num/bills_sponsored,
         bipartisan_rate3 = bipartisan3_num, 
         weighted_bipartisan_rate3 = weighted_bipartisan3_num) |> 
  #Removing NA party politicians 
  #Note: 8 rows removed
  filter(party != "NA")

#Since we are combining this with CA_Assembly_Final.csv and CA_State_Final.csv, we need to aggregate the values for each politican (no section_year)
bills_statistics_final <- bills_statistics_updated |>
  summarize(bipartisan_rate1 = mean(bipartisan_rate1, na.rm = TRUE), 
            bipartisan_rate2 = mean(bipartisan_rate2, na.rm = TRUE),
            weighted_bipartisan_rate2 = mean(weighted_bipartisan_rate2, na.rm = TRUE),
            bipartisan_rate3 = mean(bipartisan_rate3, na.rm = TRUE),
            weighted_bipartisan_rate3 = mean(weighted_bipartisan_rate3, na.rm = TRUE), 
            bills_sponsored = sum(bills_sponsored, na.rm = TRUE),
            introduced_num = sum(introduced_num, na.rm = TRUE),
            passed_num = sum(passed_num, na.rm = TRUE),
            failed_num = sum(failed_num, na.rm = TRUE),
            vetoed_num = sum(vetoed_num, na.rm = TRUE),
            engrossed_num = sum(engrossed_num, na.rm = TRUE),
            enrolled_num = sum(enrolled_num, na.rm = TRUE),
            total_bills = sum(bills_per_session, na.rm = TRUE),
            .by = people_name_sponsors)

#Is Bipartisanship Good? 
#(Source: https://batten.virginia.edu/bipartisanship-secret-sauce-effective-lawmaking-despite-rising-polarization-congress)

#bipartisan1 by Party
#Note: Lots of Republican politicians have higher counts of bipartisanship1 (easier to collaborate)
bills_bipartisan1 <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan1_num, color = party)) +
  geom_jitter(width = 0.2, alpha = 0.8) +
  labs(title = "Bipartisan1 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Number of Bipartisan1 Bills") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#Boxplot
#For labeling outliers
#(Source: https://stackoverflow.com/questions/33524669/labeling-outliers-of-boxplots-in-r)
bills_bipartisan1_boxplot <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan1_num, color = party)) +
  geom_boxplot() + 
  labs(title = "Bipartisan1 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Number of Bipartisan1 Bills") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))
  
#bipartisan2 by Party
bills_bipartisan2 <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan2_num, color = party)) + 
  geom_jitter() +
  labs(title = "Bipartisan2 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Number of Bipartisan2 Bills") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#Boxplot
bills_bipartisan2_boxplot <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan2_num, color = party)) +
  geom_boxplot() + 
  labs(title = "Bipartisan2 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Number of Bipartisan2 Bills") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#Weighted
#Note: Parties are more evenly distributed now 
bills_weighted_bipartisan2 <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = weighted_bipartisan2_num, color = party)) + 
  geom_jitter() +
  labs(title = "Weighted Bipartisan2 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Average Weighted Bipartisan2 Score") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#bipartisan3 by Party
bills_bipartisan3 <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan3_num, color = party)) + 
  geom_jitter() +
  labs(title = "Bipartisan3 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Average Bipartisan3 Score") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#Boxplot
bills_bipartisan3_boxplot <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = bipartisan3_num, color = party)) +
  geom_boxplot() + 
  labs(title = "Bipartisan3 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Average Bipartisan3 Score") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))

#Weighted
weighted_bills_bipartisan3 <- bills_statistics_updated |>
  ggplot(aes(x = session_year, y = weighted_bipartisan3_num, color = party)) + 
  geom_jitter() +
  labs(title = "Weighted Bipartisan3 Bills by Politician and Session Year",
       x = "Session Year",
       y = "Average Weighted Bipartisan3 Score") +
  theme_ipsum() + 
  scale_color_manual(values = c("D" = "#00BFC4", "R" = "#F8766D"))
