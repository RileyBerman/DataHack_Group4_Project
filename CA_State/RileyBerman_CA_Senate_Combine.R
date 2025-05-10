#Contributor: Riley Berman 
#Description: Created a tibble containing all the names of the members of the California State Senate (40 members)
#             along with their party affiliation, district, date they assumed office, committees they are on, and their Twitter handles. 
#             Outputted results as a .csv file in a created folder called "CA_State_Combined". 
#Methodology: Web scraped from a few webpages and compiled the information into one big tibble. 
#             Wrote the tibble to a .csv in the created folder.

rm(list = ls())

library(tidyverse)
library(rvest)
library(janitor)
library(fuzzyjoin)

link <- "https://ballotpedia.org/California_State_Senate"

page <- read_html(link)

CA_State <- page |> html_table() |> 
  pluck(4) |> as_tibble() |> clean_names() |> 
  #Getting district numbers for matching 
  extract(col = office, into = "district", regex = "(\\d{1,})") |>
  #Changing 1-9 districts to 01, 02, for matching
  #Changing "Democratic" to "Democrat" for matching
  #Changing "Ben Allen" to "Benjamin Allen" for matching
  mutate(district = str_pad(district, width = 2, pad = "0"), 
         party = ifelse(party == "Democratic", "Democrat", party), 
         name = ifelse(str_detect(name, "Ben"), "Benjamin Allen", name))

link <- "https://www.senate.ca.gov/committees"

committee_hyperlinks <- read_html(link) |>
  html_elements(".field-content a") |>
  html_attr("href") |> as.character()

committee_names <- read_html(link) |>
  html_elements(".field-content a") |>
  html_text2() |> as.character()

committee_data <- tibble(
  name = committee_names, 
  url = committee_hyperlinks)

#Agriculture to Transportation (other links have different ways to access members)
standing_committees <- committee_data[1:23, ]

#Getting the members of each committee
members_committes <- tibble()
for (link in standing_committees$url){
  full_link <- paste0(link, "members")
  page <- read_html(full_link)
  members <- page |> html_elements(".member-info--wrapper") |> 
    html_text2() |> as.character()
  
  committee_data <- tibble(committee_url = link, 
                           members = paste0(members, collapse = ", ")) 
  
  members_committes <- bind_rows(members_committes, committee_data)
}

#Cleaning up the members_committees tibble
members_committes_full <- members_committes |> 
  inner_join(standing_committees, by = c("committee_url" = "url")) |>
  separate_rows(members, sep = ",") |>
  #changing the names of the members to match the names in CA_State 
  mutate(members = str_remove_all(members, "\\(.*?\\)") |> str_trim(), 
         members = ifelse(str_detect(members, "Akilah"), "Akilah Weber", members), 
         members = ifelse(str_detect(members, "Choi"), "Steven Choi", members)) |> 
  summarise(committees = paste(name, collapse = ", "), .by = members) 

#Use fuzzy_join since a few of the members names are slightly different in the tibbles (e.g., Tom Umberg vs Thomas Umberg)
#Names are close enough so that they will match up with their rough counterpart within 3 character distance, not another's name
CA_State_Final <- members_committes_full |> stringdist_left_join(CA_State, by = c("members" = "name") , max_dist = 3) |>
  select(-name) |> mutate(twitter = NA)

#Create folder to store finalized .csv file 
dir.create("CA_State_Combined", showWarnings = FALSE)

#Exporting the final data set as .csv
#Will input Twitter handles manually 
write_csv(CA_State_Final, paste0("CA_State_Combined/CA_State_Final.csv"))
                                               
 