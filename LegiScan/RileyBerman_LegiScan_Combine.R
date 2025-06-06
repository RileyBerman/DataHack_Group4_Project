#Contributor: Riley Berman 
#Description: Combined LegiScan California legislature data from 2009-2010 to 2025-2026 into one tibble as well as individual tibbles by year. 
#             Outputted results as .csv files in a created folder called "LegiScan_CA_Combined". 
#Methodology: Looped through each subfolder of the LegiScan_CA folder, combining
#             (a) all the documents .csv files into one tibble,
#             (b) all the history .csv files into one tibble,
#             (c) all the bills .csv files into one tibble,
#             (d) all the people .csv files into one tibble,
#             (e) all the votes, rollcalls, sponsors, and bills .csv files into one tibble.
#             (f) then outputted the results as .csv files in a created folder called "LegiScan_CA_Combined".
rm(list = ls())

library(tidyverse)
library(janitor)
#For writing to .csv files faster
library(data.table) 

#Get list of subfolders in LegiScan_CA folder (e.g., "LegiScan_CA/2009-2010_Regular_Session")
#Note: LegiScan California legislature data goes from 2009-2010 to 2025-2026
folder_files <- list.files(path = "LegiScan_CA", pattern = "\\_Regular_Session", full.names = TRUE)

#Create empty tibble to store all documents tibble
documents_combined <- tibble()

#Create empty tibble to store all history tibble
history_combined <- tibble()

#Create empty tibble to store all bills tibble
bills_combined <- tibble()

#Create empty tibble to store all people tibble
people_combined <- tibble()

#Create empty tibble to store all votes_rollcalls_sponsors_bills tibble
votes_rollcalls_sponsors_bills_combined <- tibble()

#Create folder to store finalized .csv files 
dir.create("LegiScan_CA_Combined", showWarnings = FALSE)

#Loop through each subfolder (e.g., "LegiScan_CA/2009-2010_Regular_Session")
for (folder in folder_files){
  #Extracts folder name without the "LegiScan_CA/" part (e.g, "2009-2010_Regular_Session") 
  folder_name <- tools::file_path_sans_ext(basename(folder))
  
  print(paste("Extracting all .csv files in", folder_name))
  
  #Get list of files in subfolder of the subfolder (e.g., from "2009-2010_Regular_Session/csv")
  #Note: all subfolders contain a subfolder called "csv" that contains the .csv files
  files <- list.files(path = paste0(folder, "/csv"), pattern = "\\.csv$", full.names = TRUE)
  
  #Loop through each file in the "csv" subfolder 
  for (file in files){
    #Extracts file path name without the .csv extension 
    file_name <- tools::file_path_sans_ext(basename(file))
    
    #Assigns to data frame the extracted name
    assign(file_name, read_csv(file))
  }
  
  #Note: For some reason, people tibble includes entities like "Business, Professions and Economic Development"
  #Note: This is completely fine as long as these values don't have commas since we will compress the sponsors of each bill
  #(which very rarely includes one of these entities) into one row separated by commas
  people <- people |> mutate(name = str_remove(name, ",")) |>
    mutate(folder = folder_name) 
  
  #Manipulating votes tibble to make it suitable for joining with other tibbles 
  #Result: each row of roll_call_id contains all voters' people_id's and names
  #Note: vote_desc: when vote == 1 ~ Yea, 2 ~ Nay, 3 ~ NV, 4 ~ Absent
  #Note: votes has 122 unique people_id's, matching the people tibble
  votes_condensed <- votes |> 
  #Really only need the name and people_id since we will combine with another .csv later on containing other information
    inner_join(people |> select(people_id, name), by = "people_id") |>
    #Grouping all voters' people_id's and names for each roll_call_id into one row depending on their vote ("Yea", "Nay", "NV", "Absent")
    mutate(
      people_id_yea = paste(people_id[vote_desc == "Yea"], collapse = ", "), 
      people_id_nay = paste(people_id[vote_desc == "Nay"], collapse = ", "),
      people_id_nv = paste(people_id[vote_desc == "NV"], collapse = ", "), 
      people_id_absent = paste(name[vote_desc == "Absent"], collapse = ", "), 
      people_name_yea = paste(name[vote_desc == "Yea"], collapse = ", "), 
      people_name_nay = paste(name[vote_desc == "Nay"], collapse = ", "),
      people_name_nv = paste(name[vote_desc == "NV"], collapse = ", "), 
      people_name_absent = paste(name[vote_desc == "Absent"], collapse = ", "), 
      .by = roll_call_id) |>
    #Get rid of roll_call_id repeats
    distinct(roll_call_id, .keep_all = TRUE) |>
    #Fill blank values with NA
    mutate(across(starts_with(c("people_id_", "people_name_")), ~na_if(.x, ""))) |>
    select(-c(people_id, vote_desc, vote, name)) 
    
  #Joining rollcalls tibble with votes_condensed tibble
  #Note: the roll calls that have a total less than 80 are likely committee votes (check the history tibble for confirmation)
  votes_rollcalls <- rollcalls |>
    #Renaming date and description columns since bill tibble has similar columns with different meanings
    rename(roll_call_date = date, 
           roll_call_description = description) |>
    inner_join(votes_condensed, by = "roll_call_id") 
    
  #Manipulating sponsored tibble to make it suitable for joining with other tibbles
  #Result: each row of bill_id contains all sponsors' people_id's and names 
  #Note: sponsors has 122 unique people_id's, matching the people tibble
  sponsors_condensed <- sponsors |>
    inner_join(people |> select(people_id, name, party), by = "people_id") |>
    #Grouping all sponsors' people_id and names and party for each bill_id into one row
    mutate(people_id_sponsors = paste(people_id, collapse = ", "), 
           people_name_sponsors = paste(name, collapse = ", "),
           party_sponsors = paste(party, collapse = ", "),
           .by = bill_id) |>
    #Get rid of bill_id repeats
    distinct(bill_id, .keep_all = TRUE) |>
    select(-c(people_id, position, name, party)) 
    
  #Joining votes_rollcalls tibble with sponsors_condensed tibble
  votes_rollcalls_sponsors <- votes_rollcalls |>
    inner_join(sponsors_condensed, by = "bill_id") 
  
  votes_rollcalls_sponsors_bills <- votes_rollcalls_sponsors |>
    inner_join(bills, by = "bill_id") |>
    mutate(folder = folder_name) 
  
  votes_rollcalls_sponsors_bills_combined <- bind_rows(votes_rollcalls_sponsors_bills, votes_rollcalls_sponsors_bills_combined)
  
  #Combining all documents tibbles into one tibble with sponsor data from sponsors_condensed
  documents_combined_initial <- documents |> inner_join(sponsors_condensed, by = "bill_id") |>
    mutate(folder = folder_name)
  
  documents_combined <- bind_rows(documents_combined_initial, documents_combined) 
  
  #Combining all history tibbles into one tibble with sponsor data from sponsors_condensed
  history_combined_initial <- history |> inner_join(sponsors_condensed, by = "bill_id") |>
    mutate(folder = folder_name)
  
  history_combined <- bind_rows(history_combined_initial, history_combined)

  #Combining all bills tibbles into one tibble with sponsor data from sponsors_condensed
  bills_combined_initial <- bills |> inner_join(sponsors_condensed, by = "bill_id") |>
    mutate(folder = folder_name)
    
  bills_combined <- bind_rows(bills_combined_initial, bills_combined)
  
  #Combining all people tibbles into one tibble and adding session name
  people_combined_initial <- people |> mutate(folder = folder_name)
  
  people_combined <- bind_rows(people_combined_initial, people_combined) 
  
  print(paste("Successfully combined all .csv files in ", folder_name))
}

#Write the final votes_rollcalls_sponsors_bills to a .csv file in the created folder
fwrite(votes_rollcalls_sponsors_bills_combined, paste0("LegiScan_CA_Combined/votes_rollcalls_sponsors_bills_combined.csv"))

#Write the final documents_combined to a .csv file in the created folder
fwrite(documents_combined, paste0("LegiScan_CA_Combined/documents_combined.csv"))

#Write the final bills_combined to a .csv file in the created folder
fwrite(bills_combined, paste0("LegiScan_CA_Combined/bills_combined.csv"))

#Write the final people_combined to a .csv file in the created folder
fwrite(people_combined, paste0("LegiScan_CA_Combined/people_combined.csv"))

#Write the final history_combined to a .csv file in the created folder
fwrite(history_combined, paste0("LegiScan_CA_Combined/history_combined.csv"))
