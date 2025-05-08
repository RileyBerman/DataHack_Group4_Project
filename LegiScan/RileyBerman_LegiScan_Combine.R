#Contributor: Riley Berman 
#Description: Combined LegiScan California legislature data from 2009-2010 to 2025-2026 into one tibble as well as individual tibbles by year. 
#             Outputted results as .csv files in a created folder called "LegiScan_CA_Combined". 
#Methodology: Looped through each subfolder of the LegiScan_CA folder, extracted and combined all .csv files in each subfolder, and then
#             (a) wrote each subfolder tibble to a .csv file in the created folder, and 
#             (b) combined all subfolder tibbles into one final tibble and wrote it to a .csv file in the created folder.

rm(list = ls())

library(tidyverse)
library(janitor)
#For writing to .csv files faster
library(data.table) 

#Get list of subfolders in LegiScan_CA folder (e.g., "LegiScan_CA/2009-2010_Regular_Session")
#Note: LegiScan California legislature data goes from 2009-2010 to 2025-2026
folder_files <- list.files(path = "LegiScan_CA", pattern = "\\_Regular_Session", full.names = TRUE)

#Create empty final tibble to store finalized data
LegiScan_Final <- tibble()

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
    inner_join(people |> select(people_id, name), by = "people_id") |>
    #Grouping all sponsors' people_id and names for each bill_id into one row
    mutate(people_id_sponsors = paste(people_id, collapse = ", "), 
           people_name_sponsors = paste(name, collapse = ", "),
           .by = bill_id) |>
    #Get rid of bill_id repeats
    distinct(bill_id, .keep_all = TRUE) |>
    select(-c(people_id, position, name)) 
    
  #Joining votes_rollcalls tibble with sponsors_condensed tibble
  votes_rollcalls_sponsors <- votes_rollcalls |>
    inner_join(sponsors_condensed, by = "bill_id") 
    
  #Manipulating documents tibble to make it suitable for joining with other tibbles
  #Result: each row of bill_id contains all the bill's document_id's, descriptions, urls, and state_links
  #Note: bill documents can be amended, so there can be multiple documents for each bill (i.e., multiple bill_id's)
  documents_condensed <- documents |>
    #Don't really need document_type or document_mime columns 
    select(bill_id, document_id, document_desc, url, state_link) |>
    #Grouping all bill's document_id's, descriptions, urls, and state_links for each bill_id into one row
    mutate(document_desc = paste(document_desc, collapse = ", "), 
           url = paste(url, collapse = ", "),
           state_link = paste(state_link, collapse = ", "), 
           .by = bill_id) |>
    distinct(bill_id, .keep_all = TRUE) |>
    #Rename url and state_link columns since bill tibble has similar columns with different meanings
    rename(document_url = url, 
           document_state_link = state_link) 
    
  #Manipulating history tibble to make it suitable for joining with other tibbles
  #Result: each row of bill_id contains all the bill's action dates, sequences, and actions
  #Note: there are multiple stages in a bill, so there can be multiple actions for each bill (i.e., multiple bill_id's)
  history_condensed <- history |>
    select(-chamber) |>
    #Grouping all bill's action dates, sequences, and actions for each bill_id into one row
    mutate(date = paste(date, collapse = ", "), 
           sequence = paste(sequence, collapse = ", "),
           action = paste(action, collapse = ", "), 
           .by = bill_id) |>
    distinct(bill_id, .keep_all = TRUE)  |>
    #Rename date column since other tibbles have similar columns with different meanings
    rename(action_date = date)
    
  #Joining bills tibble with documents_condensed and history_condensed tibbles
  bills_wider <- bills |> 
    #Rename status_date, description, url, and state_link columns since other tibbles have similar columns with different meanings
    rename(bill_status_date = status_date, 
           bill_description = description, 
           bill_url = url, 
           bill_state_link = state_link) |>
    inner_join(documents_condensed, by = "bill_id") |>
    inner_join(history_condensed, by = "bill_id") 
    
  #Final tibble combining all .csv files for the subfolder
  #Joining votes_rollcalls_sponsors tibble with bills_wider tibble
  votes_rollcalls_sponsors_bills <- votes_rollcalls_sponsors |>
    inner_join(bills_wider, by = "bill_id") |> mutate(folder_id = folder_name)
    
  print(paste("Successfully combined all .csv files in ", folder_name))
    
  #Write every individual final subfolder tibble to .csv file in the created folder
  #Using fwrite from data.table instead of write_csv for faster reading
  fwrite(votes_rollcalls_sponsors_bills, paste0("LegiScan_CA_Combined/", folder_name, ".csv"))
    
  #Final tibble combining all subfolder tibbles
  #Append each subfolder tibble to the final tibble
  LegiScan_Final <- bind_rows(votes_rollcalls_sponsors_bills, LegiScan_Final)
}

#Write the final tibble to a .csv file in the created folder
fwrite(LegiScan_Final, paste0("LegiScan_CA_Combined/LegiScan_Final.csv"))
