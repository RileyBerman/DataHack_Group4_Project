6#Contributor: Riley Berman 
#Description: Created a tibble containing the party platform statements of parties (mostly Democratic and Republican) from 1840 to 2024, 
#             along with the year, party, electoral votes, and number of words in the statement.
#             Outputted results as a .csv file in a created folder called "Party_Platform_Combined". 
#Methodology: Iteratively web scraped from many (106) webpages and compiled the information into one big tibble. 
#             Wrote the tibble to a .csv in the created folder.

rm(list = ls())

library(pacman)

p_load(tidyverse, lubridate, janitor, RSelenium, rvest, netstat, wdman)

#Setting up webscraping
rD <- RSelenium::rsDriver(
  port = free_port(),
  browser = c("firefox"),
  version = "latest",
  chromever = NULL
)

remDr <- rD[["client"]]

initial_address <- "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/party-platforms-and-nominating-conventions-3"

#Go to webpage 
remDr$navigate(initial_address)

#Getting table of party platforms and their information
#Note: For 2020, the executive committee of the Republican National Committee chose not to adopt a new platform in 2020, leaving the 2016 platform in place.
#They issued a resolution regarding this decision.
Party_Platform_Final <- remDr$getPageSource()[[1]] |>
  read_html() |>
  html_table() |>
  pluck(1) |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  #Creating text column for extraction
  mutate(text = NA, ) |>
  #Get rid of last unnecessary row and download column
  slice(-n()) |>
  select(-download) |>
  #This is the column that will help determine the css selector sequence to select the party statement links for each year
  mutate(party_row_number = seq(1, n()), 
         .by = year) |>
  #Rename column for clarity
  rename(party = party_link_to_document)

#The css-selector pattern for party statement links is like this... 
#first row/party for that year:       .table > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(2) > a:nth-child(1)
#every row/party after for that year: .table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1), 
#                                     .table > tbody:nth-child(2) > tr:nth-child(3) > td:nth-child(1) > a:nth-child(1), 
#                                     etc. 
#Iteratively scraping the texts of party platform statements and adjoining to Party_Platform_Final text column
for (i in 1:nrow(Party_Platform_Final)){

  tr_number = i
  
  if (Party_Platform_Final$party_row_number[i] == 1){
    td_number <- 2
  } else{
    td_number <- 1
  }
  
  value = paste0(".table > tbody:nth-child(2) > tr:nth-child(", tr_number, ") > td:nth-child(", td_number, ") > a:nth-child(1)")
  
  #Going to the party platform statement webpage
  remDr$findElement(using = "css selector", value = value)$clickElement()
  
  #Extracting and adjoining the party platform statement text to Party_Platform_Final
  Party_Platform_Final$text[i] <- remDr$getPageSource()[[1]] |>
    read_html() |>
    html_elements(".field-docs-content p") |>
    html_text2() |>
    str_c(collapse = " ") |>
    #Get rid of extra whitespace
    str_squish() 
  
  #Start again for next party platform statement 
  remDr$goBack()
}

#Get rid of unnecessary column 
Party_Platform_Final <- Party_Platform_Final |> select(-party_row_number)
  
#Create folder to store finalized .csv file 
dir.create("Party_Platform_Combined", showWarnings = FALSE)

#Exporting the final data set as .csv
write_csv(Party_Platform_Final , paste0("Party_Platform_Combined/Party_Platform_Final.csv"))
