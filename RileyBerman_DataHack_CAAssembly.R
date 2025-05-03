#Contributor: Riley Berman 
#Description: Created a tibble containing all the names of the members of the California State Assembly (79 members, 1 vacancy) 
#along with their party affiliation, district, date they assumed office, counties represented, committees they are on, and their Twitter handle. 
#Methodology: Web scraped from a few webpages and compiled the information into one big tibble.  

library(tidyverse)
library(rvest)
library(janitor)

link <- "https://ballotpedia.org/California_State_Assembly"

page <- read_html(link)

#Get individual assembly members' date they assumed office 
CA_Assembly_Members <- page |> html_table() |> 
  pluck(4) |> as_tibble() |> clean_names() |> 
  #Getting district numbers for matching 
  extract(col = office, into = "district", regex = "(\\d{1,})") |>
  #Changing 1-9 districts to 01, 02, for matching
  #Changing "Democratic" to "Democrat" for matching
  mutate(district = str_pad(district, width = 2, pad = "0"), 
         party = ifelse(party == "Democratic", "Democrat", party))

link <- "https://www.assembly.ca.gov/assemblymembers"

#Gathering hyperlinks to individual assembly members' pages
hyperlinks <- read_html(link) |>
  html_elements(".link--committees") |>
  html_attr("href") 

All_Assembly_Members <- tibble()
  
for (link in hyperlinks){
  
  #Adding the base URL to the link
  link = paste0("https://www.assembly.ca.gov", link)
  
  #Getting assembly member's name 
  Member_Name <- read_html(link) |>
    html_elements(".name") |>
    html_text2() |>
    #Puts a space between the first and last name (e.g. DawnAddis -> Dawn Addis)
    str_replace_all("(?<=[a-z])(?=[A-Z])", " ") |>
    as_tibble() |>
    rename(name = value) 
  
  #Getting their committee membership (standing, subcommittees, select, and joint committees)
  Member_Committees <- read_html(link) |>
    html_elements(".col.member_committees") |>
    html_elements("p") |>
    html_text2() |> 
    #Standing committees are the ones not designated 
    str_c(collapse = ", ") |> 
    as_tibble() |>
    rename(committees = value)
  
  #Getting their district number, party affiliation, and counties represented
  Member_Information <- read_html(link) |>
    html_elements(".top_info") |>
    html_text2() |>
    str_replace_all(pattern = "\n", replacement = " ") |>
    as_tibble() |>
    #Extracting string information into 3 columns 
    #Pattern looks for information after District, Party, and Counties Represented
    extract(value, into = c("district", "party", "counties_represented"),
            regex = "District ([^P]+) Party ([^C]+) Counties Represented (.+)") |> 
    mutate(across(everything(), str_trim)) |>
    #Capitalizing counties 
    mutate(counties_represented = str_to_title(counties_represented)) 
  
  Assembly_Member <- cbind(Member_Name, Member_Information, Member_Committees)
  
  All_Assembly_Members <- rbind(Assembly_Member, All_Assembly_Members)
}

#Current vacancy in district 63
All_Assembly_Members <- All_Assembly_Members |> arrange(district)

#Getting rid of middle initial in name column for matching (e.g., Esmeralda Z.Soria --> Esmeraldo Soria)
All_Assembly_Members_cleaned <- All_Assembly_Members |> 
  #Automatically stops before and after it finds whitespace
  mutate(name = gsub(name, pattern = "[A-Z]+\\.", replacement = ""))

#Combine All_Assembly_Members with CA_Assembly_Members
#Matching by district and party affiliation (their names are slightly different, could use fuzzyjoin if needed)
CA_Assembly <- inner_join(CA_Assembly_Members, All_Assembly_Members_cleaned, by = c("district", "party")) |>
  select(-name.y) |>
  rename(name = name.x) 
  
#Getting the Twitter handles from the California Health Coalition Advocacy website
link <- "https://californiahealthcoalitionadvocacy.org/twitterhandles/"

CA_Twitter <- read_html(link) |>
  html_elements(".landing-block-node-text.g-font-size-18") |>
  html_text2() |>
  #Just take Assemblymember Twitter handles
  pluck(3) |>
  str_split(pattern = "\n\n") |> unlist() |>
  as_tibble() |>
  rename(twitter = value) |>
  separate(twitter, into = c("name", "twitter"), sep = "(?=@)") |>
  #Extracting name for matching (everything after "Assemblymember)
  #NAs message is for Assemblymembers with no Twitter handles (i.e, Jordan Cunningham, Megan Dahle, Tom Daly, and Jim Patterson) 
  #FUN Fact: use member instead of assembly member due to row mispelling: "Assembymember Miguel Santiago @SantiagoAD53" 
  extract(col = name, into = "name", regex = "(?<=member)(.*)") |>
  mutate(across(everything(), str_trim)) 

#Combine CA_Assembly with CA_Twitter
#There are a lot of missing Twitter handles, we will have to add these manually 
CA_Assembly_Final <- left_join(CA_Assembly, CA_Twitter, by = "name") 

#Exporting the final data set as .csv
write_csv(CA_Assembly_Final, "CA_Assembly_Final.csv")





