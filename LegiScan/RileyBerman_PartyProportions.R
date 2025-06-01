#Contributor: Riley Berman 
#Description: Retrieves Democratic and Republican party counts in the California State Legislature to assist RileyBerman_LegiScan_Analysis.R
#             Final results is three different tibbles: senate_assembly_count_table, senate_assembly_DR, senate_assembly_year
#Methodology: Webscraping with rvest

library(rvest)
library(tidyverse)

#To get Democratic and Republican party counts in the California State Legislature 
link <- "https://ballotpedia.org/California_State_Legislature"

#Note: The total number of seats does not always equal 40 due to vacancies in the chamber.
senate_count <- read_html(link) |>
  html_table() |>
  pluck(3) |>
  mutate(role = "Sen")

assembly_count <- read_html(link) |>
  html_table() |>
  pluck(5) |>
  mutate(role = "Rep")

#Getting rid of the "*" in column '94
assembly_count$`'94` <- assembly_count$`'94` |> str_remove_all("\\D") |> as.numeric()

senate_assembly_count <- bind_rows(senate_count, assembly_count)

#Change the format and modify election_year column
#Note: session_year are two-year periods starting in the December of the even-numbered year
#Example: 2017-2018 session begins in December 2016 and ends in November 2018 ~ roughly two years
#For example, 1992 actually means 1993-1994 session_year
senate_assembly_count <- senate_assembly_count |>
  pivot_longer(cols = -c(Year, role), names_to  = "election_year", values_to = "count") |>
  pivot_wider(names_from = Year, values_from = count) |>
  rename(session_year = election_year) |>
  mutate(session_year = str_remove_all(session_year, "\\D"),
         session_year = ifelse(str_detect(session_year, "9"), paste0("19", session_year), paste0("20", session_year)),
         session_year = as.numeric(session_year)) |>
  mutate(session_year = paste0(session_year + 1, "-", session_year + 2)) |>
  mutate(Independents = ifelse(is.na(Independents), 0, Independents))

senate_assembly_count_table <- senate_assembly_count |>
  summarize(total_republicans = Republicans, 
            total_democrats = Democrats,
            total_independents = Independents,
            total_members = total_republicans + total_democrats + total_independents, 
            republican_percentage = total_republicans/total_members,
            democrat_percentage = total_democrats/total_members,
            independent_percentage = total_independents/total_members, 
            .by = c(session_year, role))

#With no independents
senate_assembly_DR <- senate_assembly_count |>
  summarize(total_republicans = Republicans, 
            total_democrats = Democrats,
            total_members = total_republicans + total_democrats, 
            republican_percentage = total_republicans/total_members,
            democrat_percentage = total_democrats/total_members,
            .by = c(session_year, role))

#With no role column, so results just for each session_year, excluding independents
senate_assembly_year <- senate_assembly_count |>
  summarize(total_republicans = sum(Republicans, na.rm = TRUE), 
            total_democrats = sum(Democrats, na.rm = TRUE),
            total_members = total_republicans + total_democrats, 
            republican_percentage = total_republicans/total_members,
            democrat_percentage = total_democrats/total_members,
            .by = session_year)
