---
editor_options: 
  markdown: 
    wrap: 72
---

**Objective:** Web scrape the content of all party platform statements available 
[here](https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/party-platforms-and-nominating-conventions-3)
Save output as a .csv file. Use `Party_Platform.Rproj` to run files. 

**Files:**

-   `RileyBerman_PartyPlatform_Combine.R`: Web scrape using `rvest` from the
    website to compile a dataset of party platform statements. Outputs `Party_Platform_Final`
    as a .csv file to the folder `Party_Platform_Combined`.

-   `Party_Platform_Combined`: Folder containing `Party_Platform_Final.csv`. 