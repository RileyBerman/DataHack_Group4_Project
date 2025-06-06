---
editor_options: 
  markdown: 
    wrap: 72
---

**Objective:** Retrieve all available information about the political
activity of California's legislators from [LegiScan](https://legiscan.com/) using it's API. 
Use `LegiScan.Rproj` to run files. 

**Files:**

-   `RileyBerman_LegiScan_API.py`: Retrieves datasets from the LegiScan
    API relating to the political activity of California's legislators,
    including but not limited to a bill's voters, sponsors, and history
    and politicians' information from the 2009-2010 to the 2025-2026
    Regular Session.

    -   This is a Python file. It uses my *personal API key*. It is not available
    on the GitHub repository. Please use `RileyBerman_LegiScan_API_Redacted.py` for
    replication purposes with your own personal LegiScan API key. 

    -   Outputs results to LegiScan folder as a folder named `CA` (which
        is renamed `LegiScan_CA`). Within this folder are many
        subfolders containing .csv files that record the political
        activity of various Regular Sessions of California's
        legislature.

-   `RileyBerman_LegiScan_API_Redacted.py` : Copy of
    `RileyBerman_LegiScan_API.py`. Use for replication purposes.

-   `LegiScan_API_User_Manual.pdf`: LegiScan's official documentation for
    their API.

-   `LegiScan_CA`: Renamed folder of `CA` obtained from
    `RileyBerman_LegiScan_API.py`. Contains subfolders with information
    about each Regular Session of California's legislature. Within each
    subfolder is a subfolder called `csv` which contains 7 .csv files
    relating to various aspects of the Regular Session. There is a
    README.md file in each subfolder discussing the various dataset
    variables.

-   `RileyBerman_LegiScan_Combine.R`: Combines the subfolders of
    `LegiScan_CA` into comprehensive tibbles and outputs the results as
    .csv files to the folder `LegiScan_CA_Combined`.

-   `LegiScan_CA_Combined`: Folder containing five comprehensive .csv
    files obtained from

    1.  `documents_combined.csv`: all the documents .csv files combined.

    2.  `history_combined.csv`: all the history .csv files combined.

    3.  `bills_combined.csv`: all the bills .csv files combined.

    4.  `people_combined.csv`: all the people .csv files combined.

    5.  `votes_rollcalls_sponsors_bills_combined:`all the votes,
        rollcalls, sponsors, and bills .csv files combined.

-   `RileyBerman_LegiScan_Analysis.R`: Performs analysis on
    the .csv files in `LegiScan_CA_Combined`. Outputs presentation graphs to `Graphs` folder. 

-   `RileyBerman_PartyProportions.R`: Retrieves Democratic, Republican,
    and Independent party counts through web scraping. Sourced in
    `RileyBerman_LegiScan_Analysis.R`.
    
-   `Graphs:` Folder containing presentation graphs from
    `RileyBerman_LegiScan_Analysis.R`. 

-   `RileyBerman_LegiScan_Analysis2.R`: Continues the analysis of
    `RileyBerman_LegiScan_Analysis.R`. Sources `RileyBerman_LegiScan_Analysis.R`.
    Produces `bills_statistics_final.csv` to be copied over to the Twitter project.
