# WEB SCRAPING ################################################################
# Still to do:
# - Reformat data
# - Merge old data with new data 
# - Create an extension for identifying the newest and only importing that,
#   adding it to existing data file
# - This data file is aimed to feed into analysis for factsheets
# INSTALL PACKAGES ############################################################

#install.packages("tidyverse")
#install.packages("rvest")


# LOAD PACKAGES ###############################################################

library(dplyr)#Issue with package conflict so loaded separately
library(tidyverse)
library(rvest)

# PREPARE FOR WEB SCRAPING ####################################################

# For pulling in the main links for getting the data
url_main <- "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics"

# PERFORM FIRST WEB SCRAPE #####################################################

# Read HTML from main URL
main_webpage <- read_html(url_main)

# Save list of url's with final in the link 
weblink_main <- main_webpage %>%
  html_nodes("a") %>%  
  html_attr("href") %>%
  str_subset("final") %>%
  as.data.frame()

# PERFORM SECOND WEB SCRAPE ####################################################

#Create blank dataframe for storing output of main dataframe from  loop
df_scrape <- data.frame('RERPORTING_PERIOD_START' = character(), 
               'PEPORTING_PERIOD_END' = character() ,
               'STATUS' = character(),
               'BREAKDOWN' = character(),
               'PRIMARY_LEVEL' = character(),
               'PRIMARY_LEVEL_DESCRIPTION' = character(),
               'SECONDARY_LEVEL' = character(),
               'SECONDARY_LEVEL_DESCRIPTION' = character(),
               'MEASURE_ID' = character(),
               'MEASURE_NAME' = character(),
               'MEASURE_VALUE' = numeric())

# Create dataframe for storing output for change in data 
df_scrape_old <- data.frame("REPORTING_PERIOD" = character(),
                   "STATUS" = character(),
                   "BREAKDOWN" = character(),
                   "PRIMARY_LEVEL" = character(),
                   "PRIMARY_LEVEL_DESCRIPTION" = character(),
                   "SECONDARY_LEVEL" = character(),            
                    "SECONDARY_LEVEL_DESCRIPTION" = character(), 
                   "MEASURE_VALUE" = character(),
                   "MEASURE_ID" = character(),
                   "MEASURE_NAME" = numeric() )

# Create dataframe for catching any links that have been missed
df_2 <- data_frame()

# Set variable for counting the number of loop iterations
# Helps with keeping track of where it has failed if issues
y = 0

# loop iterates through each of links to get dataset 
for (x in unique(weblink_main$.)) {
    
    # Count of iterations
    y = y +1
  
    # Takes base url and adds extension of year and month, pulls in HTML
    webpage <- read_html(paste("https://digital.nhs.uk/",x, sep="")) 
  
    # Identifies CSV documents embedded within HTML saves as dataframe
    weblink <- webpage %>%
        html_nodes("a") %>%       
        html_attr("href") %>%     
        str_subset("\\.csv") %>%
        as.data.frame()
   
    # Some links did not have http at the front, this if statement corrects this
    if(substr(weblink[1,1],1,4) != "http") {
     # reads in csv 
      df <- read.csv(paste("https://digital.nhs.uk/",weblink[1,1],sep=""))
   } else {
    
     # Reads in csv and saves as dataframe
    df <- read.csv(weblink[1,1])
   }
    # When the dataframe does not have the number of columns expected in the
    # most recent format of the data 
    if( ncol(df) != 11) {
      
      # This formats the data into the format of the old data before the 
      # it will be collectively transformed later into the new data format
      df <- df %>%
        gather(MEASURE, MEASURE_VALUE, 8:ncol(df)) %>%
        mutate(MEASURE_ID = substr(MEASURE,1,5),
               MEASURE_NAME = substr(MEASURE,9, nchar(MEASURE))) %>%
        select(-MEASURE)
      
      # If number of columns is 10 and names match the format of old then bind
      # into the second stored format 
      if(ncol(df) == 10 & length(intersect(colnames(df), colnames(df_scrape_old))) == 10)  {
        # Old format of data is stored
        df_scrape_old <- rbind(df_scrape_old,
                               df)
      } else {
      
      # This section maintains a list of links which have not made it into
      # either the old format or new format, they may need to be added manually
      df_3 <- x %>% 
        as.data.frame()
      
      df_2 <- rbind(df_3,
                    df_2)
      }
    }  else( 
    
    # The dataframe for storing the newest format of the data
    df_scrape <- rbind(df_scrape,
                       df)
    )
    
    # Print the link for this iteration of the loop
    print(x)
    
}

# FORMAT DATA ##################################################################

df_scrape <- df_scrape %>%
  mutate(MEASURE_NAME_AGE = case_when(str_detect( MEASURE_NAME, "ged 0 to 17")== TRUE~ "0 -17",
                                      str_detect( MEASURE_NAME, "ged 18 to 34")== TRUE~ "18 -34",
                                      str_detect( MEASURE_NAME, "ged 18 to 69")== TRUE~ "18 -69",
                                      str_detect( MEASURE_NAME, "0 to 18")== TRUE~ "0 -18",
                                      str_detect( MEASURE_NAME, "under 18")== TRUE~ "0 -17",
                                      str_detect( MEASURE_NAME, "0 and 17")== TRUE~ "0 -17",
                                      str_detect( MEASURE_NAME, "Age 17 bed")== TRUE~ "17",
                                      str_detect( MEASURE_NAME, "Age 16 bed")== TRUE~ "16",
                                      str_detect( MEASURE_NAME, "65 and")== TRUE~ "65",
                                      str_detect( MEASURE_NAME, "19 to64")== TRUE~ "19 -64",
                                      str_detect( MEASURE_NAME, "18 to 64")== TRUE~ "18 -64",
                                      str_detect( MEASURE_NAME, "19 to 64")== TRUE~ "19 -64",
                                      str_detect( MEASURE_NAME, "35 and over")== TRUE~ "35",
                                      str_detect( MEASURE_NAME, "0-18")== TRUE~ "0-18",
                                      str_detect( MEASURE_NAME, "18 and over")== TRUE~ "18",
                                  T~ "All age"),
         Referrals_ind = case_when(str_detect( tolower(MEASURE_NAME), "referral")== TRUE~ 1,
                           T ~ 0),
         Discharge_ind = case_when(str_detect( tolower(MEASURE_NAME), "discharge")== TRUE~ 1,
                                   T ~ 0),
         Contacts_ind = case_when(str_detect( tolower(MEASURE_NAME), "contact")== TRUE~ 1,
                                   T ~ 0),
         Admissions_ind = case_when(str_detect( tolower(MEASURE_NAME), "admission")== TRUE~ 1,
                                  T ~ 0),
         MHA_ind = case_when(str_detect( tolower(MEASURE_NAME), "detention")== TRUE~ 1,
                                    str_detect( tolower(MEASURE_NAME), "short term order")== TRUE~ 1,
                             str_detect( tolower(MEASURE_NAME), "section 136")== TRUE~ 1,
                             str_detect( tolower(MEASURE_NAME), "cto")== TRUE~ 1,
                                    T ~ 0),
         MEASURE_VALUE = as.numeric(MEASURE_VALUE)) 


# EXPORT DATA ##################################################################

write.csv(df_scrape, "Mental_health_monthly.csv")