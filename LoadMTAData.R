library(RCurl)
library(tidyverse)
library(ggplot2)
library(lubridate)

# 
# Purpose:
#   
#   Create a dataset for monitoring the weekly MTA fare count data, including 2020 weekly counts
#   in one column and 2019 counts in another, by station and week of the year.
# 
# Input:
#   MTA weekly fare csv files from the MTA developers page (http://web.mta.info/developers/download.html)
# 
# Main steps:
#   1. Generate a list of the dates of the weekly data files for 2020.
#   2. Loop through the list and download each csv, appending all available weeks into a single
#       dataframe, adding date and week number labels. Also calculate a total column summing all fare types.
#   3. Join the 2019 weekly counts onto this data frame using the week number.
#   4. Output to csv.
# 
# 
# Note that the 2019 weekly data used for comparison has already been processed into the file
# MTAFares_2019.csv to save downloading this data every run.
# 
# Author:
#   Giles Reid (giles.reid@columbia.edu)
#




# Starting point for 2019 data
# Use this if the 2019 file needs to be re-created.
# StartDate = as.Date("2019-01-05")

# Starting point for 2020 data, end date is today so that the latest weekly data will be included.
StartDate <- as.Date("2020-01-04")
EndDate <- today()

# List of weekly dates which are used in the csv file names.
Dates <- data.frame("Date" = seq(StartDate, EndDate, by="week")) %>% 
    filter(Date <= EndDate) %>% mutate(ShortDate = format(Date, "%y%m%d")) %>%
    mutate(url = 
          paste0("http://web.mta.info/developers/data/nyct/fares/fares_",
                 ShortDate, ".csv")) %>%
    mutate(weeknumber = strftime(Date, "%V"))
 
# Load in existing data and only download dates that aren't already downloaded
ExistingData <- read.csv("MTA_weekly_data_2020.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date), weeknumber = strftime(date, "%V"))

Dates_2020 <- ExistingData %>%
  select(date) %>%
  unique() %>%
  mutate(existing_date = 1)
  
  
new_dates <- Dates %>% 
    left_join(Dates_2020, by = c("Date" = "date")) %>%
    filter(is.na(existing_date))

for (x in c(1:nrow(new_dates))) {
  print(paste0(x, " ", new_dates[x, 1]))
  download <- getURL(new_dates[x, 3])
  Dset <- read.csv(text = download, stringsAsFactors = FALSE, skip = 2) %>%
    replace(is.na(.), 0)
  Dset[,c(3:29)] <- sapply(Dset[, c(3:29)], as.numeric)
  Dset <- Dset %>% mutate(Total_2020 = rowSums(.[3:29])) %>% mutate(date = new_dates[x,1]) %>%
           mutate(weeknumber = new_dates[x,4])

  if (x == 1) {
    out <- Dset
  }
  else {
    out <- bind_rows(out, Dset) %>% replace(is.na(.), 0)
  }
}

out_combined <- bind_rows(ExistingData, out)

# For reading in all dates

# for (x in c(1:nrow(Dates))) {
#   print(paste0(x, " ", Dates[x, 1]))
#   download <- getURL(Dates[x, 3])
#   Dset <- read.csv(text = download, stringsAsFactors = FALSE, skip = 2) %>% 
#     replace(is.na(.), 0)
#   Dset[,c(3:29)] <- sapply(Dset[, c(3:29)], as.numeric)
#   Dset <- Dset %>% mutate(Total = rowSums(.[3:29])) %>% mutate(date = Dates[x,1]) %>%
#            mutate(weeknumber = Dates[x,4])
#   
#   if (x == 1) {
#     out <- Dset
#   }
#   else {
#     out <- bind_rows(out, Dset) %>% replace(is.na(.), 0)
#   }
# }


# If we want to re-write out the 2019 data this can be used:
#all_2019 <- out %>% filter(strftime(date, "%Y") == 2019)
#write.csv(all_2019, "MTAFares_2019.csv", row.names=FALSE)

# Load the pre-created full 2019 dataset - this avoids having to download all the 2019
# weekly data every run.
MTAFares_2019 <- read.csv("MTAFares_2019.csv", stringsAsFactors = FALSE)
 
# This creates the overall counts by week for the 2019 dataset.
# Note we use the weeknumber as the 2019 to 2020 comparison linkage variable.
historical_summary <- MTAFares_2019 %>% 
  group_by(date) %>% 
  summarise(weekly_ridership_2019 = sum(Total)) %>%
  filter(strftime(date, "%Y") == 2019) %>% 
  mutate(weeknumber = strftime(date, "%V")) %>%
  select(weekly_ridership_2019, weeknumber)

# This creates the by-station 2019 dataset with the week number.
historical_by_station <- MTAFares_2019 %>% 
  select(REMOTE, date, Total_2019 = Total) %>% 
  mutate(weeknumber = strftime(date, "%V")) %>%
  select(REMOTE, weeknumber, Total_2019)

# Take the 2020 data and join the 2019 numbers onto it so that we have a final dataset
# with 2019 total and 2020 total for each station and each week.
# weekly_data_2020 <- out_combined %>% 
#   select(-Total_2019) %>%
#   filter(strftime(date, "%Y") == 2020) %>%
#   left_join(historical_by_station, by = c("REMOTE", "weeknumber")) %>%
#   select(REMOTE, STATION, date, weeknumber, Total_2020 = Total, Total_2019)
weekly_data_2020 <- out_combined %>%
  select(-Total_2019) %>%
  #filter(strftime(date, "%Y") == 2020) %>%
  mutate(weeknumber = if_else(as.numeric(weeknumber) == 53, as.character(52), weeknumber)) %>%
  left_join(historical_by_station, by = c("REMOTE", "weeknumber")) %>%
  select(REMOTE, STATION, date, weeknumber, Total_2020, Total_2019)

write.csv(weekly_data_2020, "MTA_weekly_data_2020.csv", row.names = FALSE, na = "")

# Summary dataset
# This dataset just includes counts summed across all stations, making a much more compact file
#
#
 # summary_2020 <- out_combined %>% 
 #   group_by(date) %>% 
 #   summarise(weekly_ridership_2020 = sum(Total_2020)) %>%
 #   filter(strftime(date, "%Y") == 2020) %>% 
 #   mutate(weeknumber = strftime(date, "%V")) %>%
 #   select(weekly_ridership_2020, weeknumber, date)
 #

summary_2020 <- weekly_data_2020 %>%
  group_by(date) %>%
  summarise(weekly_ridership_2020 = sum(Total_2020)) %>%
  filter(strftime(date, "%Y") == 2020) %>%
  mutate(weeknumber = strftime(date, "%V")) %>%
  select(weekly_ridership_2020, weeknumber, date)

 summary_dataset <- left_join(summary_2020, historical_summary, by = "weeknumber") %>% 
   select(date, weeknumber, weekly_ridership_2019, weekly_ridership_2020)
 
 write.csv(summary_dataset, "MTA_weekly_data_summary.csv", row.names = FALSE)

# To create list of the unique stations with their codes.
# This shouldn't change often! 
#
# stations <- out %>% select(REMOTE, STATION) %>% unique()
# write.csv(stations, "MTA_station_codes.csv", row.names = FALSE)

## The following code chunk can be used to load individual weekly fare files
#download <- getURL("http://web.mta.info/developers/data/nyct/fares/fares_200425.csv")
#Test <- read.csv (text = download, stringsAsFactors = FALSE, skip = 2)
#Test1 <- Test %>% replace(is.na(.), 0) %>% mutate(Total = rowSums(.[3:29]))

