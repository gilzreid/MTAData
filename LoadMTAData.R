library(RCurl)
library(tidyverse)
library(ggplot2)
library(lubridate)

#setwd("G:/My Drive/SAC/Staff/GR/COVID_Dashboard/MTA Data")

#download <- getURL("http://web.mta.info/developers/data/nyct/fares/fares_200425.csv")
#Test <- read.csv (text = download, stringsAsFactors = FALSE, skip = 2)
#Test1 <- Test %>% replace(is.na(.), 0) %>% mutate(Total = rowSums(.[3:29]))

# Starting point for 2020 data
#StartDate = Saturday, January 04, 2020
# Starting point for 2019 data
# Use this to create historical file, or just load the pre-made csv
#StartDate = Saturday, January 05, 2019
#StartDate = as.Date("2019-01-05")

StartDate <- as.Date("2020-01-04")
EndDate <- today()
#EndDate = as.Date("2020-04-25")
#weeks <- 52

 Dates <- data.frame("Date" = seq(StartDate, EndDate, by="week"))
 Datesn <- Dates %>% filter(Date <= EndDate) %>% mutate(ShortDate = format(Date, "%y%m%d")) %>%
   mutate(url = 
          paste0("http://web.mta.info/developers/data/nyct/fares/fares_",
                 ShortDate, ".csv")) %>%
   mutate(weeknumber = strftime(Date, "%V"))
 


for (x in c(1:nrow(Datesn))) {
  print(paste0(x, " ", Datesn[x, 1]))
  download <- getURL(Datesn[x, 3])
  Dset <- read.csv(text = download, stringsAsFactors = FALSE, skip = 2) 
  Dset2 <- Dset %>% replace(is.na(.), 0) 
  Dset2[,c(3:29)] <- sapply(Dset2[, c(3:29)], as.numeric)
  Dset3 <- Dset2 %>% mutate(Total = rowSums(.[3:29])) %>% mutate(date = Datesn[x,1]) %>%
           mutate(weeknumber = Datesn[x,4])
  
  if (x == 1) {
    out <- Dset3
  }
  else {
    out <- bind_rows(out, Dset3) %>% replace(is.na(.), 0)
  }
}


#all_2019 <- out %>% filter(strftime(date, "%Y") == 2019)
#write.csv(all_2019, "MTAFares_2019.csv", row.names=FALSE)
MTAFares_2019 <- read.csv("MTAFares_2019.csv", stringsAsFactors = FALSE)
 
historical <- MTAFares_2019 %>% group_by(date) %>% summarise(weekly_ridership_2019 = sum(Total)) %>%
    filter(strftime(date, "%Y") == 2019) %>% mutate(weeknumber = strftime(date, "%V")) %>%
    select(weekly_ridership_2019, weeknumber)

historical_total <- MTAFares_2019 %>% 
  select(REMOTE, date, Total_2019 = Total) %>% mutate(weeknumber = strftime(date, "%V")) %>%
  select(REMOTE, weeknumber, Total_2019)

this_year <- out %>% filter(strftime(date, "%Y") == 2020) %>%
    left_join(historical_total, by = c("REMOTE", "weeknumber")) %>%
    select(REMOTE, STATION, date, weeknumber, Total, Total_2019)
write.csv(this_year, "MTA_weekly_data_2020.csv", row.names = FALSE, na = "")

# Summary dataset
# current_year <- out %>% group_by(date) %>% summarise(weekly_ridership_2020 = sum(Total)) %>%
#   filter(strftime(date, "%Y") == 2020) %>% mutate(weeknumber = strftime(date, "%V")) %>%
#   select(weekly_ridership_2020, weeknumber, date)
# 
# comparison <- left_join(current_year, historical, by = "weeknumber") %>% 
#   select(date, weeknumber, weekly_ridership_2019, weekly_ridership_2020)
# 
# write.csv(comparison, "MTA_weekly_data.csv", row.names = FALSE)

# Station List 
# stations <- out %>% select(REMOTE, STATION) %>% unique()
# write.csv(stations, "MTA_station_codes.csv", row.names = FALSE)
