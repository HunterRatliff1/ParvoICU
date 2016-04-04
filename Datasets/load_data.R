# # # # # # # # # # #
# Required Packages #
# # # # # # # # # # #
# This script requires the following packages. If this is your first time
# using any of these packages, they will be installed from CRAN when you
# run the line
if(!require(googlesheets)) install.packages("googlesheets")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(rio)) install.packages("rio")


#  /***************\
#  * Pull the data *
#  \***************/
# Assuming that you've added the spreadsheet to your Google Drive,
# call the spreadsheet by it's name, and select the first sheet. If this
# is your first time calling the function, you'll need to authenticate
# your Google acount in the page that opens automatically in the browser

## Read the parsed data that I have made some manual adjustments to
df <- gs_title("End of Shift Report") %>% gs_read_csv("Parsed") %>%
  select(-BLANK) %>%
  mutate(
    Timestamp = floor_date(ymd_hms(Timestamp), "minute"), # Vector as POSIXct
    Hour_Sent = hour(Timestamp))    # Get the hour the report was submitted

# Read sheet with list of categorical vars
categorical <- gs_title("End of Shift Report") %>% 
  gs_read_csv("categorical_vars")

## Count the number of volunteers on the shift
df$Count_Vols <- sapply(df$Volunteers...Staff, function(x){
  x %>% gsub(pattern="\\.", replacement="", .) %>%
    tolower() %>% read.csv(text=., header=F) %>% ncol()
}) %>% as.numeric()

# Export to CSV
export(df, "Datasets/Reports.csv")
export(df, "Datasets/Reports.RDS")



list(
  Medications = unique(categorical$Medications),
  MedSupplies = unique(categorical$MedSupplies),
  Supplies    = unique(categorical$Supplies),
  Vols = unique(categorical$Volunteers),
  Vol2 = unique(categorical$Vol2))













# # # # # # # # # # # # # # # # # # #
# Create an object of numeric data  #
# # # # # # # # # # # # # # # # # # #
Numbers <- df %>% 
  
  # Select the columns we care about
  select(
    Date.Time = Timestamp,
    Durt.Shft = Shift.Time,
    Vols.Num  = Count_Vols,
    Dogs.Dsc  = Count_Dsc,
    Dogs.Crt  = Count_Crt,
    Dogs.Int  = Count_Int,
    Dogs.Wel  = Count_Wel) %>% 
  
  # Group by the day (-2 hrs for shifts that go until 3 AM)
  group_by(Date=floor_date(Date.Time-hours(3), "day")) %>% 
  
  ## Order shifts (per day) by their cumulative rank. Normally
  ## the AM==1 & PM==2; however, if the report was submitted mid-
  ## day, the AM==1, mid-day==2, and PM==3
  mutate(
    AMPM = 1,
    AMPM = order_by(Date.Time, cumsum(AMPM))) %>%
  
  ## Get the time (hours:mins) as a number
  mutate(
    Hr  = hour(Date.Time),      # get the hours
    Min = minute(Date.Time)/60, # get the minutes as fraction of hours
    HrMin = Hr + round(Min, digits = 1)) %>% # add and round
  
  mutate(HrMin = as.numeric(HrMin)) %>%
  
#   ## Handle the PM shifts that lasted into the AM
#   mutate(HrMin = ifelse(HrMin<3,     # if:   before 3 AM
#                         HrMin+24,    # then: add 24 hrs
#                         HrMin)) %>%  # else: make no change
    
  # Drop the old columns we made & reorder
  select(Date, AMPM, HrMin, Durt.Shft, Vols.Num, Dogs.Dsc:Dogs.Wel) %>%
  arrange(desc(Date)) %>% ungroup() %>% tbl_df()

### Save as a RDS object
saveRDS(Numbers, "Datasets/Numbers.RDS")




Numbers.melt <- Numbers %>% 
  gather(Dogs.Dsc:Dogs.Wel, key = Status, value = Count) %>%
  filter(Count>0, !is.na(Count)) %>%
  group_by(Date, AMPM) %>%
  mutate(
    All     = sum(Count, na.rm = T),
    Status  = gsub("Dogs\\.", "", Status),
    Percent = Count/All) %>%
  ungroup() %>% tbl_df()

### Save as a RDS object
saveRDS(Numbers.melt, "Datasets/Numbers_melt.RDS")
  


