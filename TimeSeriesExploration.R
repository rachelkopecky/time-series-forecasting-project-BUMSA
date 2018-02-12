
#   ============================================================================
#                       Term 4 - Time Series Analysis Group Project            
#   ============================================================================
#   Purpose: Provide initial exploratory analysis of the inbound and outbound 
#               warehouse activity for a client. 
#   ============================================================================
#   Created: 02/08/2018
#   Members: Tammy Hang, Jack Letcher, Ian O'Conner, Rachel Kopecky, Paul Forst 
#            Bellarmine University
#   ----------------------------------------------------------------------------

#   Load Required Packages and Files  
#   Check that necessary packages are installed
packages <- c("tidyverse", "lubridate", "zoo", "forecast", "xts", "tibbletime")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#   Load Neccessary Packages
sapply(packages, require, character.only = TRUE)

inbound <- read.csv("Inbound Data.csv")
outbound <- read.csv("Outbound Data.csv")

#Convert Date to proper date class
inbound$Date <- mdy(inbound$Date)
outbound$Date <- mdy(outbound$Date)

#correction for comma in a Dollars field
inbound$Dollars <- str_replace(inbound$Dollars, ',', "")
outbound$Dollars <- str_replace(outbound$Dollars, ',', "")

#Convert Dollars and Wage to numeric
inbound$Dollars <- as.numeric(sub('\\$','',as.character(inbound$Dollars))) 
outbound$Dollars <- as.numeric(sub('\\$','',as.character(outbound$Dollars)))
inbound$Wage <- as.numeric(sub('\\$','',as.character(inbound$Wage))) 
outbound$Dollars <- as.numeric(sub('\\$','',as.character(outbound$Wage))) 

#Use tibble time to convert the data frames to time series
inbound <- as_tbl_time(inbound, Date)
outbound <- as_tbl_time(outbound, Date)

#Aggregate to location, task and day level to first plot
inbound_summary <- inbound %>% 
        dplyr::arrange(Report.Location, Etime.Labor.Task, Date) %>% 
        dplyr::mutate(date = collapse_index(Date, "monthly")) %>% 
        dplyr::group_by(Report.Location, Etime.Labor.Task, Date, add = TRUE) %>% 
        dplyr::summarise(ttl_hrs = sum(Hours), 
                         ttl_pay = sum(Dollars), 
                         median_wage = median(Wage))
