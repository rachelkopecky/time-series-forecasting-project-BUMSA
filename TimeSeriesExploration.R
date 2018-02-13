
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
packages <- c("tidyverse", "lubridate", "forecast", "tibbletime", "ggplot2", "ggthemes")
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

#Order by Date
inbound <- inbound[order(inbound$Date), ]
outbound <- outbound[order(outbound$Date), ]

#Use tibble time to convert the data frames to time series
inbound <- as_tbl_time(inbound, Date) %>% 
        group_by(Report.Location)
outbound <- as_tbl_time(outbound, Date) %>% 
        group_by(Report.Location)

#Aggregate to location and day level to first plot
inbound_summary <- inbound %>% 
        dplyr::arrange(Report.Location, Date) %>% 
        dplyr::mutate(Date = collapse_index(Date, "monthly")) %>% 
        dplyr::group_by(Report.Location, Date, add = TRUE) %>% 
        dplyr::summarise(ttl_hrs = sum(Hours), 
                         ttl_pay = sum(Dollars), 
                         median_wage = median(Wage))

inbound_summary %>% 
        ggplot(aes(Date, 
                   ttl_pay)) +
        geom_line(aes(color = Report.Location)) +
        scale_y_continuous(labels = scales::dollar) +
        theme_tufte() +
        ggtitle("Total Wages by Day") +
        theme(axis.title.x = element_blank())

summary(inbound_summary)
#Might be best to remove "IC - Finance - Lou" locations since there are very few entries

#Need to decompose and plot
inbound_summary$Date <- as.POSIXct(inbound_summary$Date, format = "%Y-%m-%d")
decompose(inbound_summary)

#Need to develop a forecast for it
#Aggregate at other periods like day of week?