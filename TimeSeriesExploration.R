
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
packages <- c("tidyverse", "lubridate", "xlsx")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#   Load Neccessary Packages
sapply(packages, require, character.only = TRUE)

inbound <- read.csv("Inbound Data.csv")
outbound <- read.csv("Outbound Data.csv")

#Convert Date to proper date class
inbound$Date <- mdy(inbound$Date)
outbound$Date <- mdy(outbound$Date)

