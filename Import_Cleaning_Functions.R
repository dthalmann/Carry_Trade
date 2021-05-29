###############################################################################
#Load Relevant Packages & set Working Directory
###############################################################################

library(tidyverse)
library(lubridate)
library(zoo)
setwd("C:/Users/sveng/OneDrive/Dokumente/Carry")

###############################################################################
#Path to Data Sets
###############################################################################

NZD_Exchange_Rates_1 <- "NZD_Exchange_Rates_1.csv"
NZD_Exchange_Rates_2 <- "NZD_Exchange_Rates_2.csv"
NZD_Exchange_Rates_3 <- "NZD_Exchange_Rates_3.csv"

NZD_Rates_1 <- "NZD_Rates_1.csv"
NZD_Rates_2 <- "NZD_Rates_2.csv"

JPY3MTD156N <- "JPY3MTD156N.csv"
JPY_Yields <- "JPY_Yields.csv"
IRSTCI01JPM156N <- "IRSTCI01JPM156N.csv"


###############################################################################
#Import & Clean all necessary Data Sets
###############################################################################

#------------------------------------------------------------------------------
#JPY/NZD Exchange Rate
#------------------------------------------------------------------------------

JPY_NZD_1 <- read_csv(NZD_Exchange_Rates_1, skip = 3)
JPY_NZD_2 <- read_csv(NZD_Exchange_Rates_2, skip = 3)
JPY_NZD_3 <- read_csv(NZD_Exchange_Rates_3, skip = 3)

#delete first row
JPY_NZD_1 <- JPY_NZD_1[-1,]
JPY_NZD_2 <- JPY_NZD_2[-1,]
JPY_NZD_3 <- JPY_NZD_3[-1,]

#select NZD/JPY 
JPY_NZD_1 <- JPY_NZD_1[c("Unit","NZD/JPY")]
JPY_NZD_2 <- JPY_NZD_2[c("Unit","NZD/JPY")]
JPY_NZD_3 <- JPY_NZD_3[c("Unit","NZD/JPY")]

#reformat date

JPY_NZD_1$Unit <- as.Date(JPY_NZD_1$Unit, format = "%d %h %Y")
JPY_NZD_2$Unit <- as.Date(JPY_NZD_2$Unit, format = "%d %h %Y")
JPY_NZD_3$Unit <- as.Date(JPY_NZD_3$Unit, format = "%d-%h-%y")

#Combine Rates for all dates
JPY_NZD <- rbind(JPY_NZD_1, JPY_NZD_2, JPY_NZD_3)

names(JPY_NZD) <- c("Date", "Spot")

JPY_NZD$Spot <- as.numeric(JPY_NZD$Spot)

#Select all data From 04/01/1990
JPY_NZD <- JPY_NZD[JPY_NZD$Date >= as.Date("04/01/1990", format = "%d/%m/%Y"),]

#------------------------------------------------------------------------------
#NZD Interest Rates
#------------------------------------------------------------------------------
NZD_Rates_1 <- read_csv(NZD_Rates_1, skip = 1)[1:10]
NZD_Rates_2 <- read_csv(NZD_Rates_2, skip = 1)[1:10]

#Combine Rates for all dates
NZD_Rates <- rbind(NZD_Rates_1, NZD_Rates_2)

#Format Data and Delete unnecessary Rows/ Columns
NZD_Rates[2:10] <- lapply(NZD_Rates[2:10], as.numeric)
NZD_Rates <- NZD_Rates[-c(1:3),-2]

NZD_Rates$X1 <- as.Date(NZD_Rates$X1, format = "%d %h %Y")

names(NZD_Rates) <- c("Date", "NZD_1d", "NZD_30d",
                      "NZD_60d", "NZD_90d", "NZD_1Y",
                      "NZD_2Y", "NZD_5Y", "NZD_10Y")


#Select all data From 04/01/1990
NZD_Rates <- NZD_Rates[NZD_Rates$Date >= as.Date("04/01/1990", format = "%d/%m/%Y"),]

#------------------------------------------------------------------------------
#JPY Interest Rates
#------------------------------------------------------------------------------  
JPY_Rate_3M <- read_csv(JPY3MTD156N)
JPY_Rate_1d <- read_csv(IRSTCI01JPM156N)
names(JPY_Rate_3M) <- c("Date", "3M")
names(JPY_Rate_1d) <- c("Date", "1d")
JPY_Rate_3M$`3M`<- as.numeric(JPY_Rate_3M$`3M`)

JPY_Rates <- read_csv(JPY_Yields, skip = 1)
JPY_Rates$Date <- as.Date(JPY_Rates$Date, format = "%d/%m/%Y")

#Combine Rates and delete unnecessary Data
JPY_Rates <- merge(JPY_Rates, JPY_Rate_3M, by = "Date", all.x = T, all.y = T)[c(1:11, 17)]
JPY_Rates <- merge(JPY_Rates, JPY_Rate_1d, by = "Date", all.x = T, all.y = T)
JPY_Rates$`10Y` <- as.numeric(JPY_Rates$`10Y`)

names(JPY_Rates) <- c("Date", "JPY_1Y", "JPY_2Y", "JPY_3Y",
                      "JPY_4Y", "JPY_5Y", "JPY_6Y",
                      "JPY_7Y", "JPY_8Y", "JPY_9Y",
                      "JPY_10Y", "JPY_90d", "JPY_1d")

#Select all data From 04/01/1990
JPY_Rates <- JPY_Rates[JPY_Rates$Date >= as.Date("04/01/1990", format = "%d/%m/%Y"),]

#first value must be given to interpolate missing values
JPY_Rates$JPY_1d[1] <- JPY_Rates$JPY_1d[21]

#------------------------------------------------------------------------------
#combine Interest - and Exchange Rates & add missing Values as Values from Day before
#------------------------------------------------------------------------------ 

#create date vector to add missing weekend dates etc. to data frame
Date <- seq(as.Date('1990-01-04'), as.Date('2021-01-01'), by = "1 days")
Date <- as.data.frame(Date)

#merge the NZD and JPY Rates
Rates <- merge(JPY_Rates, NZD_Rates, by = "Date", all.y = T, all.x = T)

#merge Rates and Exchange Rates
Rates <- merge(Rates, JPY_NZD, by = "Date", all.y = T, all.x = T)

#Merge with Date vector of all days & interpolate missing weekend values
Rates <- merge(Rates, Date, by = "Date", all.y = T)

#linearly interpolate missing values
Rates[2:22] <-lapply(Rates[2:22], na.approx, na.rm = F)

#fill non-estimateable values at the end (which cannot be linearly approximated)
#with previous values
Rates [2:22] <- fill(Rates[2:22], .direction = "down")

###############################################################################
#Function for Fixed Term / Fixed Rate Carries (Duration in Years or Days)
###############################################################################

fixed_carry <- function(duration, days = F){
  
  if (days == T){
    
    Rate_JPY <- paste("JPY_", duration, "d", sep = "")
    Rate_NZD <- paste("NZD_", duration, "d", sep = "")
    
  } else{
    
    Rate_JPY <- paste("JPY_", duration, "Y", sep = "")
    Rate_NZD <- paste("NZD_", duration, "Y", sep = "")
  }
  
  
  #data set
  Carry <- Rates[c("Date", Rate_JPY, Rate_NZD, "Spot")]

  #calculate implied Forward Rate & add delivery date as t + duration (in years)
  
  if (days == T){
    #calculate forward discount factor
    Carry$discount <- (((1 + Carry[[Rate_JPY]]/100)^(duration/365)) / ((1 + Carry[[Rate_NZD]]/100)^(duration/365)))
    
    Carry$fwd <- Carry$Spot* Carry$discount
    Carry$del_date <- Carry$Date %m+% days(duration)
    
  } else{
    
    #calculate forward discount factor
    Carry$discount <- ((1 + Carry[[Rate_JPY]]/100) / (1 + Carry[[Rate_NZD]]/100))^duration
    
    Carry$fwd <- Carry$Spot * Carry$discount
    Carry$del_date <- Carry$Date %m+% years(duration)
  }
  
  # spot date at delivery
  values <- Carry$Spot[Carry$Date >= min(Carry$del_date, na.rm = T)]
  
  #add spot dates at delivery to date when forward was entered 
  Carry$Spot_at_del <- c(values, rep(NA, nrow(Carry)-length(values)))
  
  return(Carry)
}

##############################################################################
#Function to Create Plots
##############################################################################

#plot spot vs forward at time of delivery
plot_rates <- function(caption, title, data, date){
  
  data <- na.omit(data)
  
  if (date == "Entry"){
    
    x <- substitute(Date)
    
  } else {
    
    x <- substitute(del_date)
  }
  
  plot <- ggplot(data = data, aes(x = eval(x)))+
    geom_line(aes(y = Spot_at_del, color = "Spot at Del"))+
    geom_line(aes(y = fwd, color = "Fwd Rate 
at Inception"))+
    labs(color = "", title = title, caption = caption, y = "Difference",
         x = "Date")+
    theme_classic()
  
  return(plot)
}

#plot difference between forward and spot at delivery
plot_diff <- function(title, caption, data, date){
  
  if (date == "Entry"){
    
    x <- substitute(Date)
    
  } else {
    
    x <- substitute(del_date)
  }
  
  data <- na.omit(data)
  
  plot <- ggplot(data = data, aes(x = eval(x)))+
    geom_line(aes(y = Spot_at_del - fwd))+
    geom_abline(intercept = 0, slope = 0)+
    geom_abline(intercept = mean(Carry$Spot_at_del - Carry$fwd, na.rm = T),
                slope = 0, color = "red")+
    labs(title = title, caption = caption, y = "Difference",
         x = "Date")+
    theme_classic()
  
  return(plot)
}
