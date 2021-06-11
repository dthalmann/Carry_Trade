###############################################################################
#Load Relevant Packages & set Working Directory
###############################################################################

library(tidyverse)
library(lubridate)
library(zoo)
setwd("~/Desktop/Bachelor 2021/Carry Trade Essay")

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
IRSTCI01JPM156N <-  "IRSTCI01JPM156N.csv"

###############################################################################
#Import & Clean all necessary Data Sets
###############################################################################


#------------------------------------------------------------------------------
#JPY/NZD Exchange Rate (our convention: E = NZD in terms of JPY)
#------------------------------------------------------------------------------


#remove the first three rows with skip = 3
JPY_NZD_1 <- read_csv(NZD_Exchange_Rates_1, skip=3)
JPY_NZD_2 <- read_csv(NZD_Exchange_Rates_2, skip=3)
JPY_NZD_3 <- read_csv(NZD_Exchange_Rates_3, skip=3)

#remove first row
JPY_NZD_1 <- JPY_NZD_1[-1,]
JPY_NZD_2 <- JPY_NZD_2[-1,]
JPY_NZD_3 <- JPY_NZD_3[-1,]

#select columns Unit and NDZ/JPY (here means E=JPY/NZD)
JPY_NZD_1 <- JPY_NZD_1[c("Unit", "NZD/JPY")]
JPY_NZD_2 <- JPY_NZD_2[c("Unit", "NZD/JPY")]
JPY_NZD_3 <- JPY_NZD_3[c("Unit", "NZD/JPY")]

#reformate date
JPY_NZD_1$Unit <- as.Date(JPY_NZD_1$Unit, format = "%d %h %Y")
JPY_NZD_2$Unit <- as.Date(JPY_NZD_2$Unit, format = "%d %h %Y")
JPY_NZD_3$Unit <- as.Date(JPY_NZD_3$Unit, format = "%d-%h-%y")

#combine all three data sets in one
JPY_NZD <- rbind(JPY_NZD_1, JPY_NZD_2, JPY_NZD_3)

#change name "Unit" to "Date" and "NZD/JPY" to "Spot"
names(JPY_NZD) <- c("Date", "Spot")

#formate column "Spot" by assigning numbers
JPY_NZD$Spot <- as.numeric(JPY_NZD$Spot)

#select all data starting from 01/01/1998
JPY_NZD <- JPY_NZD[JPY_NZD$Date >= as.Date("01/01/1998", format = "%d/%m/%Y"),]


#------------------------------------------------------------------------------
#NZD Interest Rates
#------------------------------------------------------------------------------


#remove first row and choose columns 1-10
NZD_Rates_1 <- read_csv(NZD_Rates_1, skip=1)[1:10]
NZD_Rates_2 <- read_csv(NZD_Rates_2, skip=1)[1:10]

#Combine rates for all dates
NZD_Rates <- rbind(NZD_Rates_1, NZD_Rates_2)

#Create numerics for colums 2-10 with direct command lapply (listapply)
NZD_Rates[2:10] <- lapply(NZD_Rates[2:10], as.numeric)

#Remove the second column "official cash rate" and rows 1-3
NZD_Rates <- NZD_Rates[-(1:3),-2]

#reformate date
NZD_Rates$X1 <- as.Date(NZD_Rates$X1, format = "%d %h %Y")

#name all columns
names(NZD_Rates) <- c("Date", "NZD_1d", "NZD_30d",
                      "NZD_60d", "NZD_90d", "NZD_1Y",
                      "NZD_2Y", "NZD_5Y", "NZD_10Y")

#select all Dates larger than 01/01/1998
NZD_Rates <- NZD_Rates[NZD_Rates$Date >= as.Date("01/01/1998", format = "%d/%m/%Y"),]


#------------------------------------------------------------------------------
#JPY Interest Rates
#------------------------------------------------------------------------------  

#load data files
JPY_Rate_3M <- read_csv(JPY3MTD156N) #3month rate
JPY_Rate_1d <- read_csv(IRSTCI01JPM156N) #overnight rate

#rename columns
names(JPY_Rate_3M) <- c("Date", "3M")
names(JPY_Rate_1d) <- c("Date", "1d")

#reformate 3M column by assigning numbers
JPY_Rate_3M$`3M`<- as.numeric(JPY_Rate_3M$`3M`)

#delete first row and formate Dates
JPY_Rates <- read_csv(JPY_Yields, skip = 1) #other rates
JPY_Rates$Date <- as.Date(JPY_Rates$Date, format = "%d/%m/%Y")

#final dataset is called JPY_Rates
#combine datasets, columns cannot just be combined, because dates may vary (There is not a value for every Date in both columns) 
#--> use merge (however, can only combine two columns at the time)
#in both data sets there is a column with dates based on which the datasets are merged (by = "Date")
#1. dataframe = x, 2. dataframe = y
#all.x = T means if x exists and y does not , it just says NA for the missing y-value
#all.y = T means if y exists and y does not , it just says NA for the missing x-value
#this makes sure that we do not eliminate values that are exclusively available in one dataframe or the other
#we only need column 1-10 and column 17

JPY_Rates <- merge(JPY_Rates, JPY_Rate_3M, by ="Date", all.x = T, all.y = T)[c(1:11,17)]

#merge the merged dataset (JPY_Rates) with the JPY_Rate_1d and call it JPY_Rates again
JPY_Rates <- merge(JPY_Rates, JPY_Rate_1d, by ="Date", all.x = T, all.y = T)

#column 10Y is still in characters and needs to be converted into numerics
JPY_Rates$'10Y' <- as.numeric(JPY_Rates$'10Y')

#rename dataset's columns
names(JPY_Rates) <- c("Date", "JPY_1Y", "JPY_2Y", "JPY_3Y",
                      "JPY_4Y", "JPY_5Y", "JPY_6Y",
                      "JPY_7Y", "JPY_8Y", "JPY_9Y",
                      "JPY_10Y", "JPY_90d", "JPY_1d")



#select all data starting from 01/01/1998
JPY_Rates <- JPY_Rates[JPY_Rates$Date >= as.Date("01/01/1998", format = "%d/%m/%Y"),]

#For JPY_Rate_1d there is only a value available about every 30 days
#to approximate the missing values we have to define the first value as the one available after 30 days
#replace the first value, which is missing (NA), by the first value available (21. value)
JPY_Rates$JPY_1d[1] <- JPY_Rates$JPY_1d[21]

#-------------------------------------------------------------------------------------
#combine Interest - and Exchange Rates & add missing Values as Values from Day before
#-------------------------------------------------------------------------------------
#Combine Interest - and Exchange Rates & add missing dates
#problem: interest rates are only reported for weekdays

#create vector "Date" including all Dates, also weekends
Date <- seq(as.Date("1998-01-01"), as.Date("2021-01-01"), by = "1 days")

#convert Vector "Date" into a dataframe (because the merge function only works for two dataframes)
Date <- as.data.frame(Date)

#merge the NZD and JPY Rates 
Rates <- merge(JPY_Rates, NZD_Rates, by ="Date", all.x = T, all.y = T)

#merge Rates and Exchange Rates
Rates <- merge(Rates,JPY_NZD, by ="Date", all.x = T, all.y = T)

#merge "Rates" dataset with Date vector of all days
Rates <- merge(Rates, Date, by ="Date", all.x = T, all.y = T)

#linearly interpolate missing (weekend) values
Rates[2:22] <- lapply(Rates[2:22], na.approx, na.rm = F)

#fill non-estimateable values at the end (which cannot be linearly approximated) with previous values
Rates <- Rates %>% fill(names(Rates)[2:22], .direction = "down")


###############################################################################
#Function for Fixed Term / Fixed Rate Carries (Duration in Years or Days)
###############################################################################

carry <- function(duration, days = F){
  
  if (days == T){
    
    Rate_JPY <- paste("JPY_", duration, "d", sep = "")
    Rate_NZD <- paste("NZD_", duration, "d", sep = "")
    
  } else{
    
    Rate_JPY <- paste("JPY_", duration, "Y", sep = "")
    Rate_NZD <- paste("NZD_", duration, "Y", sep = "")
  }
  
  
  #name the final dataset Carry and define its columns
  Carry <- Rates[c("Date", Rate_JPY, Rate_NZD, "Spot")]
  
  
  #calculate implied Forward Rate & add delivery date + duration (in days or years respectively)
  
  if (days == T){
    #calculate forward discount factor for interest during the year
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
plot_rates <- function(caption = "", title = "", data, date ="Delivery"){
  
  data <- na.omit(data) #eliminate NA due to missing delivery date spot rate
  
  if (date == "Entry"){
    
    x <- substitute(Date) #Date refers to the start date of a Carry (refers to the past)
    
  }else {
    
    x <- substitute(del_date)
  }
  
  plot <- ggplot(data = data, aes(x = eval(x)))+
    geom_line(aes(y = Spot_at_del, color = "Spot Rate at Del"))+ 
    geom_line(aes(y = fwd, color = "Implied Fwd Rate"))+
    labs(color = "", title = title, caption = caption, y = "JPY/NZD",
         x = "Year")+ 
    scale_x_date(breaks = function(x) seq.Date(from = ymd("2000-01-01"), to = ymd("2020-01-01"), 
                                               by = "5 years"), date_labels = "%Y")+
    theme_minimal()+ 
    theme(axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  plot.title = element_text(size=12),
                  plot.caption =element_text(size=10),
                  legend.text = element_text(size=9),
                  legend.position = c(0.9,0.9),
                  legend.background = element_rect(fill="white",
                       size=0.2, linetype = "blank"),
                  legend.title = element_blank())
  
  return(plot)
}

#plot difference between forward and spot at delivery
plot_diff <- function(title = "", caption = "", data, date = "Delivery"){
  
  if (date == "Entry"){
    
    x <- substitute(Date)
    
  } else {
    
    x <- substitute(del_date)
  }
  
  data <- na.omit(data)
  
  plot <- ggplot(data = data, aes(x = eval(x)))+
    geom_line(aes(y = Spot_at_del - fwd))+
    geom_abline(intercept = 0, slope = 0)+
    geom_abline(intercept = mean(data$Spot_at_del - data$fwd, na.rm = T),
                slope = 0, color = "red")+
    labs(title = title, caption = caption, y = "Difference [spot at del - implied fwd]",
         x = "Year")+ 
    scale_x_date(breaks = function(x) seq.Date(from = ymd("2000-01-01"), to = ymd("2020-01-01"), 
                                               by = "5 years"), date_labels = "%Y")+
    theme_minimal()+ 
    theme(axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  plot.title = element_text(size=12),
                  plot.caption =element_text(size=10),
                  legend.text = element_text(size=10))
  
  return(plot)
}

