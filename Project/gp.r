library(depmixS4)
library(ggplot2)
library(lubridate)
library(dplyr)
library(hydroTSM)

getwd()
setwd("~/Desktop/CMPT 318")
set.seed(1)
df <- read.table("TrainData.txt",header = TRUE, sep = ",",dec = ".")
class(df)



df$Time <- as.POSIXlt(df$Time, format = "%H:%M:%S")
df$Date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")

# Basic check:  Calculating and comparing the mean and standard deviation.


day_df <-df[ hour(df$Time) >= 6 & hour(df$Time) <=18 ,]
night_df <- df[ hour(df$Time) > 18 | hour(df$Time) <6 ,]



#Set Date & Time structure of day and night dataframe  for ggplot
day_df$Date <- as.POSIXct(day_df$Date, format = "%d/%m/%Y")
night_df$Date <- as.POSIXct(night_df$Date, format = "%d/%m/%Y")



# plot the day and night global power
##ggplot(night_df, aes(x = Date, y = Global_active_power))+geom_point()+geom_smooth()
##ggplot(day_df, aes(x = Date, y = Global_active_power))+geom_point()+geom_smooth()


#mean of each data point (Global_active_power) through day time period for each days
mean_day <- aggregate(day_df$Global_active_power, by = list(day_df$Date), mean)
names(mean_day)[1] <- "Time"
names(mean_day)[2] <- "mean_daytime"
##ggplot(mean_day, aes(x = Time, y = mean_daytime, group=1))+geom_point()+geom_smooth()


#mean of~ night time
mean_night <- aggregate(night_df$Global_active_power, by = list(night_df$Date), mean)
names(mean_night)[1] <- "Time"
names(mean_night)[2] <- "mean_nighttime"
##ggplot(mean_night, aes(x = Time, y = mean_nighttime, group=1))+geom_point()+geom_smooth()



#standard deviation of each data point (Global_active_power) through day time period for each days
sd_day <- aggregate(day_df$Global_active_power, by = list(day_df$Date), sd)
names(sd_day)[1] <- "Time"
names(sd_day)[2] <- "sd_daytime"
##ggplot(sd_day, aes(x = Time, y = sd_daytime, group=1))+geom_point()+geom_smooth()


#standard deviation of ~ night time
sd_night <- aggregate(night_df$Global_active_power, by = list(night_df$Date), sd)
names(sd_night)[1] <- "Time"
names(sd_night)[2] <- "sd_nighttime"
##ggplot(sd_night, aes(x = Time, y = sd_nighttime, group=1))+geom_point()+geom_smooth()








