library(depmixS4)
library(ggplot2)
library(lubridate)
library(dplyr)
library(hydroTSM)

#################################################

                    #Task 1

#################################################

#Used to reproduce results
set.seed(1)

#Read data into dataframe
df <- read.table("Dataset.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
df$Date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
df$Day <- as.POSIXlt(df$Date)$wday

#Set Time structure using POSIXct
df$Time <- as.POSIXlt(df$Time, format = "%H:%M:%S")

#Create sundays_morning dataframe that includes all rows of df == Sunday (09:00 to 10:00)
sundays = df[df$Day == "0",]
sundays_morning = sundays[hour(sundays$Time)>=9 & hour(sundays$Time)<10,]

#Set Date & Time structure of sundays_morning dataframe using POSIXct so it can be plotted using ggplot
sundays_morning$Date <- as.POSIXct(sundays_morning$Date, format = "%d/%m/%Y")

#Plot the Global_active_power of the 24 hour window Sundays for the whole year
ggplot(sundays_morning, aes(x = Date, y = Global_active_power))+geom_point()+geom_smooth()


#HMM Modelling using library depmixs4

#Model 1: nstates = 2                                                                                            
mod2 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 2, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm2 <- fit(mod2)

summary(fm2)
print(fm2)

#Model 2: nstates = 3                                                                                                  
mod3 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 3, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm3 <- fit(mod3)

summary(fm3)
print(fm3)

#Model 3: nstates = 4                                                                                                 
mod4 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 4, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm4 <- fit(mod4)

summary(fm4)
print(fm4)

#Model 4: nstates = 5                                                                                                  
mod5 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 5, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm5 <- fit(mod5)

summary(fm5)
print(fm5)

#Model 5: nstates = 6                                                                                                  
mod6 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 6, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm6 <- fit(mod6)

summary(fm6)
print(fm6)

#Model 6: nstates = 7                                                                                                  
mod7 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 7, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm7 <- fit(mod7)

summary(fm7)
print(fm7)

#Model 7: nstates = 8                                                                                                  
mod8 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 8, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm8 <- fit(mod8)

summary(fm8)
print(fm8)

#Model 8: nstates = 9                                                                                                  
mod9 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 9, ntimes = c(60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60,60,60,60,
                                                                                                   60,60))
fm9 <- fit(mod9)

summary(fm9)
print(fm9)

#Model 9: nstates = 10                                                                                                  
mod10 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 10, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm10 <- fit(mod10)

summary(fm10)
print(fm10)

#Model 10: nstates = 11                                                                                                  
mod11 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 11, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm11 <- fit(mod11)

summary(fm11)
print(fm11)

#Model 11: nstates = 12                                                                                                  
mod12 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 12, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm12 <- fit(mod12)

summary(fm12)
print(fm12)

#Model 12: nstates = 13                                                                                                  
mod13 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 13, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm13 <- fit(mod13)

summary(fm13)
print(fm13)

#Model 13: nstates = 14                                                                                                  
mod14 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 14, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm14 <- fit(mod14)

summary(fm14)
print(fm14)

#Model 14: nstates = 15                                                                                                  
mod15 <- depmix(response = Global_active_power ~ 1, data = sundays_morning, nstates = 15, ntimes = c(60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60,60,60,60,
                                                                                                     60,60))
fm15 <- fit(mod15)

summary(fm15)
print(fm15)

#Plot nstates (x-axis) vs. BIC value (y-axis)
plot(2:15,c(BIC(fm2),BIC(fm3),BIC(fm4),BIC(fm5),BIC(fm6),BIC(fm7),BIC(fm8),BIC(fm9),BIC(fm10),BIC(fm11),BIC(fm12),BIC(fm13),BIC(fm14),BIC(fm15)),ty="b")

#Focused plot on local minimum of the nstates (x-axis) vs. BIC value (y-axis)
plot(8:12, c(BIC(fm8),BIC(fm9),BIC(fm10),BIC(fm11),BIC(fm12)),ty="b")


#################################################

                    #Task 2

#################################################
#Part A
#################################################

include.packages("reshape")
include.packages("hydroTSM")

#Create new column, Minute, in sundays_morning dataframe 
sundays_morning$Minute = format(as.POSIXlt(sundays_morning$Time, format="%H:%M:%S"),"%H:%M")

#Create new dataframe that contains the average of each data point (Global_active_power) through Sunday
avgSundays_morning <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Minute), mean)

#Modify to more appropriate column names
names(avgSundays_morning)[1] <- "Time"
names(avgSundays_morning)[2] <- "avgGlobal_active_power"

#Plot the Time (x-axis) vs avgGlobal_active_power (y-axis)
ggplot(avgSundays_morning, aes(x = Time, y = avgGlobal_active_power, group=1))+geom_point()+geom_smooth()


#################################################
#Part B
#################################################

#Create columns that separate Sundays based on week number, month and season
sundays_morning$Week = format(as.POSIXlt(sundays_morning$Date, format = "%Y-%m-%d"), "%W")
sundays_morning$Month = format(as.POSIXlt(sundays_morning$Date, format = "%Y-%m-%d"), "%m")
sundays_morning$Season = time2season(sundays_morning$Date, out.fmt="seasons", type="default")


#Minimum Global_active_power based on week number of Sundays
weekMin <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Week), min)
names(weekMin)[1] <- "Week Number"
names(weekMin)[2] <- "minGlobal_active_power"
weekMin

#Maximum Global_active_power based on week number of Sundays
weekMax <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Week), max)
names(weekMax)[1] <- "Week Number"
names(weekMax)[2] <- "maxGlobal_active_power"
weekMax

#Average Global_active_power based on week number of Sundays
weekAvg <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Week), mean)
names(weekAvg)[1] <- "Week Number"
names(weekAvg)[2] <- "avgGlobal_active_power"
weekAvg

#Standard deviation of Global_active_power based on week number of Sundays
weekSD <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Week), sd)
names(weekSD)[1] <- "Week Number"
names(weekSD)[2] <- "sdGlobal_active_power"
weekSD

#Linear Regression Model of Week vs. Global_active_power
weekRegression <- lm(Week ~ Global_active_power, sundays_morning)
weekRegression


#Minimum Global_active_power based on month of the Sundays
monthMin <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Month), min)
names(monthMin)[1] <- "Month"
names(monthMin)[2] <- "minGlobal_active_power"
monthMin

#Maximum Global_active_power based on month of the Sundays
monthMax <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Month), max)
names(monthMax)[1] <- "Month"
names(monthMax)[2] <- "maxGlobal_active_power"
monthMax

#Average Global_active_power based on month of the Sundays
monthAvg <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Month), mean)
names(monthAvg)[1] <- "Month"
names(monthAvg)[2] <- "avgGlobal_active_power"
monthAvg

#Standard deviation of Global_active_power based on month of the Sundays
monthSD <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Month), sd)
names(monthSD)[1] <- "Month"
names(monthSD)[2] <- "sdGlobal_active_power"
monthSD

#Linear Regression Model of Month vs. Global_active_power
monthRegression <- lm(Month ~ Global_active_power, sundays_morning)


#Minimum Global_active_power based on the Season the Sundays are on
seasonMin <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Season), min)
names(seasonMin)[1] <- "Season"
names(seasonMin)[2] <- "minGlobal_active_power"
seasonMin

#Maximum Global_active_power based on the Season the Sundays are on
seasonMax <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Season), max)
names(seasonMax)[1] <- "Season"
names(seasonMax)[2] <- "maxGlobal_active_power"
seasonMax

#Average Global_active_power based on the Season the Sundays are on
seasonAvg <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Season), mean)
names(seasonMax)[1] <- "Season"
names(seasonMax)[2] <- "avgGlobal_active_power"
seasonMax

#Standard deviation of Global_active_power based on the Season the Sundays are on
seasonSD <- aggregate(sundays_morning$Global_active_power, by = list(sundays_morning$Season), sd)
names(seasonSD)[1] <- "Season"
names(seasonSD)[2] <- "sdGlobal_active_power"
seasonSD