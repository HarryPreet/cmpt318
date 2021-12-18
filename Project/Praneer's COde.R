library("ggplot2")
library("lubridate")
library("dplyr")
library("EnvStats")
library("modeest")

######################################################################

                    #Phase 1 - Characteristic 1

######################################################################

#Read TRAIN data into dataframe
df <- read.table("TrainData.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
df$Day <- as.POSIXlt(df$Date)$wday

#Set Time structure using POSIXlt
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")

mod_df <- df[format(df$Date,'%Y') != "2006" & format(df$Date,'%Y') != "2009", ]

for(i in 1:ncol(mod_df)) {
  mod_df[is.na(mod_df[,i]), i] <- mean(mod_df[,i], na.rm = TRUE)
}

#AVERAGE GLOBAL ACTIVE POWER OF A GIVEN DATE 
dateAvg <- aggregate(mod_df$Global_active_power, by = list(mod_df$Date), mean)
colnames(dateAvg) <- c("Date", "avgGlobal_active_power")

#PLOTTING ENTIRE THREE YEARS OF AVERAGE GAP DATEWISE
ggplot(dateAvg, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR THE ENTIRE WEEK IN THREE YEARS
dateAvg$Day<- as.POSIXlt(dateAvg$Date)$wday
weekAvg<-aggregate(dateAvg$avgGlobal_active_power, by = list(dateAvg$Day), mean)
colnames(weekAvg) <- c("Day", "avgGlobal_active_power")
ggplot(weekAvg, aes(x = Day, y = avgGlobal_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg<-aggregate(mod_df$Global_active_power, by = list(mod_df$Time), mean)
head(timeAvg)
colnames(timeAvg) <- c("Time", "avgGlobal_active_power")
ggplot(timeAvg, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()+geom_line()


#MORNINGS (06:00 - 12:00)
morning <- mod_df[hour(mod_df$Time) >= 6 & hour(mod_df$Time) < 12,]

#MORNING AVERAGED FOR EACH DATE
morning_avgDate<-aggregate(morning$Global_active_power, by = list(morning$Date), mean)
colnames(morning_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(morning_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#NIGHT (18:00 - 24:00)
night <- mod_df[hour(mod_df$Time) >= 18 & hour(mod_df$Time) < 24,]

#NIGHT AVERAGED FOR EACH DATE
night_avgDate<-aggregate(night$Global_active_power, by = list(night$Date), mean)
colnames(night_avgDate) <- c("Date", "avgGlobal_active_power")
ggplot(night_avgDate, aes(x = Date, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKDAYS MORNING
weekdays_morning<-morning[morning$Day !=6 & morning$Day !=5,]

#WEEKDAYS MORNING AVERAGED FOR EACH MINUTE
weekdays_morning_avgTime<-aggregate(weekdays_morning$Global_active_power, by = list(weekdays_morning$Time), mean)
colnames(weekdays_morning_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekdays_morning_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKDAYS NIGHT
weekdays_night<-night[night$Day !=6 & night$Day !=5,]

#WEEKDAYS NIGHT AVERAGED FOR EACH MINUTE
weekdays_night_avgTime<-aggregate(weekdays_night$Global_active_power, by = list(weekdays_night$Time), mean)
colnames(weekdays_night_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekdays_night_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKENDS MORNING
weekends_morning<-morning[morning$Day ==6 | morning$Day ==5,]

#WEEKENDS MORNING AVERAGED FOR EACH MINUTE
weekends_morning_avgTime<-aggregate(weekends_morning$Global_active_power, by = list(weekends_morning$Time), mean)
colnames(weekends_morning_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekends_morning_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()


#WEEKENDS NIGHT
weekends_night<-night[night$Day ==6 | night$Day ==5,]

#WEEKENDS NIGHT AVERAGED FOR EACH MINUTE
weekends_night_avgTime<-aggregate(weekends_night$Global_active_power, by = list(weekends_night$Time), mean)
colnames(weekends_night_avgTime) <- c("Time", "avgGlobal_active_power")
ggplot(weekends_night_avgTime, aes(x = Time, y = avgGlobal_active_power))+geom_point()+geom_smooth()




#CALCULATING MEAN, STANDARD DEVIATION, MIN, MAX OF GLOBAL ACTIVE POWER FOR
#1. WEEKDAY MORNINGS
#2. WEEKDAY NIGHTS
#3. WEEKEND MORNINGS
#4. WEEKEND NIGHTS 

#WEEKDAY MORNINGS
mean(weekdays_morning$Global_active_power)
sqrt(var(weekdays_morning$Global_active_power))
min(weekdays_morning$Global_active_power)
max(weekdays_morning$Global_active_power)

#WEEKDAY NIGHTS
mean(weekdays_night$Global_active_power)
sqrt(var(weekdays_night$Global_active_power))
min(weekdays_night$Global_active_power)
max(weekdays_night$Global_active_power)

#WEEKENDS MORNINGS
mean(weekends_morning$Global_active_power)
sqrt(var(weekends_morning$Global_active_power))
min(weekends_morning$Global_active_power)
max(weekends_morning$Global_active_power)

#WEEKENDS NIGHTS
mean(weekends_night$Global_active_power)
sqrt(var(weekends_night$Global_active_power))
min(weekends_night$Global_active_power)
max(weekends_night$Global_active_power)


#Plotting Average Data to Visualise Normality
plot(weekdays_morning_avgTime$avgGlobal_active_power)
plot(weekdays_night_avgTime$avgGlobal_active_power)
plot(weekends_morning_avgTime$avgGlobal_active_power)
plot(weekends_night_avgTime$avgGlobal_active_power)

plot(weekdays_morning$Global_active_power)
plot(weekdays_night$Global_active_power)
plot(weekends_morning$Global_active_power)
plot(weekends_night$Global_active_power)



######################################################################

                  #Phase 1 - Characteristic 2

######################################################################

#FOR
#1. WEEKDAYS_MORNING
#2. wEEKDAYS_NIGHT
#3. WEEKENDS_MORNING
#4. WEEKENDS_NIGHT

#CORRELATION BETWEEN GLOBAL_ACTVE_POWER AND GLOBAL_INTENSITY
cor(weekdays_morning$Global_active_power, weekdays_morning$Global_intensity, method = "pearson")
cor(weekdays_night$Global_active_power, weekdays_night$Global_intensity, method = "pearson")
cor(weekends_morning$Global_active_power, weekends_morning$Global_intensity, method = "pearson")
cor(weekends_night$Global_active_power, weekends_night$Global_intensity, method = "pearson")

#CORRELATION BETWEEN GLOBAL_ACTIVE_POWER AND GLOBAL_REACTIVE_POWER
cor(weekdays_morning$Global_active_power, weekdays_morning$Global_reactive_power, method = "pearson")
cor(weekdays_night$Global_active_power, weekdays_night$Global_reactive_power, method = "pearson")
cor(weekends_morning$Global_active_power, weekends_morning$Global_reactive_power, method = "pearson")
cor(weekends_night$Global_active_power, weekends_night$Global_reactive_power, method = "pearson")

#CORRELATION BETWEEN GLOBAL_ACTIVE_POWER AND VOLTAGE
cor(weekdays_morning$Global_active_power, weekdays_morning$Voltage, method = "pearson")
cor(weekdays_night$Global_active_power, weekdays_night$Voltage, method = "pearson")
cor(weekends_morning$Global_active_power, weekends_morning$Voltage, method = "pearson")
cor(weekends_night$Global_active_power, weekends_night$Voltage, method = "pearson")

#CORRELATION BETWEEN GLOBAL_REACTIVE_POWER AND GLOBAL_INTENSITY
cor(weekdays_morning$Global_reactive_power, weekdays_morning$Global_intensity, method = "pearson")
cor(weekdays_night$Global_reactive_power, weekdays_night$Global_intensity, method = "pearson")
cor(weekends_morning$Global_reactive_power, weekends_morning$Global_intensity, method = "pearson")
cor(weekends_night$Global_reactive_power, weekends_night$Global_intensity, method = "pearson")

#CORRELATION BETWEEN GLOBAL_REACTIVE_POWER AND VOLTAGE
cor(weekdays_morning$Global_reactive_power, weekdays_morning$Voltage, method = "pearson")
cor(weekdays_night$Global_reactive_power, weekdays_night$Voltage, method = "pearson")
cor(weekends_morning$Global_reactive_power, weekends_morning$Voltage, method = "pearson")
cor(weekends_night$Global_reactive_power, weekends_night$Voltage, method = "pearson")

#CORRELATION BETWEEN GLOBAL_INTENSITY AND VOLTAGE
cor(weekdays_morning$Global_intensity, weekdays_morning$Voltage, method = "pearson")
cor(weekdays_night$Global_intensity, weekdays_night$Voltage, method = "pearson")
cor(weekends_morning$Global_intensity, weekends_morning$Voltage, method = "pearson")
cor(weekends_night$Global_intensity, weekends_night$Voltage, method = "pearson")




######################################################################

                  #Phase 2 - Approach 2

######################################################################

x = rep(360, 523)

#Model 1: nstates = 2
mod2 <- depmix(Global_active_power~1, data = weekdays_morning, nstates = 8, ntimes = x, family=gaussian())
fm2 <- fit(mod2)

summary(fm2)
print(fm2)
