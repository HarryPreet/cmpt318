library("ggplot2")
library("lubridate")
library("dplyr")
library("EnvStats")
library("modeest")

#Used to reproduce results
set.seed(1)

#Read Test data into dataframe
df_TEST <- read.table("TemporaryTestData.txt", header = TRUE, sep = ",", dec = ".")

#Set Date structure using POSIXlt
df_TEST$Date <- as.POSIXct(df_TEST$Date, format = "%d/%m/%Y")

#Create new column that displays weekday (0:6 where 0 == Sunday)
df_TEST$Day <- as.POSIXlt(df_TEST$Date)$wday

#Set Time structure using POSIXlt
df_TEST$Time <- as.POSIXct(df_TEST$Time, format = "%H:%M:%S")

install.packages("ggplot2")
library("ggplot2")

mod_df <- df[format(df$Date,'%Y') != "2006" & format(df$Date,'%Y') != "2009", ]

for(i in 1:ncol(mod_df)) {
  mod_df[is.na(mod_df[,i]), i] <- mean(mod_df[,i], na.rm = TRUE)
}

#AVERAGE GLOBAL ACTIVE POWER OF A GIVEN DATE 
dateAvg_TEST <- aggregate(mod_df_TEST$Global_active_power, by = list(mod_df_TEST$Date), mean)
colnames(dateAvg_TEST) <- c("Date", "Global_active_power")

#PLOTTING ENTIRE THREE YEARS OF AVERAGE GAP DATEWISE
ggplot(dateAvg_TEST, aes(x = Date, y = Global_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR THE ENTIRE WEEK IN THREE YEARS
dateAvg_TEST$Day<- as.POSIXlt(dateAvg_TEST$Date)$wday
weekAvg_TEST<-aggregate(dateAvg_TEST$Global_active_power, by = list(dateAvg_TEST$Day), mean)
colnames(weekAvg_TEST) <- c("Day", "Global_active_power")
ggplot(weekAvg_TEST, aes(x = Day, y = Global_active_power))+geom_point()+geom_smooth()

#PLOTTING AVERAGE FOR EACH MINUTE IN THREE YEARS
timeAvg_TEST<-aggregate(mod_df_TEST$Global_active_power, by = list(mod_df_TEST$Time), mean)
head(timeAvg_TEST)
colnames(timeAvg_TEST) <- c("Time", "Global_active_power")
ggplot(timeAvg_TEST, aes(x = Time, y = Global_active_power))+geom_point()+geom_smooth()+geom_line()

#WEEKENDS
weekends_TEST<-mod_df_TEST[mod_df_TEST$Day ==6 | mod_df_TEST$Day ==5,]

#WEEKDAYS
weekdays_TEST<-mod_df_TEST[mod_df_TEST$Day !=6 & mod_df_TEST$Day !=5,]

#WEEKDAYS MORNING
weekdays_TEST_morning<-weekdays_TEST[hour(weekdays_TEST$Time)>=6 & hour(weekdays_TEST$Time)<12,]

#WEEKDAYS MORNING AVERAGED FOR EACH MINUTE
weekdays_TEST_morning_average<-aggregate(weekdays_TEST_morning$Global_active_power, by = list(weekdays_TEST_morning$Time), mean)
colnames(weekdays_TEST_morning_average) <- c("Time", "Global_active_power")
ggplot(weekdays_TEST_morning_average, aes(x = Time, y = Global_active_power))+geom_point()+geom_smooth()

#WEEKDAYS NIGHT
weekdays_TEST_night<-weekdays_TEST[hour(weekdays_TEST$Time)>=18 & hour(weekdays_TEST$Time)<24,]

#WEEKDAYS NIGHT AVERAGED FOR EACH MINUTE
weekdays_TEST_night_average<-aggregate(weekdays_TEST_night$Global_active_power, by = list(weekdays_TEST_night$Time), mean)
colnames(weekdays_TEST_night_average) <- c("Time", "Global_active_power")
ggplot(weekdays_TEST_night_average, aes(x = Time,             y = Global_active_power))+geom_point()+geom_smooth()

#WEEKENDS MORNING
weekends_TEST_morning<-weekends_TEST[hour(weekends_TEST$Time)>=6 & hour(weekends_TEST$Time)<12,]

#WEEKENDS MORNING AVERAGED FOR EACH MINUTE
weekends_TEST_morning_average<-aggregate(weekends_TEST_morning$Global_active_power, by = list(weekends_TEST_morning$Time), mean)
colnames(weekends_TEST_morning_average) <- c("Time", "Global_active_power")
ggplot(weekends_TEST_morning_average, aes(x = Time, y = Global_active_power))+geom_point()+geom_smooth()

#WEEKENDS NIGHT
weekends_TEST_night<-weekends_TEST[hour(weekends_TEST$Time)>=18 & hour(weekends_TEST$Time)<24,]
#WEEKENDS NIGHT AVERAGED FOR EACH MINUTE
weekends_TEST_night_average<-aggregate(weekends_TEST_night$Global_active_power, by = list(weekends_TEST_night$Time), mean)
colnames(weekends_TEST_night_average) <- c("Time", "Global_active_power")
ggplot(weekends_TEST_night_average, aes(x = Time, y = Global_active_power))+geom_point()+geom_smooth()


#CALCULATING MEAN AND STANDARD DEVIATION OF GLOBAL ACTIVE POWER FOR
#1. WEEKDAY MORNINGS
#2. WEEKDAY NIGHTS
#3. WEEKEND MORNINGS
#4. WEEKEND NIGHTS 


#WEEKDAY MORNINGS
mean(weekdays_TEST_morning$Global_active_power)
sqrt(var(weekdays_TEST_morning$Global_active_power))

#WEEKDAY NIGHTS
mean(weekdays_TEST_night$Global_active_power)
sqrt(var(weekdays_TEST_night$Global_active_power))

#WEEKENDS MORNINGS
mean(weekends_TEST_morning$Global_active_power)
sqrt(var(weekends_TEST_morning$Global_active_power))

#WEEKENDS NIGHTS
mean(weekends_TEST_night$Global_active_power)
sqrt(var(weekends_TEST_night$Global_active_power))


#Plotting Average Data to Visualise Normality

plot(weekdays_TEST_morning_average$Global_active_power)
plot(weekdays_TEST_night_average$Global_active_power)
plot(weekends_TEST_morning_average$Global_active_power)
plot(weekends_TEST_night_average$Global_active_power)

plot(weekdays_TEST_morning$Global_active_power)
plot(weekdays_TEST_night$Global_active_power)
plot(weekends_TEST_morning$Global_active_power)
plot(weekends_TEST_night$Global_active_power)
















