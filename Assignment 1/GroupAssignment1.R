

getwd()
setwd("~/Desktop/CMPT318")

df <- read.table("FirstAssignmentData.txt",header = TRUE, sep = ",",dec = ".")


class(df)

print(is.data.frame(df))


# A: Global_active_power  # B : Global_reactive_power # C: Voltage  # D: Global_intensity 

vec_of_A <- c(df$Global_active_power)

mean_of_A <- mean(vec_of_A)
median_of_A <- median(vec_of_A)
mode_of_A <- mfv(vec_of_A)
sd_of_A <- sd(vec_of_A)
geomean_of_A <- geoMean( vec_of_A [vec_of_A >0]) 




vec_of_B <- c(df$Global_reactive_power)

geomean_of_B <- geoMean( vec_of_B[vec_of_B >0])
mean_of_B <- mean (vec_of_B)
median_of_B <- median(vec_of_B)
mode_of_B <- mfv(vec_of_B)
sd_of_B <- sd(vec_of_B)

vec_of_C <-c(df$Voltage)
vec_of_D <-c(df$Global_intensity)


cor_of_AB<- (cor(vec_of_A, vec_of_B, use = "complete.obs", method = "pearson"))


cor_of_AC<- (cor(vec_of_A, vec_of_C, use = "complete.obs", method = "pearson"))


cor_of_AD<- (cor(vec_of_A, vec_of_D, use = "complete.obs", method = "pearson"))
cor_of_BC<- (cor(vec_of_B, vec_of_C, use = "complete.obs", method = "pearson"))
cor_of_BD<- (cor(vec_of_B, vec_of_D, use = "complete.obs", method = "pearson"))
cor_of_CD<- (cor(vec_of_C, vec_of_D, use = "complete.obs", method = "pearson"))

print(geomean_of_A)
print(geomean_of_B)
print(mean_of_A)
print(mean_of_B)
print(median_of_A)
print(median_of_B)
print(mode_of_A)
print(mode_of_B)
print(sd_of_A)
print(sd_of_B)
print(cor_of_AB)
print(cor_of_AC)
print(cor_of_AD)
print(cor_of_BC)
print(cor_of_BD)
print(cor_of_CD)




df$Date <- as.POSIXlt(df$Date ,format = "%d/%m/%Y")
df$Time <- strptime(df$Time,format = "%H:%M:%S")


weekdays_df <- df[df$Date == "2009-12-01" | df$Date =="2009-12-02" |df$Date == "2009-12-03"| df$Date == "2009-12-04"|df$Date == "2009-12-07" |df$Date == "2009-12-08",] 
weekends_df <- df[df$Date == "2009-12-05" | df$Date == "2009-12-06",]




day_weekdays_df <-weekdays_df[ hour(weekdays_df$Time) >= 6 & hour(weekdays_df$Time) <18 ,]
night_weekdays_df <- weekdays_df[ hour(weekdays_df$Time) >= 18 | hour(weekdays_df$Time) <6 ,]

day_weekends_df <-weekends_df[ hour(weekends_df$Time) >= 6 & hour(weekends_df$Time) <18 ,]
night_weekends_df <- weekends_df[ hour(weekends_df$Time) >= 18 | hour(weekends_df$Time) <6 ,]

#A Global_active_power
print(max(day_weekdays_df$Global_active_power))
print(min(day_weekdays_df$Global_active_power))
print(max(night_weekdays_df$Global_active_power))
print(min(night_weekdays_df$Global_active_power))
print(max(day_weekends_df$Global_active_power))
print(min(day_weekends_df$Global_active_power))
print(max(night_weekends_df$Global_active_power))
print(min(night_weekends_df$Global_active_power))



#B global_reactive_power

print(max(day_weekdays_df$Global_reactive_power))
print(min(day_weekdays_df$Global_reactive_power))
print(max(night_weekdays_df$Global_reactive_power))
print(min(night_weekdays_df$Global_reactive_power))
print(max(day_weekends_df$Global_reactive_power))
print(min(day_weekends_df$Global_reactive_power))
print(max(night_weekends_df$Global_reactive_power))
print(min(night_weekends_df$Global_reactive_power))









