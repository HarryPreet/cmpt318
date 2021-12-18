library("depmixS4")
library(ggplot2)


df <- read.table("Dataset.txt", header = TRUE, sep = ",", dec = ".")

df$Date <- as.POSIXct(df$Date, format = "%d/%m/%Y")
df$Day <- as.POSIXlt(df$Date)$wday
df$Time <- strptime(df$Time, format = "%H:%M:%S")
sundays = df[df$Day == "0",]
sundays_morning = sundays[hour(sundays$Time)>=6 & hour(sundays$Time)<8,]
sundays_morning$Date <- as.POSIXct(sundays_morning$Date, format = "%d/%m/%Y")
#ggplot(sundays_morning, aes(x=Date, y=Global_active_power))+geom_point(alpha=1,col = "red",size=1) +geom_smooth()


v<-vector()
set.seed(1)
for(states in 2:50){	
 print(states)
 mod<- depmix(response = sundays_morning$Global_active_power ~ 1, data = sundays_morning, nstates = states,ntimes=c(120,120,120,120,120,
																		   120,120,120,120,120,
																		   120,120,120,120,120,	
																	         120,120,120,120,120,
																		   120,120,120,120,120,
																		   120,120,120,120,120,
																		   120,120,120,120,120,
																		   120,120,120,120,120,
																		   120,120,120,120,120,	
																		   120,120,120,120,120,	
																		   120,120))																			
																
 fm <- fit(mod)
 print(fm)
 v<-c(v,BIC(fm))
}

print(v)

plot(2:50,v,ty="b")







