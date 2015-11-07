#loading package to be used in the assigment
library(data.table)  #This package will be helpfull in reading contents from file using fread
library(lubridate)   # Ease the use of date variables
library(dplyr)       # Make working with dataframe very intuitive
library(chron)       # in complement to package lubridate. Use to convert time from character format to Date format

df <- fread("household_power_consumption.txt", na.strings="NA") #  Reading data from file into a dataframe df

DATE1 <- as.Date(strptime("01/02/2007", "%d/%m/%Y")) # Defining the lower treshold date of the dataset
DATE2 <- as.Date(strptime("02/02/2007", "%d/%m/%Y")) # Defining the upper treshold date of the dataset

df <- mutate(df, Date = as.Date(strptime(Date, "%d/%m/%Y"))) # Converting the Date variable from character format to Date format
df <- filter(df,Date >= DATE1 & Date <= DATE2) # filtering the initial dateset
df <- mutate(df, Time = chron(times=Time))     # Converting the Time variable from character format to Date format
df$DateTime <- with(df, as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) # Creating the DateTime variable to be used by concatening 
                                                                                  # Date and Time variables

# Converting variables to numeric to be used in plot
df <- transform(df, Global_active_power =as.numeric(Global_active_power))
df <- transform(df, Global_reactive_power =as.numeric(Global_reactive_power))
df <- transform(df, Voltage =as.numeric(Voltage))
df <- transform(df, Global_intensity =as.numeric(Global_intensity))
df <- transform(df, Sub_metering_1 =as.numeric(Sub_metering_1))
df <- transform(df, Sub_metering_2 =as.numeric(Sub_metering_2))
df <- transform(df, Sub_metering_3 =as.numeric(Sub_metering_3))

df <- mutate(df, weekday = wday(as.Date(DateTime,"%Y-%m-%d %H:%M:%S")+1, label=TRUE, abbr = TRUE)) # creating weekday name to be used in x-axis

at <- seq(1, length(df$weekday), by=1439) # Defining the sequence
labs <- as.character(df$weekday[at])  # Constructing the x label

#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels
png("plot4.png", width = 480, height = 480) 

# 4 plots arranged in 2 rows and 2 columns

par(mfrow=c(2,2)) # create a matrix of 2 rows and 2 columns plots that are filled in by row

# Rows 1 column 1 graphic
{ 
plot(df$DateTime, df$Global_active_power, xlab=' ', ylab='Global Active Power', xaxt = "n", type = "l",lwd=2) # formula for plotting  df$Global_active_power versus df$DateTime
axis(side=1, at=df$DateTime[ at ], labels=labs) # x axis label annotation
} 
# Rows 1 column 2 graphic
{
plot(df$DateTime, df$Voltage, xlab='datetime', ylab='Voltage', xaxt = "n", type = "l",lwd=2) # plotting voltage
axis(side=1, at=df$DateTime[ at ], labels=labs) # x axis label annotation
}
# Rows 2 column 1 graphic
{
plot(df$DateTime, df$Sub_metering_1, xlab=' ', ylab='Energy sub metering', xaxt = "n", type = "l",lwd=2) # plotting Sub_metering_1
par(new=T)    # Adding new plot
plot(df$DateTime, df$Sub_metering_2, xlab=' ', ylab=' ', xaxt = "n", type = "l",lwd=2, col="red",ylim=c(0,max(df$Sub_metering_1))) # plotting Sub_metering_2
par(new=T)   # Adding new plot
plot(df$DateTime, df$Sub_metering_3, xlab=' ', ylab=' ', xaxt = "n", type = "l",lwd=2, col="blue",ylim=c(0,max(df$Sub_metering_1))) # plotting Sub_metering_3
axis(side=1, at=df$DateTime[ at ], labels=labs) # x axis label annotation
}
# Rows 2 column 2 graphic
{
plot(df$DateTime, df$Global_reactive_power, xlab='datetime', ylab='Global_reactive_power', xaxt = "n", type = "l",lwd=2) # plotting Global_reactive_power
axis(side=1, at=df$DateTime[ at ], labels=labs) # x axis label annotation
}

dev.off()