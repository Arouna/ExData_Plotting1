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

# Plot 1
# Plotting the hitograme  of the first of the variable 

#Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels
png("plot1.png", width = 480, height = 480) 

# make the histogram
hist(df$Global_active_power, main = "Global Active Power", xlab='Global Active Power (kilowatts)', ylab="Frequency",col="red") 

dev.off()