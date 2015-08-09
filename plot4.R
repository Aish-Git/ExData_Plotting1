# Aish Varadhan
# Aug 9, 2015
# Exploratory Data Analysis - Project 1 on Plotting


# Source data from the file - assumes file is already downloaded to working folder

plot4 <- function(file_path = getwd(), file_name = "household_power_consumption.txt") {
  
  # Load all required libraries
  library(dplyr)
  library(lubridate)
  library(sqldf)
  
  # As we need to filter rows for specific dates when we read, use sqldf - set file pointer
  file_ptr <- file(paste(file_path, file_name, sep = "\\"))
  
  # Set file attributes - interpret values with a ? as N/A based on file description
  attr(file_ptr,"file.format") <- list(sep=";",header=TRUE)
  
  # read only those values where Date = '1/2/2007' or '2/2/22007'
  file_dat <- sqldf("select * from file_ptr where Date= '1/2/2007' or Date='2/2/2007'") 
  
  # Convert Date to R date format using lubridate and as.date() to get rid of UTC that lubridate leaves behind
  file_dat<-mutate(file_dat,Date=as.Date(dmy(file_dat$Date)))
  
  # Close file pointer
  close(file_ptr)
  
  # Open output device as png
  png(filename = "plot4.png",width = 480, height = 480)
  
  # Open 2x2 multi plot window
  par(mfrow=c(2,2))
  par(mar=c(4,4,1,1))
  
  # Plot Global ACtive Power from plot 2
  plot(file_dat$Global_active_power,type = "l",xaxt="n", ylab= "Global Active Power", xlab = "")
  axis(side = 1,at = c(1,1440,2880),labels = c("Thu","Fri","Sat"))   # Close output device
  
  # Plot voltage
  plot(file_dat$Voltage,type = "l",xaxt="n", ylab= "Voltage", xlab = "datetime")
  axis(side = 1,at = c(1,1440,2880),labels = c("Thu","Fri","Sat"))   # Close output device
  
  # Plot sub_metering from plot3
  plot(file_dat$Sub_metering_1,type = "l",ylab="Energy sub metering",xlab="",xaxt="n")
  lines(file_dat$Sub_metering_2,type = "l",col = "red")
  lines(file_dat$Sub_metering_3,type = "l",col = "blue")
  axis(side = 1,at = c(1,1440,2880),labels = c("Thu","Fri","Sat"))   
  legend('topright',c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = 1,col=c("black","red","blue"),bty = "n")
  
  # Plot global reactive power
  plot(file_dat$Global_reactive_power,type = "l",xaxt="n", ylab= "Global_reactive_power", xlab = "datetime")
  axis(side = 1,at = c(1,1440,2880),labels = c("Thu","Fri","Sat"))   # Close output device
  
  # Close output device
  dev.off()
}