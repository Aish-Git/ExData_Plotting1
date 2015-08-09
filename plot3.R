# Aish Varadhan
# Aug 9, 2015
# Exploratory Data Analysis - Project 1 on Plotting


# Source data from the file - assumes file is already downloaded to working folder

plot3 <- function(file_path = getwd(), file_name = "household_power_consumption.txt") {
  
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
  png(filename = "plot3.png",width = 480, height = 480)
   
  # Plot sub_metering*, custom x-axis labels instead of index (points) - plot1, then 2 and 3 as lines using line function
  
  #xaxt = "n" doesn't label the axis
  plot(file_dat$Sub_metering_1,type = "l",ylab="Energy sub metering",xlab="",xaxt="n")
  
  # plot the others as lines using the lines function to sumperimpose/overlay
  lines(file_dat$Sub_metering_2,type = "l",col = "red")
  lines(file_dat$Sub_metering_3,type = "l",col = "blue")
  
  #specify tick marks at 1, 1440, 2880 (start, mid and end) with labels as Thu, Fri, sat
  axis(side = 1,at = c(1,1440,2880),labels = c("Thu","Fri","Sat"))   
  
  # add a legend
  legend('topright',c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("black","red","blue"))
  
  # Close output device
  dev.off()
}