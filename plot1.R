## This file implements part 1 of the first assignment for the Data Exploratory Analysis course.
##
## If you want to directly create the PNG file, load this code as a source and call doPlot1ToPng()
##
## The project will automatically download the zipped data, extract it, and remove the zip file
## If you have problems running the code, download the zip from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip to the working directory and extract it, so the working directory contains the file household_power_consumption.txt


## If the file household_power_consumption.txt exists in the directory it returns it as a data.frame. Otherwise, it tries to obtain it from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip. Also, it leaves a copy of the file in the working directory.
##
## WARNING: if the URL does not exist, the file must be obtained manually and copied to the working directory.
## Args:
##  None
## 
## Returns:
##  A data.frame containing with the contents of household_power_consumption.txt
##
## Error handling:
##   None. 
readInputFromUrl <- function() {
  # All 4 plotX.R files contain a copy of this function. It should be externalized to an utils source, but it is replicated in each source file so all of them are self-contained
  temp.zip<-"./file.zip"
  temp.file<-"household_power_consumption.txt"
  if (!file.exists(temp.file)) {
    message("File does not exist in working directory. Recovering it from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip . If this fails, please manually obtain the zip file and uncompress it in the working directory - i.e, add the file household_power_consumption.txt to your working directory and call again to this funtion")
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp.zip, mode="wb")
    unzip(temp.zip, temp.file)
    file.remove(temp.zip)
  } else {
    message("File household_power_consumption.txt exists in the working directory.")
  }
  message("Reading household_power_consumption.txt to return it as a data.frame. It can take some time. It requires around 350MB of memory to load all of the data in memory.")
  data <- read.csv(file = temp.file, sep = ";", stringsAsFactors=FALSE)
  message("Adapting dates, selecting only from  2007-02-01 to 2007-02-02")
  data <- subset(x = data, subset = ((Date=="1/2/2007") | (Date=="2/2/2007")))
  data$DateTime<- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
  data$Date<-as.Date(data$Date,format="%d/%m/%Y")
  data$Time<-as.Date(data$Time, format="%H:%M")
  data$Global_active_power<-as.double(data$Global_active_power)
  data$Global_reactive_power<-as.double(data$Global_reactive_power)
  data$Voltage<-as.double(data$Voltage)
  data$Global_intensity<-as.double(data$Global_intensity)
  data$Sub_metering_1<-as.double(data$Sub_metering_1)
  data$Sub_metering_2<-as.double(data$Sub_metering_2)
  data$Sub_metering_3<-as.double(data$Sub_metering_3)
  
  return(data)
}

## Initializes the environment to English
initialize <- function() {
  Sys.setlocale("LC_TIME", "English")
}

## Reads data via readInputFromUrl and plots the histogram of Global Active Power between 01/02/2007 and 02/02/2007
doPlot1 <- function() {
  initialize();
  data<-readInputFromUrl();
  hist(data$Global_active_power, col="red",main="Global Active Power", xlab = "Global Active Power (kilowatts)")
}
## Calls to doPlot but setting the device to a png file "plot1.png"
doPlot1ToPng <- function() {
  png(file="plot1.png",width = 480, height = 480)
  doPlot1()
  dev.off()
}
