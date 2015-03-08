## This file implements part 4 of the first assignment for the Data Exploratory Analysis course.
##
## If you want to directly create the PNG file, load this code as a source and call doPlot4ToPng()
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

doPlot4.1 <- function(data) {
  # Copied from exercise 1. Not externalized to a function to re-use it to avoid grading problems - i.e, to make the file self-contained
  with(data, plot(DateTime,Global_active_power,type="n",xlab="",ylab = "Global Active Power"))
  with(data,lines(DateTime, Global_active_power))
}
doPlot4.2 <- function(data) {
  with(data, plot(DateTime,Voltage,type="n",xlab="datetime",ylab = "Voltage"))
  with(data,lines(DateTime, Voltage))
  
}
doPlot4.3 <- function(data) {
  # Copied from exercise 3. Not externalized to a function to re-use it to avoid grading problems - i.e, to make the file self-contained
  # build base plot
  with(data, plot(DateTime, Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
  # add submeters
  with(data,lines(DateTime, Sub_metering_1, col="black"))
  with(data,lines(DateTime, Sub_metering_2, col="red"))
  with(data,lines(DateTime, Sub_metering_3, col="blue"))
  # add legend
  legend("topright",lty=c("solid","solid","solid"),col=c("black","red","blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),bty="n")
}
doPlot4.4 <- function(data) {
  with(data, plot(DateTime,Global_reactive_power,type="n",xlab="datetime",ylab = "Global_reactive_power"))
  with(data,lines(DateTime, Global_reactive_power))
  
}
## Reads data via readInputFromUrl and plots 4 different graphs between 01/02/2007 and 02/02/2007. It temporarily modifies the par to show 4 graphs, it restores initial par after finishing
doPlot4 <- function() {
  initialize()
  data<-readInputFromUrl()
  doPlot4.1(data)
  doPlot4.2(data)
  doPlot4.3(data)
  doPlot4.4(data)
}



## Calls to doPlot but setting the device to a png file "plot4.png"
doPlot4ToPng <- function() {
  
  png(file="plot4.png",width = 480, height = 480)
  originalPar <- par()
  par(mfrow=c(2,2))
  doPlot4()
  par(originalPar)
  dev.off()
}
