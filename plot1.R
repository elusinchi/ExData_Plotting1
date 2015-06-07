setClass('myDate')

#the zipped dataset is assumed to be in the same directory from which the R code is executed
source_zip <- "exdata-data-household_power_consumption.zip"
household_data_file <- "household_power_consumption.txt"

if (file.exists(source_zip))
{
  # set to true if you want to FORCE the creation of the dataset from the household data given
  recreateData <- FALSE
  
  # set to true to generate the right plot. The same file is essentially used for plot 1 to 4 
  drawPlot1 <- TRUE
  drawPlot2 <- FALSE
  drawPlot3 <- FALSE
  drawPlot4 <- FALSE
  
  # if the data doesn't exist, create it regardless of recreateData variable
  if (recreateData || !exists("household_data_small"))
  {
    # to debug, we can work on a much smaller data set
    use_smaller_data_set <- FALSE
    number_of_rows <- 1000
    floor_date <- as.Date("2006-12-16")
    end_date <- as.Date("2006-12-16")
  
    # otherwise, use the correct values
    if (!use_smaller_data_set) {
      number_of_rows <- -1
      floor_date <- as.Date("2007-02-01")
      end_date <- as.Date("2007-02-02")
    }
  
    # make sure to read the date in a proper format
    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
    column_classes <- c("myDate", "factor", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric")
    household_data <- read.csv(unz(source_zip, household_data_file), header=TRUE,sep =";", nrows=number_of_rows, na.strings ="?", colClasses = column_classes)
  
    # use a subset to only keep the date within the needed range
    household_data_small <- subset(household_data, (Date >= floor_date & Date <= end_date) )
    
    # create a date/time (i.e. timestamp) column. This will be used as the x axis of our plots
    household_data_small$DateTime <- strptime(paste(household_data_small$Date, household_data_small$Time), format= "%Y-%m-%d %H:%M:%S" )
  }
  
  # -----------------------------------------------------
  
  # The four plots. Use the boolean to generate the one you want (drawPlot1=TRUE in plot1.R, etc.)
  
  #PLOT 1
  if (drawPlot1)
  {
    png(file = "./plot1.png")
    with(household_data_small, hist(Global_active_power, col="Red", main ="Global Active Power", xlab = "Global Active Power (kilowatts)"))
    dev.off()
  }
  
  #PLOT 2
  if (drawPlot2)
  {
    png(file = "./plot2.png")
    plot(household_data_small$DateTime, household_data_small$Global_active_power, type="l", xlab="", ylab ="Global Active Power (kilowatts)")
    dev.off()
  }
  
  #PLOT 3
  if (drawPlot3)
  {
    png(file = "./plot3.png")
    plot(household_data_small$DateTime, household_data_small$Sub_metering_1, type="n", xlab="", ylab ="Energy sub metering", col="Black")
    points(household_data_small$DateTime, household_data_small$Sub_metering_1, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Black")
    points(household_data_small$DateTime, household_data_small$Sub_metering_2, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Red")
    points(household_data_small$DateTime, household_data_small$Sub_metering_3, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Blue")
    legend("topright",col=c("black","red","blue" ),pch="",lty=1, lwd=1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    dev.off()
  }
  
  #PLOT 4
  if (drawPlot4)
  {
    png(file = "./plot4.png")
    par(mfcol=c(2,2))
    plot(household_data_small$DateTime, household_data_small$Global_active_power, type="l", xlab="", ylab ="Global Active Power")
    plot(household_data_small$DateTime, household_data_small$Sub_metering_1, type="n", xlab="", ylab ="Energy sub metering", col="Black")
    points(household_data_small$DateTime, household_data_small$Sub_metering_1, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Black")
    points(household_data_small$DateTime, household_data_small$Sub_metering_2, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Red")
    points(household_data_small$DateTime, household_data_small$Sub_metering_3, type="l", xlab="", ylab ="Global Active Power (kilowatts)", col="Blue")
    legend("topright",col=c("black","red","blue" ),pch="",lty=1, lwd=1, bty="n", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    plot(household_data_small$DateTime, household_data_small$Voltage, type="l", xlab="datetime", ylab ="Voltage")
    plot(household_data_small$DateTime, household_data_small$Global_reactive_power, type="l", xlab="datetime", ylab ="Global_reactive_power")
    dev.off()
  }
} else {print("zipped dataset not found in the working directory!")}
