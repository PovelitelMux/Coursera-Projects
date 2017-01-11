#  ------------------------------------------------------------------------------------------------------------------------
#
# Course:  Exploratory Data Analysis
# Project: Course Assignment II
# Author:  Sai Pavlo
# 
# Program Name: plot2.R
#
# Program prepare_data.R uses function dwld_file to download the data from 
# Data download url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# and then make a plot to find whether total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
#
#  ------------------------------------------------------------------------------------------------------------------------ 
#
# Prepare working directory
# use setwd() function for selection your working directory
# sourse library
library(plyr)
library(ggplot2)
# clean working area 
rm(list=ls())

# function to Download the file from the website location to the local directory
dwld_file <- function(fileurl){
        
        if (!file.exists("data")) dir.create("data")                            # create a folder if it doesnt exist        
        
        if (!file.exists("./data/NEI-Dataset.zip")) {                           # download the file if its not already downloaded
                download.file(fileurl, destfile = "./data/NEI-Dataset.zip", method = "curl")
        }
        
        # Unzip all files
        unzip("./data/NEI-Dataset.zip", overwrite=T)
        
        #List all files in zip folder
        files <- unzip("./data/NEI-Dataset.zip", list=T)
}

# Call funtion to download from the url
dwld_file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip")

# read RDS files
SCC             <- readRDS("Source_Classification_Code.rds")
SummarySCC.PM25 <- readRDS("summarySCC_PM25.rds")
#
################################################################################
#
# Question 6:
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles 
# County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?#
#
################################################################################
#
# Convert year into a factor
SummarySCC.PM25$year <- factor(SummarySCC.PM25$year) 

# Filter Baltimore && LA Data
LA.Bltimr.Data <- SummarySCC.PM25[ SummarySCC.PM25$fips =="24510" | SummarySCC.PM25$fips =="06037" ,]

# Check for the Motor Vehicle Source classification
Vehicle.Data <- as.data.frame(SCC[ grep("vehicle", SCC$EI.Sector, ignore.case=T), 1])
names(Vehicle.Data) <- "SCC"

# Merge the Motor Vehicle with the PM25 data
Merged  <- merge(Vehicle.Data, LA.Bltimr.Data, by="SCC", na.rm=TRUE)

# Create City variable and assign appropriate city names for the fips code
Merged$city[Merged$fips=="24510"] <- "Baltimore"
Merged$city[Merged$fips=="06037"] <- "LA"

# prepare the data for plotting by calculating the mean
plot_data <- ddply(Merged, .(year, city), summarize, Total.Emissions=sum(Emissions))

# GG plot 
par(bg="white", mfrow=c(1,1))

# Use qplot() for graph
#qplot(data=plot_data, x=year, y=Total.Emissions, color=city, xlab="Year", ylab="Total Emissions each year", 
#     main="LA vs. Baltimore - PM2.5 Emission from Motor Vehicles")

# Use ggplot() for graph
gplot<-ggplot(plot_data, aes(year, Total.Emissions))
gplot + geom_point(aes(color=city), size=4) + labs( title="LA vs. Baltimore - PM2.5 Emission from Motor Vehicles",
                                   y="Total PM2.5 Emission each year")

# copy the plot from the screen device to .png file
dev.copy(png, file="plot6.png", width = 480, height = 480)

# turn device off
dev.off()
