#  ------------------------------------------------------------------------------------------------------------------------
#
# Course:  Exploratory Data Analysis
# Project: Course Assignment II
# Author:  Pavlo Sai
# 
# Program Name: plot1.R
#
# Program prepare_data.R uses function dwld_file to download the data from 
# Data download url: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# and then make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008
#
#  ------------------------------------------------------------------------------------------------------------------------ 
#
# Prepare working directory
# use setwd() for selection your working directory
# sourse library
library(plyr)

# Clear Environment Memory
rm(list=ls())

# function to Download the file from the website location to the local directory
dwld_file <- function (fileurl) {
        
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
# Question 1:
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system,   
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
#
################################################################################
#
# Convert the year into a factor
SummarySCC.PM25$year <- factor(SummarySCC.PM25$year) 

# Use ddply() to calculate the sum of Emissions based on the year factor 
plot_data <- ddply(SummarySCC.PM25, .(year), summarize, TotalEmissions = sum(Emissions))

#Base plot 
par(bg="white", mfrow=c(1,1))

plot(plot_data$year, plot_data$TotalEmissions, xlab="Year", ylab="Total PM2.5 Emissions", type="n", boxwex=0.05, main=" Total PM25 Emissions")
lines(plot_data$year, plot_data$TotalEmissions)

# copy the plot from the screen device to .png file
dev.copy(png, file="plot1.png", width = 480, height = 480)

# turn device off
dev.off()
