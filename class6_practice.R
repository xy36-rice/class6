# EBIO338/358: Analysis and Visualization of Biological Data
# Class 6: Data Exploration Practice

# Load data on the tropical forest national parks home to the mammals and bird populations from class
# The file is PlosBiology_TableS1_Class.csv. Name the data sites.
sites <- read.csv(file="PlosBiology_TableS1_Class.csv", stringsAsFactors = T)

# Examine the first 5 lines and the structure of the data
head(sites, 5)
str(sites)
# Summarize the number of years the sites have been studied
summary(sites$Years)
# Make a pie chart showing the number of years the sites have been studied and orient it so that the greatest number is on the left
pie(sites$Years, clockwise = T)
# Summarize the number of species at the sites
summary(sites$Species)
# Create a frequency table of the number of sites in each region; each country
table(sites$Region)
table(sites$Country)
# What is the mean number of species per region rounded to the nearest whole number?
round(tapply(sites$Species, sites$Region, mean))
# Create a barplot showing variation in the number of species per region
barplot(round(tapply(sites$Species, sites$Region, mean)))
# Add x and y axis labels and fill the black columns with pink hatching that descends from left to right
barplot(round(tapply(sites$Species, sites$Region, mean)),
        xlab = "Region", ylab = "Species", col="pink", density=20, angle=-45)
# Create a contingency table of the landscape types per region
table(sites$Landscape, sites$Region)
# Create a boxplot of protected area size in hectares (PA_AREA) by region with nice axes labels
# Make the y-axis labels horizontal and ensure they are fully visible in the plot
# Make the width of the boxes proportional to the observations
par(mar=c(5, 6, 4, 2))
boxplot(sites$PA_Area ~ sites$Region, xlab = "Region", ylab = "", varwidth = T, las = 1)
par(mar=c(5, 4, 4, 2)) # Set margin size back to default values
title(ylab = "Protected Area Size")
# Create a boxplot of protected area size in hectares (PA_AREA) by landscape type with nice axes labels. Use notches and orient it horizontally
par(mar=c(5, 6, 4, 2))
boxplot(sites$PA_Area ~ sites$Landscape, xlab = "Landscape", ylab = "", notch = T, horizontal = T, las = 1)
par(mar=c(5, 4, 4, 2)) # Set margin size back to default values
title(ylab = "Protected Area Size")
