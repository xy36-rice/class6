# EBIO338/358: Analysis and Visualization of Biological Data
# Class 6: Data Exploration

# Load data on the status of tropical mammals and birds

data <- read.csv(file="PlosBiology_TableS2_Class.csv", stringsAsFactors = T)
str(data)
head(data)
str(data)

# Explore data 

# Use the table() function to generate a FREQUENCY TABLE with counts of observations for each category
table(data$Class)
table(data$RLS)
table(data$Diet)
table(data$Status)
table(data$Hunted)
table(data$Site)

# Use the length() function to count the number of elements in a vector (i.e., how long a vector is)
length(table(data$Site))

# It's  good practice to build in checks to make sure you're getting what you think you're getting
# For example, check to make sure the table contains the expected number of observations using sum() 
# and comparing that value to the number of rows in the data by extracting the dimensions using dim()
sum(table(data$Site))
dim(data)

# Use the table() function to generate a CONTINGENCY TABLE, 
# which shows how observations are distributed along a variable, 
# contingent on the value of another variable 

table(data$Site, data$Class)
table(data$Diet, data$Class)

# This can also be achieved using tapply()
tapply(data$Diet, data$Class, table)

# But table() can also create a contingency table using multiple variables
table(data$Diet, data$Site, data$Class)
table(data$Diet, data$Class, data$Site)

# Still each observation (i.e., row of data) is only included in one of the tables
sum(table(data$Diet, data$Class, data$Site))

# TO: a) Create a contingency table showing the number of hunted species per site
# b) Create a contingency table separately for birds and mammals showing the population status for each Red List Status (i.s., RLS) 


# GRAPHICAL DATA EXPLORATION 
# Plot
plot(data$Slope ~ log(data$Mass_kg), ylab="Population trend", xlab="log(Body mass) (kg)", las=1)
points(data$Slope[data$Class=="Aves"] ~ log(data$Mass_kg[data$Class=="Aves"]), col="blue")
text(6, 1.2, "Birds", col="blue")
text(6, 1.4, "Mammals")

# Use "adj" to adjust which way the text is justified
text(6, 1.2, "Birds", col="blue", adj=0) # 0 for left justified text
text(6, 1.4, "Mammals", adj=0)

# Redraw figure and add points to complete the legend
plot(data$Slope ~ log(data$Mass_kg), ylab="Population trend", xlab="log(Body mass) (kg)", las=1)
points(data$Slope[data$Class=="Aves"] ~ log(data$Mass_kg[data$Class=="Aves"]), col="blue")
text(6, 1.2, "Birds", col="blue", adj=0) # 0 for left justified text
text(6, 1.4, "Mammals", adj=0)

points(5.7, 1.4)
points(5.7, 1.2, col="blue")


# Let's look closer at the data on body mass 

sort(data$Mass_kg)
min(data$Mass_kg)

# We can make our own FUNCTION called my.min to calculate the minimum of a vector using 
# function(x){} and adding the code in between the {}
my.min <- function(x){
  sort(x)[1]
}

my.min(data$Mass_kg)

# or our own function my.mean to calculate the mean
my.mean <- function(x){
  sum(x)/length(x)
}

my.mean(data$Mass_kg)
mean(data$Mass_kg)

# TO DO: Create a function called my.max to calculate the maximum value of a vector


# The summary() function is a handy function that gives descriptive statistics for a numeric vector
summary(data$Mass_kg)
summary(data$Slope)

# Quartiles divide the data into 4 equal parts based on the number of observations
# The 50th percentile is the median
# The 25th percentile is the middle number between the smallest number and the median
# The 75th percentile is the middle number between the median and the largest number

myvec <- c(1, 1, 1, 4, 5, 6, 8, 8, 11, 15, 16)
myvec
summary(myvec)
# DO TO: By hand, determine the 25th, 50th and 75th percentiles for myvec.
# Now check to see if your calculation matches values given by a summary of myvec

# We can summarize also multiple groups at once using tapply()
tapply(data$Slope, data$Class, summary)
tapply(data$Slope, data$Status, summary)




# BOXPLOTS
head(data)
?boxplot
boxplot(log(data$Mass_kg) ~ data$Class) 
boxplot(log(data$Mass_kg) ~ data$Class, ylab="log(Body mass) (kg)", 
        xlab="Class", las=1, main="Body size of birds and mammals")

# In a boxplot, the middle band is the 50th percentile
# The top and bottom of the box are the 75th and 25th percentiles, respectively 
# The ends of the whiskers can be set by the argument range (default range is 1.5 x interquartile range from box)

# Set notch to TRUE to show notches; medians differ between groups if notches don't overlap
boxplot(log(data$Mass_kg) ~ data$Class, ylab="log(Body mass) (kg)", 
        xlab="Class", las=1, main="Body size of birds and mammals",
        notch=TRUE)

# Flip orientation by setting horizontal to TRUE; update x and y names accordingly
boxplot(log(data$Mass_kg) ~ data$Class, ylab="Class", 
        xlab="log(Body mass) (kg)",  main="Body size of birds and mammals",
        notch=TRUE, horizontal=TRUE)

# Boxplots can be applied to multiple factors by adding them to the formula
boxplot(log(data$Mass_kg) ~  data$Diet + data$Class, ylab="", 
        xlab="log(Body mass) (kg)", las=1, main="Body size by diet and class", cex.axis=1, horizontal=TRUE)

# Change the size of the axis text using cex.axis 
boxplot(log(data$Mass_kg) ~  data$Diet + data$Class, ylab="", 
        xlab="log(Body mass) (kg)", las=1, main="Body size by diet and class", cex.axis=0.3, horizontal=TRUE)

# Or change the size of the margins
par(mar=c(5, 10, 4, 2)) 
boxplot(log(data$Mass_kg) ~  data$Diet + data$Class, ylab="", 
        xlab="log(Body mass) (kg)", las=1, main="Body size by diet and class", cex.axis=1, horizontal=TRUE)

# For an extra layer of summary information, varwidth argument draws box widths in proportion to their number of observations
boxplot(log(data$Mass_kg) ~  data$Diet + data$Class, ylab="", varwidth=TRUE,
        xlab="log(Body mass) (kg)", las=1, main="Body size by diet and class", cex.axis=1, horizontal=TRUE)
par(mar=c(5, 4, 4, 2)) # Set margin size back to default values


# BARPLOTS
?barplot()

# Create a barplot showing the number of species per site
barplot(table(data$Site))

# Clean up the axes
barplot(table(data$Site), ylab="Number of species", xlab="Tropical Forest National Park", las=2)

# Order the bars so that the sites with the most species are farthest to the left
barplot(sort(table(data$Site), decreasing=TRUE), ylab="Number of species", xlab="Tropical Forest National Park", las=2)

# Change the color and fill from solid to striped
barplot(sort(table(data$Site), decreasing=TRUE), ylab="Number of species", 
        xlab="Tropical Forest National Park", las=2, col="orange", border="brown", density=20, angle=45)




# PIECHARTS
?pie

# Let's use a pie chart to look at the status of wildlife 

# First examine the population status for all populations in the dataset
pie(table(data$Status))

# Add a main title and make the colors more meaningful in relation to the status
pie(table(data$Status), col=c("orange", "green4", "light blue", "gray"),
    main="Population status of tropical mammals and birds
    in 15 protected areas worldwide")

# Now look at the conservation status based on the IUCN Red List Status 
pie(table(data$RLS))

# Add a main title
pie(table(data$RLS), 
    main="Conservation Status of tropical mammals and birds
    in 15 protected areas worldwide")

# Rotate the pie by changing the initial angle
pie(table(data$RLS), init.angle=160,
    main="Conservation Status of tropical mammals and birds
    in 15 protected areas worldwide")

# And make the colors more meaningful
pie(table(data$RLS), init.angle=160,
    col=c("black", "light blue", "red", "gray", "yellow", "orange"),
    main="Conservation status of tropical mammals and birds
    in 15 protected areas worldwide 
    (N=510 populations)")

# Reorder levels of RLS to make order more meaningful
data$RLS <- factor(data$RLS, levels(data$RLS)[c(2, 4, 5, 6, 3, 1)])

pie(table(data$RLS), init.angle=0,
    col=c("grey90", "grey80", "grey70", "grey60", "grey40", "grey20"),
    border = F,
    main="Conservation status of tropical mammals and birds
    in 15 protected areas worldwide 
    (N=510 populations)")
