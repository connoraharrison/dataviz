##### Data Visualization Assignment 1 #####

### Replication ###

# Load Data
crimeRatesByState2005 <- read.csv("~/Georgetown Class Documents/PPOL 646 - Data Visualization/crimeRatesByState2005.csv")
View(crimeRatesByState2005)

# Call Packages
library(readxl)
library(ggplot2)

# Assign new name to data set
crime_data <- crimeRatesByState2005

# Remove Outlier (D.C.) and US Averages
crime_data2 <- crime_data[crime_data$state != "District of Columbia",]
crime_data_clean <- crime_data2[crime_data2$state != "United States",]

# XY Plane
ggplot(data = crime_data_clean, 
       mapping = aes(x = murder, y = burglary))

# Load Scatter Data; Add colors and opacity to data points
ggplot(data = crime_data_clean, 
       mapping = aes(x = murder, y = burglary)) +
  geom_point(alpha = 0.2, color = "blue")

# Add Best Fit Line; Change color
ggplot(data = crime_data_clean, 
       mapping = aes(x = murder, y = burglary)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color="blue", size = 1.2) + 
  scale_x_continuous(limits=c(0,10), breaks=c(0, 2, 4, 6, 8, 10)) + 
  scale_y_continuous(limits=c(0, 1200), breaks=c(0, 200, 400, 600, 800, 1000, 1200)) +
  labs(title = paste("MURDERS VERSUS BURGLARIES IN THE UNITED STATES"), 
       subtitle = "States with higher murder rates tend to have higher burglary rates", 
       caption = "Source: U.S. Census Bureau | Nathan Yau", 
       x = "Muders per 100,000 population", 
       y = "Burglaries per 100,000 population")
       
### Original Visualization ###

# Install necessary package
install.packages('doBy')

# Call Packages
library(doBy)
library(ggplot2)
library(plyr)

# Load and View Data
U.S..Agency.for.International.Development <- read.csv("~/Georgetown Class Documents/PPOL 646 - Data Visualization/U.S. Agency for International Development.csv")
View(U.S..Agency.for.International.Development)

# Rename Data
usaid <- U.S..Agency.for.International.Development

# Keep data for year 2018
usaid2 <- usaid[usaid$Award_Transaction_Fiscal_Year == 2018, ]

# Summary Stat 1: Average Project Value by Country
projectval_avg <- summaryBy(Award_Transaction_Value ~ Recipient_Location, data = usaid2)

# Summary Stat 2: Number of Projects Funded by Country
number_projects <- count(usaid2, "Recipient_Location")

#Combine Summary stats into single data frame
usaid3 <- data.frame(number_projects, projectval_avg)

# Scatter Plot A
ggplot(data = usaid3,
       mapping = aes(x = freq, y = Award_Transaction_Value.mean)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red")

# Intial Scatter Reveals Outliers. Drop values
usaid4 <- usaid3[usaid3$freq < 15000,]

usaid5 <- usaid4[usaid4$Award_Transaction_Value.mean < 4000000,]

# Scatter Plot B
ggplot(data = usaid5,
       mapping = aes(x = freq, y = Award_Transaction_Value.mean)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red")

# Load Region Data
library(readxl)
countries_of_the_world <- read_excel("C:\\Users\\conno\\OneDrive\\Documents\\Georgetown Class Documents\\PPOL 646 - Data Visualization\\countries of the world.xls")

# Rename country variable in USAID data for merge
names(usaid5)[names(usaid5) == "Recipient_Location"] <- "Country"

# Merge Data Frames to Include Region
usaid_region <- merge(usaid5, countries_of_the_world, by.x = "Country", by.y = "Data is public domain from US government.")

# Rename Region Variable
names(usaid_region)[names(usaid_region) == "..2"] <- "Region"

# Assign Color
usaid_region$Region <- as.factor(usaid_region$Region)

# Add Labels to Scatter Plot
ggplot(data = usaid_region,
       mapping = aes(x = freq, y = Award_Transaction_Value.mean, color=Region)) +
  geom_point(alpha = 0.5, size = 2) + 
  geom_smooth(method = "loess", color = "red", se = FALSE) + 
  labs(title = paste("Frequency and Average Value of USAID Grants"), 
       subtitle = "By Country for FY18", 
       caption = "Source: ForeignAssistance.gov", 
       x = "Number of Grants Received", 
       y = "Average Value of USAID Grant")
