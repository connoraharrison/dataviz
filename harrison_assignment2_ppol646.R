### Data Visualization Assignment 2 ###

# Import Data

library(readr)
crimeRatesByState2005 <- read_delim("C:\\Users\\Connor\\Documents\\Georgetown Docs\\crimeRatesByState2005.tsv", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
View(crimeRatesByState2005)

# Load Packages
library(tidyverse)

### Yau Replication Figure 6-15 ###

# Rename data
crime <- crimeRatesByState2005

# Base Plot 
ggplot(data = crime,
       mapping = aes(x = murder, y = burglary)) +
  geom_point(mapping = aes(size=sqrt(population/pi)), fill="red", pch=21) +
  scale_radius(name = "Population", range = c(1,25)) +
  geom_text(aes(label=state), show.legend = FALSE, size = 2.5)
  

# Plot with Labels
ggplot(data = crime,
       mapping = aes(x = murder, y = burglary)) +
  geom_point(mapping = aes(size=sqrt(population/pi)), fill="red", pch=21, show.legend = FALSE) +
  scale_radius(name = "Population", range = c(1,25)) +
  geom_text(aes(label=state), size = 2.5) +
  labs(title = paste("MURDERS VERSUS BURGLARIES IN THE UNITED STATES"),
       caption = "Source: U.S.Census Bureau | Nathan Yau",
       x = "Murders
            per 100,000 population", 
       y = "Burglaries
            per 100,000 population")