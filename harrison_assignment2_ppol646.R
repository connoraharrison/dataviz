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

### Original Visualization 2 ###

# Load Packages
library(tidyverse)
library(readr)
library(readxl)
library(wesanderson)

# Load Data
USAID <- read_csv("~/Georgetown Docs/U.S. Agency for International Development.csv")
population <- read_csv("~/Georgetown Docs/Data/UNdata_Export_20190210_173214793/UNdata_Export_20190210_173214793.csv")
gapdata_gdp_ppp_v14 <- read_excel("~/Georgetown Docs/Data/gapdata_gdp_ppp_v14.xlsx")
countries_of_the_world <- read_excel("~/Georgetown Docs/Data/countries of the world.xls")

# Clean Data Sets
#GDP
gdp <- gapdata_gdp_ppp_v14[, 1:3]
gdp_2018 <- filter(gapdata_gdp_ppp_v14, Year=='2018')
gdp_2018 <- select(gdp_2018, country='Area', gdp='GDP per capita - with interpolations')

#Population
population <- select(population, country='Country or Area', population='Value')

#USAID
usaid_2018 <- filter(USAID, Award_Transaction_Fiscal_Year=='2018')
usaid_2018 <- select(usaid_2018, country='Recipient_Location', amount='Award_Transaction_Value')
usaid_sum <- usaid_2018 %>% group_by(country) %>% summarize(amount_total = sum(amount))

# Region Data
region <- select(countries_of_the_world,country='Data is public domain from US government.', region='..2')


# Join Data Frames
pop_gdp <- left_join(population,gdp_2018,by="country")
aid_region <- left_join(region,usaid_sum,by="country")
aid_total_country <- left_join(pop_gdp,aid_region,by="country")

# Exploratory Visualization
ggplot(data = aid_total_country,
       mapping = aes(x = gdp, y = amount_total, color = region)) +
  geom_point(mapping = aes(size = population))

# Drop Outliers
aid_total_country <- filter(aid_total_country, gdp < 20000)
gdp_low <- filter(aid_total_country, gdp < 10000)

# Refined Visualization
ggplot(data = gdp_low,
       mapping = aes(x = gdp, y = amount_total, color = region)) +
  geom_point() +
  geom_text(aes(label=country, size=population), show.legend = FALSE) +
  scale_size_continuous(range = c(2,10)) +
  scale_color_manual(name = "Region", values = wes_palette(n=5, name="Darjeeling1"), 
                     labels = c("ASIA (EX. NEAR EAST)" = "Asia", "C.W. OF IND. STATES" = "C.Asia",
                                                 "LATIN AMER. & CARIB" = "Latin America", "OCEANIA" ="Oceania", 
                                                 "SUB-SAHARAN AFRICA" = "Sub-Saharan Africa")) +
  labs(title = paste("Is US Foreign Aid Getting to the Right Places?"),
       subtitle = "USAID Funding to the Poorest Nations in FY18",
       caption = "Source: ForeignAssistance.gov, World Bank, UN Data",
       x = "Per Capita GDP", 
       y = "Foreign Assistance Received
       (in USD)")




