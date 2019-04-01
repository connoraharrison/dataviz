##########################################
### Data Visaulization Assignment  5.1 ###
#     Connor Harrison, Mar 10, 2019      #
##########################################

# Load Packages
library(tidyverse)
library(readr)
library(readxl)
library(ggthemes)

# Load Data
ForeignAssistance_data <- read_csv("~/Georgetown Docs/Data/ForeignAssistance-FullDataSet/ForeignAssistance-FullDataSet-2017-and-Later.csv")
region <- read_excel("~/Georgetown Docs/Data/countries of the world.xls")
gdp_full <- read_excel("~/Georgetown Docs/Data/gapdata_gdp_ppp_v14.xlsx")
population <- read_csv("~/Georgetown Docs/Data/UNdata_Export_20190210_173214793/UNdata_Export_20190210_173214793.csv")

# Keep 2018 Data
FA_2018 <- filter(ForeignAssistance_data, Award_Transaction_Fiscal_Year==2018)

# Keep and Rename Relevant Variables
FA_2018 <- select(FA_2018, Year = 'Award_Transaction_Fiscal_Year', Category = 'Award_Transaction_US_Foreign_Assistance_Category', 
                  Amount = 'Award_Transaction_Value', Country = 'Recipient_Location')

# Clean Region and Country Data to Merge
region <- select(region,Country='Data is public domain from US government.', region='...2')
FA_2018_regions <- left_join(FA_2018,region,by="Country")

# Check Missing Values on Merge
region_missing <- filter(FA_2018_regions, is.na(region))
region_missing <-arrange(region_missing, desc(Country))

FA_2018_complete <- FA_2018_regions %>%
  mutate(region=replace(region, region=="NEAR EAST", "NEAR EAST & NORTH AFRICA")) %>%
  mutate(region=replace(region, region=="NORTHERN AFRICA", "NEAR EAST & NORTH AFRICA")) %>%
  mutate(region=replace(region, region=="BALTICS", "EUROPE")) %>%
  mutate(region=replace(region, region=="WESTERN EUROPE", "EUROPE")) %>%
  mutate(region=replace(region, region=="EASTERN EUROPE", "EUROPE")) %>%
  mutate(region=replace(region, region=="C.W. OF IND. STATES", "CENTRAL ASIA")) 
  
FA_2018_complete <- filter(FA_2018_complete, region != "NORTHERN AMERICA" & region != "NA")

# Create Variable: Sum of Total FA for each country
FA_2018_complete_sum <- FA_2018_complete %>% group_by(region) %>%
  mutate(Sum_Amount = sum(Amount))

FA_2018_complete_sum <- ddply(FA_2018_complete, .(Category, region), summarize, val = sum(Amount))

# Plot 1.0
ggplot(data = FA_2018_complete_sum, 
      mapping = aes(x=region, y=val)) +
  geom_col(aes(fill = Category)) +
  coord_flip()

# Plot 2.0
# Create factor to sort columns
FA_2018_complete_sum$region <- factor(FA_2018_complete_sum$region, levels = c("EUROPE", "OCEANIA","CENTRAL ASIA",
                                "LATIN AMER. & CARIB", "ASIA (EX. NEAR EAST)","NEAR EAST & NORTH AFRICA", "SUB-SAHARAN AFRICA"))

ggplot(data = FA_2018_complete_sum, 
       mapping = aes(x=region, y=val)) +
  geom_col(aes(fill = Category)) +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_economist() +
  theme(panel.grid.major.y = element_blank(),
        legend.title = element_blank(), 
        plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(title = paste("The Majority of US Foreign Aid Promotes \nGlobal Health and Humanitarian Assistance"),
       subtitle = "Total Value of Foreign Aid given to each region in FY2018 \nshows US commitment to alleviating Humanitarian Disasters",
       caption = "Source: ForeignAssistance.gov; World Bank Open Data; UN Data")



