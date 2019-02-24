##########################################
### Data Visaulization Assignement 3.1 ###
#     Connor Harrison, Feb 24, 2019      #
##########################################

# Reguire Packages
library(tidyverse)
library(readr)
library(ggthemes)

# Load Data
WPP2017_LifeTable <- read_csv("~/Georgetown Docs/Data/WPP2017_LifeTable.csv")

# Only keep aggregated gender observations
life_table2 <- filter(WPP2017_LifeTable, Sex=="Total") 

# Infant Mortality Measure: Keep obs where age range is 0-1
life_table_child <- filter(life_table2, AgeGrp==0)

# Drop Projections
life_table_child_p <- filter(life_table_child, MidPeriod<2019)

# Keep only region aggregated variables
life_table_child_region_p <- filter(life_table_child_p, Location=="Africa" | Location=="Asia" | Location=="Caribbean" 
                                    | Location=="Central America" | Location=="Central Asia" | Location=="Eastern Africa"
                                    | Location=="Eastern Asia" | Location=="Eastern Europe" | Location=="Europe" | 
                                      Location=="High Income Countries" | Location=="Latin America and the Caribbean" 
                                    | Location=="Least developed countries" | Location=="Less developed regions" | 
                                      Location=="Less developed regions, excluding China" | Location=="Low-income countries"
                                    | Location=="Lower-middle-income countries" | Location=="Middle Africa" | 
                                      Location=="Middle-income countries" | Location=="Northern Africa" | Location=="Northern America"
                                    | Location=="South America" | Location=="South-Central Asia" | Location=="South-Eastern Asia"
                                    | Location=="Sub-Saharan Africa" | Location=="Upper-middle-income countries" | 
                                      Location=="Western Europe" | Location=="World" | Location=="Oceania" | Location=="Southern Asia") 

# Refine life tables to selected regions and income categories
life_table_child_region2 <- filter(life_table_child_region_p, Location=="Central Asia" | Location=="Southern Asia" | Location=="South-Eastern Asia" |
                                     Location=="Northern Africa" | Location=="Sub-Saharan Africa" | Location=="Western Europe" |
                                     Location=="Eastern Europe" | Location=="Northern America" | Location=="Latin America and the Caribbean" |
                                     Location=="Oceania")


# Rename Variables Based on Codebook
life_table_child_region2 <- select(life_table_child_region2, Region='Location', Year='MidPeriod', Central_Death_Rate='mx', 
                                    Prob_Dying='qx', Prob_Surviving='px', Number_Deaths='dx', Survival_Ratio='Sx', 
                                    Expectation_of_Life='ex')
# Tile Plot
ggplot(data = life_table_child_region2,
       mapping = aes(x = Year, y = Region)) +
         geom_tile(aes(fill = Expectation_of_Life)) +
  ggthemes::scale_fill_continuous_tableau() +
  theme(plot.title = element_text(face = "bold")) +
  labs(fill="Life Expectancy at Birth",
       x="", y="",
       title = "An Ageing World", 
       subtitle = "Since 1950, average life expectancy has steadily increased across\nthe world. However, significant disparities between regions remain.",
       caption = "Data: UN World Population Prospects 2017")
  
##########################################
### Data Visaulization Assignement 3.2 ###
#     Connor Harrison, Feb 24, 2019      #
##########################################

# Reguire Packages
library(tidyverse)
library(readr)
library(ggthemes)

# Load Data
WPP2017_LifeTable <- read_csv("~/Georgetown Docs/Data/WPP2017_LifeTable.csv")

# Only keep aggregated gender observations
life_table2 <- filter(WPP2017_LifeTable, Sex=="Total") 

# Infant Mortality Measure: Keep obs where age range is 0-1
life_table_child <- filter(life_table2, AgeGrp==0)life_table2 <- filter(WPP2017_LifeTable, Sex=="Total") 

# Life Table for Highly-affected HIV nations
life_table_hiv <- filter(life_table2, Location=="Lesotho" | Location=="Botswana" | Location=="Swaziland"
                         | Location=="Zambia" | Location=="Zimbabwe")

# Pull out specific age and range period
life_table_hiv_p <- filter(life_table_hiv, AgeGrp==0 & MidPeriod < 2019)

# Rename Variables Based on Codebook
life_table_hiv_p <- select(life_table_hiv_p, Region='Location', Year='MidPeriod', Central_Death_Rate='mx', 
                                   Prob_Dying='qx', Prob_Surviving='px', Number_Deaths='dx', Survival_Ratio='Sx', 
                                   Expectation_of_Life='ex')
# Initial Plot 
ggplot(data = life_table_hiv_p,
       mapping = aes(x = Year, y = Expectation_of_Life, color = Region)) +
  geom_line()
  
# Plot
ggplot(data = life_table_hiv_p,
       mapping = aes(x = Year, y = Expectation_of_Life, color = Region)) +
  geom_line(size=1.2, alpha=0.5) +
  scale_x_continuous(breaks = seq(1950, 2020, 5)) +
  scale_y_continuous(breaks = seq(40, 65, 5)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none', panel.grid.major.x = element_blank(), panel.border = element_blank()) +
  scale_colour_economist() +
  labs(fill="",
       x="", y="",
       title = "The HIV Epidemic", 
       subtitle = "Impact of HIV on Life Expectancy in 5 African Nations",
       caption = "Data: UN World Population Prospects 2017")

## Visualization Refined in Illustrator ##
# (somewhat) #

