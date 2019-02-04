### In Class Exercise

# Import Data
library(haven)
library(ggplot2)
states <- read_dta("C:/Users/conno/OneDrive/Desktop/Stata Files/Data/states.dta")
View(states)



ggplot(data = states,
       mapping = aes(x = college, y = womleg_2011, color = as.character(south))) +
  geom_point(mapping = aes(size = attend_pct)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(name = "Region of the US", 
                     values = c("0" = "brown2", "1" = "darkturquoise"),
                     labels = c("0" = "Non-Southern", "1" = "Southern")) + 
  scale_size_continuous(name = "Percent Who Frequently \nAttend Religious Services") +
  labs(title = paste("Data Visualization in Class Exercise Feb 4, 2019"), 
       subtitle = "Connor Harrison", 
       x = "Percent of College Graduate Residents", 
       y = "Percent of Women in State Legislature")




