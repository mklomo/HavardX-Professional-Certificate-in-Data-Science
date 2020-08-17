#Data Visualization in Practice
library(tidyverse)
library(dslabs)
data("gapminder")
gapminder <- gapminder %>%
  as_tibble()


#Checking infant mortality rates for selected countries
countries <- c("Sri Lanka", "Turkey", "Poland", "South Korea", "Malaysia",
               "Russia", "Pakistan", "Vietnam", "Thailand", "South Africa",
               "Thailand", "South Africa")
gapminder %>%
  filter(year == 2015 & country %in% countries) %>%
  select(country, year,infant_mortality)%>%
  arrange(desc(infant_mortality))


#The use of scatterplots
gapminder %>%
  filter(year == 1962) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point()
#Life expectancy around 70 years and above have 3 or fewer children per family
#Life Expectancy lower than 65 years and more than 5 children per family


#Lets confirm whether indeed these countries are fromregions we expect!
gapminder %>%
  filter(year == 1962) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point(aes(colour = continent), size = 3)
#In 1962, "the West versus developing world" view was grounded in some reality.
#Is this still the case 50 years later?
gapminder %>%
  filter(year == 1962 | year == 2012) %>%
  ggplot(aes(x = fertility, y = life_expectancy, group = continent)) +
  geom_point(aes(colour = continent), size = 3) +
  facet_grid(continent ~ year) +
  scale_color_brewer(palette = "Set1")

#Or
gapminder %>%
  filter(year == 1962 | year == 2012) %>%
  ggplot(aes(x = fertility, y = life_expectancy, group = continent)) +
  geom_point(aes(colour = continent), size = 3) +
  facet_wrap(~year) +
  scale_color_brewer(palette = "Set1")
#This plot clearly shows that the majority of countries hae moved
#from the developing world cluster to the western world one.

#Showing progression of life over time,from 1962 - 2016
selected_years <- c(1962, 1972, 1982, 1992, 2002, 2012)
gapminder %>%
  filter(year %in% selected_years) %>%
  ggplot(aes(x = fertility, y = life_expectancy)) +
  geom_point(aes(colour = continent), size = 3) +
  facet_wrap(~year) +
  scale_color_brewer(palette = "Set1")
#This plot clearly shows how most Asian countries have improved
#at a much faster rate than European ones.
#To compare plots in a facet, ensure that the scales are fixed
  
  
  
#Time Series Plots
#Time series plots have time in the x-axis and an outcome or the 
#measurement of treatment in the y-axis
gapminder %>%
  filter(country == "Ghana") %>%
  ggplot(aes(x = year, y = infant_mortality)) +
  geom_point()
#The above plot shows the OUTCOME, infant mortality, over time

gapminder %>%
  filter(country == "Ghana") %>%
  ggplot(aes(x = year, y = fertility)) +
  geom_point()

#Comparing 2 countries using direct labels
gapminder %>%
  filter(country %in% c("Ghana", "Malaysia")) %>%
  ggplot(aes(x = year, y = infant_mortality)) +
  geom_line(aes(colour = country), show.legend = FALSE)  +
  directlabels::geom_dl(aes(label = country, colour = country), method = "smart.grid")
  
#Computing GDP per person
gapminder <- gapminder %>%
  mutate(dollars_per_day = (gdp/(population*365)))

#Displaying the distribution of dollars_per_day in 1970

gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(x = log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, colour = "black")
#Because the plot is concentrated around the $10 section,
#we are unable to investigate the very poor countries.
#We use log transformations to investigate further into this.
#Because the range of the data is pretty small, a log2 transformation does the trick.
gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, colour = "black")
#This provides a close-up of the mid to lower income countries

#Which base?
#Other cases involve involve transforming using base e and base 10.
#Prof does not recommend using natural log for data exploration and visualization.
#In this case, we use base 2 instead of base 10 because the resulting output is easier to interpret

#An example in which base 10 makes sense
gapminder %>%
  filter(year == 2000) %>%
  ggplot(aes(x = log10(population))) +
  geom_histogram(binwidth = 0.5, colour = "black")
  
#Transforming values or the scale
#There are 2 ways to do this:
#-log the values before plotting them 
#-or use log scales in the axes
#The advantage of using logged scales is that we see the original values on the axes.
#The other advantage of showing logged scales is that the original values are displayed in the plot,
#which are easier to interpret.
gapminder %>%
  filter(year == 2000 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_histogram(colour = "black", binwidth = 1) +
  scale_x_continuous(trans = "log2")
#Note that the log base 10 transformation has its own function:
#scale_x_log10, but currently base 2 does not.
#The logistic transformation (logit) is useful when plotting proportions between 0 and 1.
#The square root transformation (sqrt) is useful when considering counts.
#The reverse transformation is used when we want smaller values to be on the right or on top.
  
  
  
#Visualizing multimodal distribution
gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_histogram(binwidth = 1, colour = "black") +
  scale_x_continuous(trans = "log2")
#When a distribution, like the one above, does not MONOTONICALLY decrease from the mode,
#we call the locations where it goes up and down again local modes and say that the distribution
#has multiple modes.
#The histogram above suggests that the 1970 country income distribution has 2 modes:
#-one at about 2 dollars per day 
#-another at about 32 dollars per day
#This bimodality is consistent with a dichotomous world made up of countries with average income
#less than $8 a day and countries above that.


#Comparing Multiple Distributions with boxplots and ridge plots
#We note that the histogram does not show if if the two groups of
#countries aee west versus the developing world
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(x = dollars_per_day, y = region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

#From the above, we declare two groups
gapminder <- gapminder %>%
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe", "Southern Europe",
                  "Northern America", "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    continent == "Africa" & region != "North Africa" ~ "Sub-Saharan",
    TRUE ~ "Others"
  ))

#Transform this group into a factor
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Sub-Saharan",
                                          "Latin America", "East Asia",
                                          "West")))
  
#Establishing a base boxplot for the groups
base <- gapminder %>%
  filter(year == 1970, !is.na(gdp)) %>%
  ggplot(aes(reorder(x = group, dollars_per_day, FUN = median), y = dollars_per_day)) +
  geom_boxplot(aes(fill = group), show.legend = FALSE) +
  scale_y_continuous(trans = "log2") +
  geom_point(alpha = 0.4) +
  xlab("")


#Ridge Plots
#These are stacked smooth densities or histograms by using the function ggridges package
gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day, y = reorder(group, dollars_per_day, FUN = median))) +
  scale_x_continuous(trans = "log2") +
  ggridges::geom_density_ridges(scale = 1, aes(fill = group))
  
  
#If the number of data points is small enough, we can add them to the ridge plot as follows
gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day, y = reorder(group, dollars_per_day, FUN = median))) +
  scale_x_continuous(trans = "log2") +
  ggridges::geom_density_ridges(jittered_points = TRUE, scale = 1, aes(fill = group), alpha = 0.4)

#Comparing 1970 to 2010
gapminder %>%
  filter(year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(west_vs_developing = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_histogram(binwidth = 1, colour = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west_vs_developing)


#We notice that there are far more country counts in 2010 than in 1970.
#This is because some countries were founded after the 1970. An example
#is the Soviet Union.

#List of countries with dollars_per_day in 1970
country_list_1 <- gapminder %>%
  filter(year == 1970 & !is.na(dollars_per_day)) %>%
  .$country

#List of countries with dollars_per_day in 2010
country_list_2 <- gapminder %>%
  filter(year == 2010 & !is.na(dollars_per_day)) %>%
  .$country
  
#Final Country list
country_list <- intersect(country_list_1,country_list_2)
  
#Hence,
gapminder %>%
  filter(country %in% country_list & year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(west_vs_developing = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_histogram(binwidth = 1, colour = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west_vs_developing)
  

#Proportion of data represented by country list  
country_list_pop_2010 <-gapminder %>% filter(country %in% country_list & year %in% 2010 & !is.na(population)) %>% 
  summarise(Population = sum(population)) %>%
  .$Population
  
world_pop_2010 <- gapminder %>% filter(year %in% 2010 & !is.na(population)) %>% 
  summarise(Population = sum(population)) %>%
  .$Population
  
data_rep <- (country_list_pop_2010/world_pop_2010)*100

#We now see that the rich countries have become a bit richer,
#but percentage-wise, the poor countries appear to have improved more.

#Which specific regions?
gapminder %>%
  filter(country %in% country_list & year %in% c(1970, 2010) & !is.na(dollars_per_day)) %>%
  ggplot(aes(reorder(x = group, dollars_per_day, FUN = median), y = dollars_per_day)) +
  geom_boxplot(aes(fill = group)) +
  scale_y_continuous(trans = "log2") +
  geom_point(alpha = 0.4) +
  facet_wrap(~ year)

#We would also like to show the boxplots next to eachother
gapminder %>%
  filter(country %in% country_list & year %in% c(1970, 2010) & !is.na(dollars_per_day)) %>%
  ggplot(aes(reorder(x = group, dollars_per_day, FUN = median), y = dollars_per_day, fill = factor(year))) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2")


#The Ecological Fallacy and importance of showing the data
#Note that it is necessary to describe the importance of variability between
#groups when examining group level data
present_year <- 2010
mew <- gapminder %>%
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe", "Southern Europe",
                  "Northern America", "Australia and New Zealand") ~ "West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% "Southern Asia" ~ "Southern Asia",
    region %in% "Central Asia" ~ "Central Asia",
    region %in% "Eastern Europe" ~ "Eastern Europe",
    region %in% "Western Asia" ~ "Western Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & .$region != "North Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands")) 


new_df <- new %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
    summarise(income = (sum(gdp)/sum(population))*(1/365),
            infant_survival_rate = 1 - sum(infant_mortality*population/1000)/sum(population))

#Plot infant survival vs income with transformed axes
new_df %>%
  ggplot(aes(x = income, y = infant_survival_rate, label = group, colour = group)) +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "logit") +
  geom_label(size = 3, show.legend = FALSE)
#Based on the plot above,  do we conclude that a country with a low income
#is destined to have a low survival rate? Do we conclude that survival rates
#in Sub-Saharan Africa are all lower than in Southern Asia, which are in turn lower than
# the Pacific Islands, and so on?

#Jumping to this conclusion based on a plot showing averages is referred to as the ecological fallacy.
#The almost perfect linear relationship between survival rates and income is only observed for the averages
#at the regional level.
#The plot below shows a different story


new %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  ggplot(aes(x = dollars_per_day, y = (1-infant_mortality/1000), label = group)) +
  ggrepel::geom_text_repel(data = new[sample(nrow(new),7),], aes(label = country, colour = group), size = 5, show.legend =  FALSE) +
  geom_point(aes(colour = group), size = 3, alpha = 0.4) +
  scale_y_continuous(trans = "logit") +
  scale_x_continuous(trans = "log2")
#Here, we see that countries from the same regions can be quiet different and that countries
#with the same income can have different survival rates.
  
#The logit scale is useful when we want to highlight differences near 0 or 1.
#For survival rates, this is important because a survival rate of 90% is UNACCEPTABLE.
#While a survival rate of 99% is relatively good.
#Here, p is the proportion of infants that survived. The odds (p/(1-p)) tell us how many
#more infants are expected to survive than to die.
  









  
  

  
  
  
  
  
  
  
  
  
  
