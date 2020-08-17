#Chapter 8: Visualizing data distribution
#summarizing data
library(tidyverse)
library(dslabs)
data("heights")
s <- heights %>%
  filter(sex == "Male") %>%
  summarise(average = mean(height), std_dev = sd(height))

#The class of s is a data.frame hence we
#use s$average to access the average. Same applies to std_dev
class(s)
s <- heights %>%
  filter(sex == "Male") %>%
  summarise(median = median(height), 
            minimum = min(height),
            maximum = max(height))

#With summarise, we can only call functions that return a single value

#Using the dot placeholder
#This makes dplyr return a vector instead of a dataframe
data("murders")
murders <- murders %>%
  mutate(murder_rate = (total/population)*100000)

summarise(murders, mean(murder_rate))
#However, this is not the murder_rate of the US,
#because in this computation, we are counting the small states
#just the same as the large states

#The US murder rate is computed by
us_murder_rate <- murders %>%
  summarise(rate = (sum(total)/sum(population)*100000))

#Now, this is the real US murder rate and its still a dataframe
class(us_murder_rate)


#To fix this, we use the "." or placeholder function
us_murder_rate <- us_murder_rate %>%
  .$rate

class(us_murder_rate)


#Sorting by different columns
#use arrange to sort entire tables
data("murders")
murders %>%
  #sort the entire data table in ascending order of population
  arrange(population) %>%
  head(3)

#arranging the entire data table by descending order of murder_rate
murders <- murders %>%
  mutate(rate = (total/population)*100000)

murders %>%
  arrange(desc(rate)) %>%
  head(5)

#arrange by region and then by murder_rate
murders %>%
  arrange(region, population, rate) %>%
  head(6)


#Use top_n() to show the top 15 states with highest murder_rate then order them 
murders %>%
  top_n(15, rate) %>%
  arrange(desc(rate))


#Or
murders %>%
  arrange(desc(rate)) %>%
  top_n(15)

#Or
murders %>%
  arrange(desc(rate)) %>%
  head(15)

#Case study: Describing student heights
library(tidyverse)
library(dslabs)
data("heights")

#With categorical data, the distribution simply describes the proportion
#of each category
prop.table(table(heights$sex))
#This two-frequency table is the simplest form of a distribution

#To visualize more than 2 categories, a simple barplot describes the distribtion
data("murders")
murders %>%
  group_by(region) %>%
  summarise(Proportion = n()/nrow(murders)) %>%
  ggplot(aes(reorder(x = region, Proportion), y = Proportion, fill = region)) +
  geom_bar(stat = "identity")
#We usually use barplots to display a few numbers


#Cummulative Density distributions
#Similar to what the frequency table does for categorical data,
#the CDF defines the distribution for numerical data
heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height)) +
  stat_ecdf()
#The word empirical,"e", is added to make the distinction when data is used.
#We therefore use the term ecdf

#Histgrams
#Although the CDF concept is widely discussed in statistics textbooks,
#the plot is actually not very popular.
heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "#999999", col = "black") +
  xlab("Male heights in inches")

#Description: First, the range of the data is from 49 to 83 inches
#Second: The male heights are close to symmetric around 70 inches
#What information do we loose?All the values in each interval are treated the same
#when computing bin heights (for e.g. no distinguishing b/n 64, 64.1 and 64.2).


#Smoothed Density
#In smoothed density plots, density is plotted against the height,
#as opposed to a histogram where count is plotted against the heights.
#We therefore plot a histogram, using binsizes appropraite for our data
#and computing frequencies rather than counts, and we draw a smooth curve that goes
#through the tops of the histogram bars.

heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height)) +
  geom_density(fill = "red", alpha = 0.3)


heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(x = height)) +
  geom_density(fill = "blue", alpha = 0.3)
#We can see a second bump making it flout the normality assumption

#Interpreting the y-axis
#The geom_density is scales so that the area under the density curve
#adds up to 1.


#The Normal Distribution
#The mean and standard deviation is all that is needed to specify the normal distribution.
#In R we scale variables using the scale() function
#To check for normality, we use the quantile-quantlie plot i.e. qqplot
#To effectively do this, scale the data first and then make the qqplot against the standard normal
heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

murders %>%
  mutate(rate = (total/population)*100000) %>%
  ggplot(aes(sample = scale(rate))) +
  geom_qq() +
  geom_abline()

#Assumption of normality does not apply here  

#Boxplots
ggplot(data = heights, aes(x = sex, y = height)) +
  geom_boxplot(aes(fill = sex))

#Case study: describing student heights
#Techniques used to confirm normal approximation
#-Histogram
#-Density plots
#-QQ-plots

#But to compare 2 or more distributions, we use the boxplots
#Boxplots
ggplot(data = heights, aes(x = sex, y = height)) +
  geom_boxplot(aes(fill = sex))
  
#This plot immiediately reveals that males are on average taller than females.
#But does the normal approximation also work on female height data?
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(x = height)) +
  geom_histogram(fill = "#999999", colour = "black")
#Notice the very short heights close to 50 inches 
#Doesn't approximate normal curve well
#Lets check with the desity plot
heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(x = height)) +
  geom_density(fill = "blue", colour = "black", alpha = 0.4)
#The second bumb is confirmed

heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()
#Here, we notice that the highest points tend to be taller than expected
#by the normal distibution

