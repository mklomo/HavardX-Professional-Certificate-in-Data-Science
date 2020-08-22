#Chapter 11: Robust Summaries

#11.1 Outliers
library(tidyverse)
library(dslabs)
data("outlier_example")
str(outlier_example)

#With the assumtion of normality, we summarise the data with the mean and sd
mean(outlier_example)
sd(outlier_example)
#There is something wrong right? Look at the std. dev.
#A 95% CI constructed around the mean reveals the interval
#(mean - qnorm(0.975)*std_dev) < mean < (mean + qnorm(0.975)*std_dev)
#6.10382-(1.96*7.796558) < 6.10382 < 6.10382-(1.96*7.796558)
#-9.177516 < 6.10382 < 21.38507

#Using a boxplot
boxplot(outlier_example)



#11.2 Using the median instead
#The median value for which half the values are smaller 
#and the other half are bigger, is robust to such outlers
median(outlier_example)

#IQR
#From theory normally distributed data implies that IQR/1.349 = std_dev()
IQR(outlier_example)/1.349
#about 3 inches

#Chapter 11.4 Tukey's definition of an outlier
#In R, points dalling outside the whiskers of the boxplot are calles outliers
#Top_whisker = 75th percentile + 1.5*IQR
#Bottom_whisker = 25th percentile - 1.5*IQR
Q_1 <- quantile(outlier_example, 0.25)
Q_3 <- quantile(outlier_example, 0.75)
IQR <- Q_3 - Q_1
r <- c(Q_1 - 1.5*IQR, Q_3 + 1.5*IQR)

#using pnorm(r), we realize that this covers 99.3% of the data
#Keep in mind that this is not such an extreme event: 
#if we have 1000 data points that are normally distributed, 
#we expect to see about 7 outside of this range. But these would not
#be outlerssince we expect to see them under typical variation.

#Using Tukey's far outliers
#With a normal distribution, 100% of the data falls in this interval
max_height <- quantile(outlier_example, 0.75) + 3*IQR(outlier_example)
#max_height should be 6.91

x <- outlier_example[outlier_example < max_height]
qqnorm(x)
qqline(x)



#Case study: self-reported student heights
data("reported_heights")

#Check the structure of the data
str(reported_heights)

#Converting heights from chr to numeric
reported_heights <- reported_heights %>%
  mutate(original_height = height, height = as.numeric(height))

#Lets see what is hapenning
reported_heights %>%
  filter(is.na(height))

#Removing all NA,s in the height table
reported_heights <- reported_heights %>%
  filter(!is.na(height))

#Computing summary statistics
reported_heights %>%
  group_by(sex) %>%
  summarise(mean = mean(height), std_dev = sd(height), 
            median = median(height), MAD = mad(height))

#boxplot
reported_heights %>%
  ggplot(aes(x = sex, y = height)) +
  geom_boxplot()


#Checking the extreme points using the extreme points
heights_IQR <-  3*IQR(reported_heights$height)
max_height <- quantile(reported_heights$height, 0.75) + heights_IQR  
min_height <- quantile(reported_heights$height, 0.25) - heights_IQR 
reported_heights %>%
  filter(!between(height, min_height, max_height)) %>%
  select(original_height) %>%
  .$original_height
  
#Observe the mistakes
  
  















