#Using dplyr package
#includes "mutate", "filter" , "subset" and the pipe operator "%>%"
library(dplyr)
murders <- mutate(murders, rate = (total/population)*100000)
head(murders)

#Using the filter funtion 
#Filter is for rows
filter(murders, rate <= 0.71)

#using select 
#Select is for columns
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

#Using the pipe operator
murders %>% 
  select(state, region, rate) %>%
  filter(rate <= 0.71)

#Creating Dataframes in dplyr using data_frame to create a tibble
grades <- data_frame( names = c("John", "Juan", "Jean", "Yao"), exam_1 = c(95, 80, 90, 85), exam_2 = c(90, 85, 85, 90))
grades
str(grades)
class(grades$names)


#Now in subsetting its we work with dataframes and not just vectors
#Hence the use of tidy data format and the tidyverse package
library(tidyverse)
library(dplyr)
library(dslabs)
data("murders")
head(murders)
murders <- mutate(murders, rate = total/population *100000)
murders <- mutate(murders, rank = rank(-murder_rate))
murders_new_york <- filter(murders, state == "New York")
no_south <- filter(murders, region != "South")
nrow(no_south)
filter(murders, state %in% c("New York", "Texas"))

murders_nw <- filter(murders, region %in% c("Northeast", "West"))

murders_nw <- filter(murders, region %in% c("Northeast", "West") & rate < 1)
head(murders_nw)

#summarize & group_by
library(dslabs)
library(dplyr)
data("heights")
head(heights)

s <- heights %>%
  filter(sex == "Male") %>%
  summarise(average = mean(height), standard_deviation = sd(height)) %>%
  pull(average)

#Using group_by
heights %>%
  group_by(sex) %>%
  summarise(average = mean(height), stand_dev = sd(height))
murders %>%
  group_by(region) %>%
  summarise( median_rate = median(rate))

#Using the arrange function to order a dataframe
murders %>%
  arrange(-rank, state) %>%
  head()


murders %>%
  arrange(rate) %>%
  head()


#Using the top_n function
murders %>%
  top_n(10, rate)


#Exercises
library(NHANES)
data("NHANES")
head(NHANES)


#To ignore NA's in the dataset, we use the na.rm argument

ref <- NHANES %>%
  filter(AgeDecade == " 20-29") %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE))

ref_avg <- ref %>%
  pull(average)

#Reporting min and max
ref <- NHANES %>%
  filter(AgeDecade == " 20-29") %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE), minimum = min(BPSysAve, na.rm = TRUE), maximum = max(BPSysAve, na.rm = TRUE))

females <- NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE), minimum = min(BPSysAve, na.rm = TRUE), maximum = max(BPSysAve, na.rm = TRUE))

  
males <- NHANES %>%
  filter(Gender == "male") %>%
  group_by(AgeDecade) %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE), minimum = min(BPSysAve, na.rm = TRUE), maximum = max(BPSysAve, na.rm = TRUE))

Gender <- NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE), minimum = min(BPSysAve, na.rm = TRUE), maximum = max(BPSysAve, na.rm = TRUE))



Males_Comp <- NHANES %>%
  filter(Gender == "male", AgeDecade == " 40-49") %>%
  group_by(Race1) %>%
  arrange(BPSysAve) %>%
  summarise(average = mean(BPSysAve, na.rm = TRUE), stand_dev = sd(BPSysAve, na.rm = TRUE), minimum = min(BPSysAve, na.rm = TRUE), maximum = max(BPSysAve, na.rm = TRUE))



#Tibble vs dataframe
as_tibble(murders)
class(murders[,4])
class(as_tibble(murders)[,4])

#Using the case_when function
x <- c(-2, -2, 0, 1, 2, 3, 4, 5)
case_when(x < 0 ~ "Negative",
          x > 0 ~ "Positive",
          x == 0 ~ "Zero")
murders %>%
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South", 
    TRUE ~ "Other"))%>%
  group_by(group) %>%
  summarise(rate = sum(total)/sum(population)*10^5)

#Exercise 4.15
data("murders")
murders_tibble <- as_tibble(murders)

murders_tibble %>%
  group_by(region) %>%
  summarise(rate = sum(total)/sum(population) *10^5) %>%
  arrange(rate)














