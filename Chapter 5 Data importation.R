#fullpath
wd <- getwd()

#using the readr and readxl packages
library(readr)
read_lines("US_sample_data.csv", n_max = 5)
US_Height_Data <- read_csv("US_sample_data.csv")
View(US_Height_Data)
names(US_Height_Data)

library(readxl)
excel_sheets("Compiled DDS dataset.xls")
Bronx <- read_excel("Compiled DDS dataset.xls", sheet = "Bronx", skip = 4)
head(Bronx)
Staten_Island <- read_excel("Compiled DDS dataset.xls", sheet = "Staten Island", skip = 4)
head(Staten_Island)
Queens <- read_excel("Compiled DDS dataset.xls", sheet = "Queens", skip = 4)
head(Queens)
Manhattan <- read_excel("Compiled DDS dataset.xls", sheet = "Manhattan", skip = 4)
head(Manhattan)
Brooklyn <- read_excel("Compiled DDS dataset.xls", sheet = "Brooklyn", skip = 4)
head(Brooklyn)
#Combining all sheets in the data set with the Header as the joint 
library(dplyr)
Compile_DDS <- rbind(Bronx, Staten_Island, Queens, Manhattan, Brooklyn) %>%
  arrange(BOROUGH)

#Downloading datasets but use API's instead
health_data_url <- "http://data.humdata.org/dataset/039ad0f5-7ffa-414a-82cf-0bf3cfd59a6b/resource/47cebfc5-1bcf-49d5-a22a-db1155751e58/download/health_indicators_gha.csv" 
read_lines(health_data_url, n_max = 10)
health_data <- read_csv(health_data_url)
head(health_data)













