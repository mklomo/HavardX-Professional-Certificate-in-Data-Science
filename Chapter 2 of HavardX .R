#R Basics with HarvardX on edx
install.packages("dslabs")
library(dslabs)
library(tidyverse)
library(ggplot2)
data("murders")


#Visualisation of Murder Rate
ggplot(murders, aes(population, total, label=abb, color=region)) +
  geom_label()

#Learning  Objects using x^2+x-1 = 0
#of the form ax^2+bx+c
a <- 1
b <- 1
c <- -1

#Roots of the Equation
x_1 <- (-b+sqrt(b^2-4*a*c))/(2*a)
x_2 <- (b+sqrt(b^2-4*a*c))/(2*a)
print(c(x_1, x_2))



#Data Types
a <- 2
class(a)

library(dslabs)
#calling the data in murders
data(murders)
#checking the data type
class(murders)

#Examining the object
str(murders)

#We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("Italy", "Canada", "Egypt")
class(codes)
class(country)



#We can also name the elements of a numeric vector
codes <- c(Italy = 380, Canada = 124, Egypt = 818)
class(codes)

#Or a numeric vector of codes
codes <- c("Italy" = 380, "Canada" = 124, "Egypt" = 818)
class(codes)


##We can also use the names function
codes <- c(380, 124, 818)
country <- c("Italy", "Canada", "Egypt")
names(codes) <- country

#Using Square Brackets
codes[1]
codes[c(1,3)]
codes[1:2]


#If the entries are named, they may be accessed by referring to their name
codes["Canada"]
codes["Egypt"]

#Examining the murders dataset
head(murders)
length(murders$population)
class(murders$population)
class(murders$state)
class(murders$region)
levels(murders$region)

#Accessing columns using the $
murders$population

#Acessing variable names represented by columns
names(murders)

#Working with levels
region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
levels(region)

#Working with Matrices
mat <- matrix(1:20, 5, 4)
mat[2,] #Acessing the second row
mat[2,3] #Acessing the second row and the third column
mat[ , 4] #Acessing the entire fourth column
mat[ , c(1,4)] #Acessing the entire first and fourth columns
mat[ c(1,5), c(2,4)]

#We can convert matrices to dataframes using the as.data.frame
new_mat <- as.data.frame(mat)
cities <- c("Accra", "Tema", "Kumasi", "Ahafo")
names(new_mat) <- cities


##Exercises
#Column names
names(murders)

#Extract the abb column
a <- murders$abb
class(a)

#Extracting abbreviation
b <- murders$state

#Checking 
identical(a,b)

#Use of length and levels
length(levels(murders$region))

table(murders$region)


#Creating Sequences
seq(1, 10)
seq(0, 20, 3)
vec <- c(lab = 1:10)



#Coercion
x <- c(1, "Canada", 3)
class(x)

#Exercises 2.8
temp <- c("Beijing" = 35, "Lagos" = 88, "Rio de Janeiro" = 42, "San Juan" = 84, "Toronto" = 30)
city <- c("Beijing", "Lagos", "Rio_de_Janeiro", "San_Juan", "Toronto")

class(a <- 1L)

x <- c("1", "3", "5") 
class(x)


#Sorting orders arranges vectors in ascending 
x <- sort(murders$total)

#The order function returns the index of the input order
index <- order(murders$total)
murders$total[index]

#Order state by highest murder totals
murders$state[index]

#max and which.max
max(murders$total)  #returns the largest value in a vector
i_max <- which.max(murders$total) #returns the index/location of the largest 
murders$abb[i_max]



#Exercises
pop <- sort(murders$population)
pop[1] #Samllest population
index <- order(murders$population)
murders$population[index]
i_min <- which.min(murders$population)
murders$population[i_min]
murders$state[i_min]

ranks <- rank(murders$population)
state_rank <- murders$state[ranks]
my_df <- data.frame(state_rank, ranks) 

ord <- order(murders$population)
stat_ord <- murders$state[ord]
my_df <- data.frame(stat_ord, ord)


#Working with NAs
data("na_example")
str(na_example)
mean(na_example)
ind <- is.na(na_example)
table(ind)
mean(na_example, na.rm = TRUE)

#Alternative
mean(na_example[ind != TRUE])


#One code line showing largest state pop
murders$state[which.max(murders$population)]

#Rescaling a Vector
inches <- c(69, 62, 66, 70, 70, 73, 73, 67, 73, 67, 70)

#convert to centimeters
inches * 2.54

#Subtraction
inches - 69

#Murder rate per 100,000
murder_rate <- (murders$total/murders$population) * 100000
murder_rate[1:7]
mean(murder_rate)

#Which state has highest murder rate?
murders$state[which.max(murder_rate)]

#Exercise 
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)
Cels_temp <- (5/9)*(temp-32)

x <- 1:100
p <- 1/(x^2)
sum(p)



#Logicals
ind <- murder_rate < 0.71
murders$state[ind]
sum(ind)

#Logical Operators
west <- murders$region == "West"
safe <- murder_rate <= 1

#Define new index
ind <- safe & west
murders$state[ind]
sum(ind)


#Using the which function
ind <- which(murders$state == "California") #Gives index
murder_rate[ind]


#Using match
ind <- match(c("New York", "Florida", "Texas"), murders$state)
murder_rate[ind]

c("Boston", "Dakota", "Washington") %in% murders$state
which(murders$state %in% c("New York", "Florida", "Texas"))


#Exercise
low <- murder_rate < 1 #Logical Vector
sum(low)
ind <- which(low) #This returns the index/indices of the logical vector
murders$state[ind]

North_East <- murders$region == "Northeast"
log_ind <- North_East & low
sum(log_ind)
murders$state[log_ind]

log_ind <- murder_rate < mean(murder_rate)
sum(log_ind)

ind <- match(c("AK", "MI", "IA"), murders$abb)
murders$state[ind]

log_ind <- c("MA", "ME", "MI", "MO", "MU") %in% murders$abb
which(log_ind != TRUE)


#Basic Plots
x <- murders$population/(10^6)
y <- murders$total
plot(x,y)

#Or
with(murders, plot(population, total))

#Creating a Histogram
x <- with(murders, total/population *100000)
hist(x)
murders$state[which.max(x)]



#Boxplot
murders$rate <- with(murders, total/population *100000)
boxplot(rate ~ region, data = murders)



