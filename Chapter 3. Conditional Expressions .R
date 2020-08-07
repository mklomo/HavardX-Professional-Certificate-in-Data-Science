#Conditional Expressions
a <- 0

if(a != 0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}


library(dslabs)
data("murders")
murder_rate <- with(murders, total/population *100000)

ind <- which.min(murder_rate)

if(murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}

#Using ifelse
a <- c(0, 1, 2, 3, 4, 5)
result <- ifelse(a > 0, 1/a, NA)

#using na_example data
data("na_example")
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))
log_index <- is.na(na_example) 
sum(log_index)
subset <- na_example[!log_index]




#Defining functions
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

p <- 1:2000
identical(mean(p), avg(p))

avg <- function(x, arithmetic = TRUE){
  s <- sum(x)
  n <- length(x)
  ifelse(arithmetic, s/n, prod(x)^(1/n))
}

#For Loops

compute_s_n <- function(x){
  s <- 1:x
  sum(s)
}

#Using a for loop
m <- 1:25
s_n <- vector(length = 25) #Creating an empty vector
for(i in m){
  s_n[i] <- compute_s_n(i)
  print(s_n)
}

#simplest example of a for-loop
for(i in m){
  print(i)
}

plot(m, s_n)


x <- c(1, 2, -3, 4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not all Positives")
}

new_names <- ifelse(nchar(murders$state) >= 8, murders$abb, NA)
log_ind <- is.na(new_names)
new_names[log_ind != TRUE]

sum_n <- function(x){
  y <- 1:x
  sum(y)
}


x <- 5000
sum_n(x)

altman_plot <- function(x,y){
  difference <- x-y
  addition <- x+y
  plot(difference, addition)
}


x <- 1:50
y <- 26:75
altman_plot(x,y)

x <- 3
my_func <- function(y){
  x <- 5
  y+5
}

compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}


s_n <- vector("numeric", length = 25)
k <- 1:25
for(i in k){
  s_n[i] <- compute_s_n(i) 
}


#Use of Functionals "sapply"
x <- 1:30
n <- sapply(x, compute_s_n)
#Using a tibble df from dplyr
data_frame(x, n)

plot(x,n)
lines(x, x*(x+1)*(2*x+1)/6)






