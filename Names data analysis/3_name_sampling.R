### Blog post: bootstrap resampling with R
### Author: jtaveras
### Data source: data created in blog post 1, using http://www.ssa.gov/OACT/babynames/limits.html


library(dplyr)

## run code that created data in blog post 1
source("/Users/johnt/Google Drive/R Blog/code/Names data analysis/1_combine_csv_files.R")


## create pool of names from which to sample
names_pool = filter(files, year==2013, gender=="M") %.%
             group_by(name) %.% 
             summarise(births = sum(count)) %.% 
             arrange(desc(births))
names_pool$prob = names_pool$births / sum(names_pool$births)
head(names_pool)


### TRY OUT THE SAMPLING FUNCTION

## how to sample 30 names with given probabilities
name_sample = sample(names_pool$name, size = 30, 
                     replace = TRUE, prob = names_pool$prob)
sort(name_sample)

## count and see duplicated names
sum(duplicated(name_sample))
name_sample[duplicated(name_sample)]



### AUTOMATE THE SAMPLING

## package the sampling and duplicate test into a function for streamlined use
sample_is_duplicated = function(size_val) {
  x = sample(names_pool$name, size = size_val, 
             replace = TRUE, prob = names_pool$prob)
  sum(duplicated(x)) > 0
}

## use sapply to run function over size_val of 1:100
sapply(X = 1:100, FUN = sample_is_duplicated)


## run sapply 1000 times.  
x = replicate(n = 1000, sapply(X = 1:100, FUN = sample_is_duplicated))
probabilities = apply(x, 1, mean)


## plot result
plot(probabilities, type="l", 
     xlab = "Sample Size", ylab = "Probability of Duplicate",
     main = "Cumulative probability of duplicate names")



## DIFFERENT QUESTION: how many duplicates will you find for different sample sizes?

### tweak earlier function slightly.  Return number of duplicates instead of TRUE/FALSE
sample_duplicates = function(size_val) {
  x = sample(names_pool$name, size = size_val, 
             replace = TRUE, prob = names_pool$prob)
  sum(duplicated(x))
}

## run sample_duplicates 10,000 times with  a sample size of 100
sample_100 = replicate(n = 10000, sapply(X = 100, FUN = sample_duplicates))
hist(sample_100, 
     main = "Histogram: duplicate names in 100-person party",
     xlab = "Number of Duplicates")





## disregard
stats_2013 = data.frame(num = 1:100, allGirls = probabilities)
stats_2013 = cbind(stats_2013, data.frame(allBoys = probabilities))

# plot(gvisLineChart(stats_2013, xvar = "num", yvar = c("allBoys","allGirls","Coed"),
#      options = list(width=450, height=350,
#                     title = "Probability of duplicate names",
#                     hAxis = "{title: 'Party Size'}",
#                     vAxis = "{title: 'Probability of Duplicate'}")))



### appendix: birthday paradox
birthdays = data.frame(birthday = 1:365)
birthday_sample = function(size_val) {
#   size_val = 5
  x = sample(birthdays$birthday, size = size_val, replace = TRUE)
   sum(duplicated(x)) > 0
#   sum(duplicated(x)) > 0
}

n = 1000
x = replicate(n, sapply(X = 2:70, birthday_sample))
y = apply(x, 1, mean)
plot(y, type="l",
     xlab = "Party Size", ylab = "Probability of Duplicate Birthday",
     main = "Cumulative probability of duplicate birthdays")
lines(y)
data.frame(num = 2:70, prob = y)

