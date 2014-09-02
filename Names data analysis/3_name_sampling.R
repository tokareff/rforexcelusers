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


## package the sampling and duplicate check into a function
sample_is_duplicated = function(size_val) {
  x = sample(names_pool$name, size = size_val, 
             replace = TRUE, prob = names_pool$prob)
  sum(duplicated(x)) > 0
}


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
