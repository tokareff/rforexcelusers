### Blog post: high level name trends using dplyr
### Author: jtaveras
### Data source: data created in blog post 1, using http://www.ssa.gov/OACT/babynames/limits.html

library(dplyr)
library(googleVis)
library(reshape2)


## load the data created in prior blog post
source("/Users/johnt/Google Drive/R Blog/code/Names data analysis/1_combine_csv_files.R")



### Question: how have number of distinct names trended over time?

## count number of distinct names, names per million births and number of births by year
names_count = group_by(files, year, gender) %.% 
  summarise(unique_names = n_distinct(name),
            names_per_1M = n_distinct(name) / (sum(count) / 10000000),
            births = sum(count))

## quick data check
names_count[1:10,]

## transpose gender column like pivot table
uniq_names_count = dcast(names_count, year ~ gender, value.var="unique_names")
names_per_1M = dcast(names_count, year ~ gender, value.var="names_per_1M")
# births = dcast(names_count, year ~ gender, value.var="births")

## quick data check
names_count[1:10,]


## graph trends
## could be turned into a function to minimize repetition

g1 = gvisLineChart(uniq_names_count, xvar="year", yvar=c("F","M"),
                   options=list(title="# of Distinct Names",
                                hAxis="{title: 'Year'}",
                                vAxis="{title: 'Number of Names'}", width=450,height=350))
g2 = gvisLineChart(names_per_1M, xvar="year", yvar=c("F","M"),
                   options=list(title="Distinct Names per Million Births",
                                hAxis="{title: 'Year'}",
                                vAxis="{title: 'Number of Names'}", width=450,height=350))

plot(gvisMerge(g1, g2, horizontal = TRUE))
