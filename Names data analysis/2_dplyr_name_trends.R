### Blog post: high level name trends using dplyr
### Author: jtaveras
### Data source: data created in blog post 1, using http://www.ssa.gov/OACT/babynames/limits.html

library(dplyr)
library(googleVis)
library(reshape2)


## load the data created in prior blog post
source("/Users/johnt/Google Drive/R Blog/rforexcelusers/Names data analysis/1_combine_csv_files.R")

str(files)

### Question: how have number of distinct names trended over time?

## count number of distinct names, names per million births and number of births by year
names_count = group_by(files, year, gender) %.% 
  summarise(unique_names = n_distinct(name),
            names_per_1M = n_distinct(name) / (sum(count) / 1000000),
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






## QUESTION 2: Name density, what % of population covered by the top 50 names, over time?

# flag top 50 names by year, state and gender
top_by_state = group_by(files, year, state, gender) %.% 
  mutate(group = ifelse( rank(desc(count), ties.method = "first") <= 50, "TopX", "Other"))

# summarize to the national level
top_national = group_by(top_by_state, gender, year, group) %.%
  summarise(births = sum(count))

# transpose gender and group columns
top_national = dcast(top_national, year ~ gender+group, value.var = "births")

# calculate % of population with top 50 names
top_national$M_topx_pct = top_national$M_TopX / (top_national$M_Other + top_national$M_TopX)
top_national$F_topx_pct = top_national$F_TopX / (top_national$F_Other + top_national$F_TopX)

# quick data check
head(top_national)

# plot % of newborns having the top 50 names
plot(gvisLineChart(top_national, xvar = "year", yvar = c("M_topx_pct","F_topx_pct"), 
                   options = list(title = "% of newborns having the top 50 names", 
                                  vAxis="{minValue:0}",
                                  width=450, height=350)))

