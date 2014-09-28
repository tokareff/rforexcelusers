library(dplyr)
library(reshape2)
library(ggplot2)


#### STEP 1 - DATA PREP - 
#### Goal: Create a wide dataset, with one row per name and 104 columns, one for year year, holding the desired metric.
#### Excel users: basically create a "wide" data set with a pivottable, with Name as row, Year as column, and the calculated matrix
#### Lots of dplyr functions to shape data how I need it 

# Load data set we created in the first baby names blog post
load("/Users/johnt/Google Drive/R Blog/rforexcelusers/Names data analysis/files.RData")

# Create summary with annual birth counts by name.
names_summary = group_by(files, gender, name, year) %.%
                summarise(births = sum(count))

# Calculate yearly "popularity" of each name
# popularity = % of babies ofannual births for a name divided by total births in that year for that gender
names_summary = group_by(names_summary, gender, year) %.%
  mutate(popularity = births / sum(births))

# For each name, calculate its relative popularity in each year; relative to the its unweighted mean popularity
# i.e., if 10% of boys were named X in 1950 and on average (over 104 years) 5% of boys were named X, then the pop_pct = 10%/5% = 2.0
names_summary = group_by(names_summary, gender, name) %.%
  mutate(popularity_pct = popularity / mean(popularity)) %.%
  arrange(gender, name, year)

# Find top 1000 names all-time, for each gender
top_1000_names = group_by(names_summary, gender, name) %.% 
  summarise(births = sum(births)) %.% 
  group_by(gender) %.% 
  mutate(rank = rank(desc(births)),
         cum_pct = round(cumsum(births) / sum(births),4)) %.%
  filter(rank <= 1000)  %.% arrange(rank)

# Check # of births accounted for by top 1000 names
sum(filter(top_1000_names, gender=="F")$births) / sum(filter(files, gender=="F")$count)
sum(filter(top_1000_names, gender=="M")$births) / sum(filter(files, gender=="M")$count)


# Keep name summary data for only the top 1000 names.  merge() by default keeps rows matching in both data frames
names_summary = merge(names_summary, select(top_1000_names, gender, name))


# Transpose data frame to create wide matrix
names_matrix = melt(names_summary, id.vars = c("gender","name", "year"), measure.vars = c("popularity_pct"))
names_matrix = dcast(names_matrix, gender+name ~ year+variable, value.var = "value", fill = 0)

head(names_matrix)



#### STEP 2 - CLUSTERING
#### run hclust() function.  

names_matrix_clust = filter(names_matrix, gender == "M")

# keep only columns with "popularity_pct" in name
names_matrix_clust = names_matrix_clust[,grepl("popularity_pct", names(names_matrix_clust))]

#create distance matrix and run cluster function
#selected ward method to force fairly equal sized clusters
names_matrix_dist = dist(names_matrix_clust)
h = hclust(names_matrix_dist, method="ward.D")

# plot(h) #not pretty given large number of clustered items


### create copy of cleaned names_matrix and append the assigned cluster number
names_matrix_copy = filter(names_matrix, gender == "M")

# append assigned cluster number to each name (arbitrarily picked 15 clusters)
names_matrix_copy$cluster_15 = cutree(h, k=15)
table(names_matrix_copy$cluster_15)

names_matrix_copy = melt(names_matrix_copy, id.vars=c("gender","name","cluster_15"), measure.vars=c(names(names_matrix_copy)[grepl("popularity_pct",names(names_matrix_copy))]),
                         value.name = "popularity_pct")
names_matrix_copy$year = as.numeric(substr(names_matrix_copy$variable, 1,4))





#### STEP 3 - VISUALIZE CLUSTERS
#### a time series chart with confidence interval

## Create cluster summary table
  ## For each cluster, calculate the median popularity_pct each year, and +/- 1 median absolute deviation
  ## For the +/- 1 MAD range, didn't let the range go lower than the actual minimum, or higher than the actual maximum.  
names_matrix_summ = group_by(names_matrix_copy, cluster_15, year) %.% 
  summarise(
    popularity_pct_avg = median(popularity_pct),
    popularity_pct_mad = mad(popularity_pct),
    popularity_pct_min = max(min(popularity_pct), median(popularity_pct) - 1.0*mad(popularity_pct)),
    popularity_pct_max = min(max(popularity_pct), median(popularity_pct) + 1.0*mad(popularity_pct))
  )

## graph a single cluster with confidence range
cluster = 7
ggplot(filter(names_matrix_summ, cluster_15 == cluster), aes(x=year, y=popularity_pct_avg)) + geom_line() +
  geom_smooth(aes(ymin=popularity_pct_min, ymax=popularity_pct_max), stat="identity") + theme_bw() + 
  labs(x="Year", y="Median Pop Pct")

## list the names within the cluster
paste(unique(filter(names_matrix_copy, cluster_15==cluster)$name), collapse=", ")

## graph 15 random names within the cluster
# rand_names = sample(unique(filter(names_matrix_copy, cluster_15 == cluster)$name), 15)
# cluster_names = filter(names_matrix_copy, name %in% rand_names)
# ggplot(cluster_names, aes(x=year, y=popularity_pct, group=name)) + geom_line()






###### EXTRAS

#### QUICK PLOTS
#### Look at a few time series.  And add up number of births on file

plot_name = function(x, gender_val, metric="popularity_pct") {
  t = filter(names_summary, name==x, gender==gender_val) %.% arrange(year)
  # want charts to have full date range, even if name did not exist for certain years
  years = data.frame(year=1910:2013)
  t = merge(years, t, all.x=TRUE)
  print(sum(t$births, na.rm=TRUE))
  plot(x = t$year, y=t[,metric], type="l",
       main=paste(x), xlab="",ylab="")
}

plot_name("Woodrow", "M")
plot_name("Brantley", "M")


#### IDENTIFY FAR-APART names based on distance matrix
# find top 5 distant pairs
minmax = sort(names_matrix_dist)[(length(names_matrix_dist)-4):length(names_matrix_dist)]
which(as.matrix(names_matrix_dist) == minmax[5], arr.ind=TRUE)
filter(names_matrix, gender == "M")[c(987,107),"name"] #farthest boys #woodrow, brantley


#### RANK-ORDER THE CLUSTERS BASED ON WEIGHTED AVERAGE PEAK YEAR
head(names_matrix_summ)
peaks = group_by(names_matrix_summ, cluster_15) %.%
  summarise(mean_year = round(sum(year*popularity_pct_avg)/sum(popularity_pct_avg),0))
counts = group_by(names_matrix_copy, cluster_15) %.%
  summarise(name_count = n_distinct(name))
peaks = merge(peaks, counts) %.%
  arrange(mean_year)
peaks