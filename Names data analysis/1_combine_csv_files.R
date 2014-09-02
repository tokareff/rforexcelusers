### Blog post: combining CSV files with R
### Author: jtaveras
### Data source: http://www.ssa.gov/OACT/babynames/limits.html


## get all the appropriate file names
setwd("/Users/johnt/Google Drive/R Blog/raw_data/namesbystate/")
file_names = list.files(getwd())
file_names = file_names[grepl(".TXT",file_names)]

## check that I have the right read.csv parameters
files = read.csv("WY.TXT", header=F)

## run read.csv across all file_names
files = lapply(file_names, read.csv, header=F, stringsAsFactors = F)
files = do.call(rbind,files)
names(files) = c("state", "gender", "year", "name", "count")

## quick data check
str(files)
head(files)


