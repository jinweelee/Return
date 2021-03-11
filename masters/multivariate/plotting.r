library(mvtnorm)
library(ICSNP)
library(jocre)
library(matlib)
library(data.table)
library(ggplot2)
library(scatterplot3d)
library(andrews)

rm(list=ls())

#
data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis"
seed_data <- fread(file.path(data_dir, "seeds.txt"))
seed_dt <- seed_data[,!c("group")]
head(seed_data)
head(seed_dt)

data(USairpollution, package="HSAUR2")
head(USairpollution)

# Plots
#pairs(seed_dt)
#bubbleplot
ggplot(seed_dt, aes(x=area, y=perimeter, size = asym)) + 
  geom_point(alpha=0.5)
#3D scatter plot
#star plot


?andrews

head(seed_data)
#andrews(seed_data, type =1, clr = 8) clr = column idx to color






