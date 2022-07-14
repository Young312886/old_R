##Packages##

(.packages()) #List the packages currently loaded

#Want to count the nmber of letter in a string##
install.packages('stringr')
library(stringr)#activating the packages#

example(str_length)
str_length('apple')

update.packages('stringr')
remove.packages('stringr')

rm(list=ls())
Apps <- read.csv("Apps_Rating.csv")
Rankings <- read.csv("Apps.csv")
