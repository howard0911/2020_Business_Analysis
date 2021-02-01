#####-------------------------------------------------------
##### Business Analytics
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 1, 15 Sep 2020
#####-------------------------------------------------------

### Introducing R ###

# To simplify, we assume that the data files are saved in the default directory of your R. To know what your current
# default directory is, type "getwd()". To change it as desired, type "setwd("D:/R work")", for example.

setwd("D:/R work")

# Value assign:
n <- 15

# Case sensitive:
x <- 1
X <- 10

# Value replace:
n <- 10 + rnorm(10)

# Need some help?
help(rnorm)
?rnorm

help.start()


# Load package:
library(MASS)

# Install package:
install.packages('fBasics')

# Inspect packages currently loaded:
search()


# List the objects in memory:
ls()
name <- "Carmen"
n1 <- 10; n2 <- 100; m <- 0.5
ls(); ls(pattern="m"); ls(pat="^m")
ls.str()

# Delete objects:
rm()
rm(n1)
rm(list=ls())


# Vector:
x1 <- c(0:7)			# x0 <- c(1:3, 8:10)
( x2 <- rep(2, 8) )
( x2 <- rep(1:4, 2) )
( x2 <- rep(1:4, each = 2) )
( x3 <- seq(-1, 1, 0.2) )	# evenly spaced sequence

# Matrix:
( x4 <- rnorm(12) )
length(x4)
dim(x4) <- c(4,3)
x4

# Aggregate ¡V cbind / rbind:
( u <- c(1:3));  ( v <- c(-1:-3) )
( m <- rbind(u, v) )

# Matrix indexing:
m[2,2];  x4[1,3]
x4[2,]


# Measures of Association for Continuous Variables
cov(x1, x2)
cor(x1, x2)
var(x4)
cor(x4)
sd(x4)^2

mean(x4)

cov(x4[,1], x4[,2])



