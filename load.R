#Load Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(quantmod)
library(scales)
library(lubridate)

source("./common/eg_theme.R")
data <- read.csv("data.csv", sep="")