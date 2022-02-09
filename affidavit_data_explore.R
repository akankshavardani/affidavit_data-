############ Project : Why do corrupt politicians get elected ################
## Author : Akanksha Vardani
## Start Date : 9 February 2022

## Loading the libraries 
library(data.table) 
library(ggplot2) 
library(fixest)
library(xtable)
library(Hmisc)
library(rdd)
library(lubridate)
library(stringr)
library(haven)
library(statar)
library(tidyverse)
library(readxl)
library(pblapply)
library(gsubfn)
library(mgsub)

## Setting the data upload and save paths
inpath_shrug <- "~/Dropbox/Datasets/SHRUG/"
outpath <- "~/Desktop/Research/Projects/affidavit_data-/data_analysis/"


## Loading the affidavit data
affidavit <- read.csv(paste0(inpath_shrug,"affidavits/shrug-v1.5.samosa-affidavits-csv/affidavits_clean.csv"))

## What do I need to find in the data first -
# How to know which candidate is for state and which for general elections - especially if both elections in the same year
# How many candidates repeat over time - to get a sense on incumbency 
# what all the variables mean 
