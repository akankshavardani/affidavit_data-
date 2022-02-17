############ Project : Why do corrupt politicians get elected ################
## Author : Akanksha Vardani
## Start Date : 9 February 2022

#######Installing and loading Required Packages

# Package names
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "ggfortify", "DT", "reshape2", "knitr", "lubridate", "pwr","data.table","ggrepel","scales","fixest","xtable","Hmisc","haven",
              "RcmdrMisc","lmtest", "ggpubr", "dslabs", "stringr", "assist", "ggstatsplot", "forcats", "styler","magrittr", "tidyverse","statar","readxl","rdd",
              "pbapply","gsubfn","mgsub","fedmatch")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


## Setting the data upload and save paths
inpath_shrug <- "~/Dropbox/Datasets/SHRUG/"
outpath <- "~/Desktop/Research/Projects/affidavit_data-/data_analysis/"


## Loading the affidavit data
affidavit <- read.csv(paste0(inpath_shrug,"affidavits/shrug-v1.5.samosa-affidavits-csv/affidavits_clean.csv"))

# Loading the assembly elections data
candidates <- read.csv(paste0(inpath_shrug,"assembly_elections/shrug-v1.5.samosa-assembly-csv/assembly_candidates_clean.csv"))
elections <- read.csv(paste0(inpath_shrug,"assembly_elections/shrug-v1.5.samosa-assembly-csv/assembly_elections_clean.csv"))

# Loading the PMGSY data
roads <- read.csv(paste0(inpath_shrug,"pmgsy/shrug-v1.5.samosa-ancillary-csv/shrug_ancillary.csv")) ##the identifier in this data shr_id 
#- NEED to link with Constituency Data Tanushree does this!

## What do I need to find in the data first -
#1. How to know which candidate is for state and which for general elections - especially if both elections in the same year
###### SO shrug only has assembly election data; will have to get from trivedi site for general elections (To DO!!!)

#2. How many candidates repeat over time - to get a sense on incumbency 
#3. what all the variables mean 

#For 1 and 2. will need to link with the elections data 

#################################### Linking affidavit data and candidates data ####################################
## Linking the data using sh_cand_id

## will drop the observations from the candidates datsaet for election years before 2004 - to get a better sense of the match between the two datasets
candidates <- setDT(candidates)[year>=2004]
aff_cand <- statar::join(affidavit,candidates,kind="full",on=c("sh_cand_id"), gen="aff_cand")

aff_cand %>% count(aff_cand) ## only get 19,424 matches. 74,352 candidates from affidavit not matched - at least all these should be matches because even though 
## not all candidates might be submitting an affidavit all those that do must be running for elections

## do the year variables match for those that matched between the two datasets ?
aff_cand %>% 
  filter(aff_cand==3) %>% 
  count(year.x==year.y) ## yes

## do names match exactly ?
aff_cand %>% 
  filter(aff_cand==3) %>% 
  count(adr_cand_name==cand_name) ## True for 16118 and false for 3306

## how about age and sex ?
aff_cand %>% 
  filter(aff_cand==3) %>% 
  count(age.x==age.y) ## Not a perfect match

# can't match over sex as the variable does not exist in the affidavit data - weird ! But the JPE 2014 Fisman et al paper has that information - so maybe can map for some

## how do the matches vary by years
affid <- aff_cand %>% group_by(year.x) %>% count(aff_cand)
cand <- aff_cand %>% group_by(year.y) %>% count(aff_cand)
rm(affid,cand)

# creating a single variable for constituency name in the candidates data file

candidates %>% count(ac07_name=="")
# ac07_name == ""      n
# 1:           FALSE  32662
# 2:            TRUE 113679
candidates %>% count(ac08_name=="")
# ac08_name == ""      n
# 1:           FALSE 113679
# 2:            TRUE  32662

candidates[ac07_name=="",con_name := ac08_name][ac08_name==""&is.na(con_name),con_name := ac07_name]

## verifying
candidates %>% count(is.na(con_name))
candidates %>% count(is.na(tr_ac_name))

## is tr_ac_name same as con_name ?
sum(candidates$tr_ac_name!=candidates$con_name) # No
x <- candidates[which(candidates$tr_ac_name!=candidates$con_name)] ## similar - will use the tr_ac_name and so deleting con_name
setDT(candidates)[,con_name:=NULL]
rm(x)

## cleaning the state name, district name, constituency name and candidate names variables 
## keeping the same name for district name and constituency and candidate name variables 
setnames(affidavit,c("adr_district_name","adr_con_name","adr_cand_name"),c("district_name","con_name","cand_name"))
setnames(candidates,c("tr_district_name","tr_ac_name","cand_name"),c("district_name","con_name","cand_name"))

names <- c("pc01_state_name","district_name","con_name","cand_name")

setDT(affidavit)[,(names):=lapply(.SD,function(x) iconv(x,"US-ASCII","UTF-8")), .SDcols=names] ## the encoding of the candidate name variable was such that tolower was giving an error. ON terminal
## found the encoding tyoe and then converted that to UTF-8 and now the functiion works 

affidavit[,(names):=lapply(.SD,tolower), .SDcols=names][,(names):=lapply(.SD,trimws), .SDcols=names]
candidates[,(names):=lapply(.SD,tolower), .SDcols=names][,(names):=lapply(.SD,trimws), .SDcols=names]


## variables that we require exact match on - state name, district name, constituency name and year
## are the names of the state same across the two files ?
state_names_c <- candidates %>% distinct_at(c("pc01_state_name"))
state_names_a <- affidavit %>% distinct_at(c("pc01_state_name"))
which(!state_names_c$pc01_state_name%in%state_names_a$pc01_state_name) ## same 
rm(state_names_a,state_names_c)

## are the names of the districts same across the two files 

dist_names_c <- setDT(candidates %>% distinct_at(c("district_name")))
dist_names_c<-dist_names_c[order(district_name)]

dist_names_a <- setDT(affidavit %>% distinct_at(c("district_name")))[order(district_name)]

dist_not_in_c <- data.table(dist_names_c$district_name[which(!dist_names_c$district_name%in%dist_names_a$district_name)])
dist_not_in_a <- data.table(dist_names_a$district_name[which(!dist_names_a$district_name%in%dist_names_c$district_name)])
rm(dist_names_a,dist_names_c,dist_not_in_a,dist_not_in_c)
## will have to fuzzy match on district names too :(

## fuzzy matching on district names - keeping year and state name exact
districts_a <- unique(affidavit[,c("pc01_state_name","district_name")])
districts_c <- unique(candidates[,c("pc01_state_name","district_name")])

## creating a single variable that is of the form year-state-district_name
districts_a[,dist_ident:=paste0(district_name,"-", pc01_state_name)]
districts_c[,dist_ident:=paste0(district_name,"-",pc01_state_name)]

districts_a$id_a <- as.numeric(rownames(districts_a))
districts_c$id_c <- as.numeric(rownames(districts_c))

district_fuzzy <- merge_plus(districts_a,districts_c,by="dist_ident",
                             fuzzy_settings=build_fuzzy_settings(method = "lsc",maxDist = 0.8,matchNA = FALSE),
                             unique_key_1 = "id_a",
                             unique_key_2 = "id_c")

                             
dist_matches <- district_fuzzy$matches[order(pc01_state_name_1)]
nomatch_a <- district_fuzzy$data1_nomatch[order(pc01_state_name)]
nomatch_c <- district_fuzzy$data2_nomatch[order(pc01_state_name)]
## constituency 
