### RVC survey data script 1

### Set Up ------------------------------------------------------------------

# load packages and libraries
remotes::install_github("ropensci/rfishbase")
library("rfishbase")
library("dplyr")
library("duckdb")

library(tidyverse)
library(easypackages)
libraries("sp", "tidyr", "dplyr", "readr", "tibble", "stringr", "truncnorm",
          "conflicted", "devtools", "readxl", "purrr", "reshape2","rvc","ggplot2","labdsv","rfishbase")

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distinct", "dplyr")

# Clearing the workspace
rm(list=ls())

# Reading in the dataset:
rvc1 <- read_csv("RVC_data_no_repeats.csv")
length_weight<-read_csv("Length_weight.csv")
View(length_weight)

#read rvc1 as tibble
rvc1 <- as_tibble(rvc1)

### Data tidying -----------------------------------------------------------------

##Checking for misspellings and duplicates in the columns:
sort(unique(rvc1$Family))
sort(unique(rvc1$Species))

## deleting the 'Site_plot_date' column:
rvc2<-select(rvc1,-c(Site_plot_date))

# adding a column for unique survey ID:
rvc_IDs <- rvc2 %>% 
  unite ("ID_SURV",c(Site,'Plot Type',Date),sep = "_", remove = FALSE)

# Renaming columns:
rvc_IDs <- rename(rvc_IDs,
                  Fish_no=`# Fish`,
                  Avg_size=`Avg Size`,
                  Min_size=`Min Size`,
                  Max_size=`Max Size`,
                  Plot_type=`Plot Type`,
                  Plot_no = `Plot #`,
                  Visit = `Visit (time since outplanting completed)`)

# replacing the NA's in 'Fish_no', 'Avg_size', 'Min_size' and 'Max_size' with 0:

rvc_IDs2<-rvc_IDs %>%  
  mutate(Fish_no = ifelse(is.na(Fish_no),0,Fish_no),
          Avg_size = ifelse(is.na(Avg_size),0,Avg_size),
          Min_size = ifelse(is.na(Min_size),0,Min_size),
          Max_size = ifelse(is.na(Max_size),0,Max_size))

# reading 'Fish_no', Avg_size', 'Min_size' and 'Max_size' as numeric:

rvc_IDs2<-rvc_IDs2 %>% 
  mutate(Fish_no = as.numeric(Fish_no),
        Avg_size = as.numeric(Avg_size),
        Min_size = as.numeric(Min_size),
        Max_size = as.numeric(Max_size))

# reading 'Visit','Family','Species' and 'ID_SURV' as factors:
rvc_IDs2<-rvc_IDs2 %>% 
  mutate(Visit= as.factor(Visit),
         Family= as.factor(Family),
         Species = as.factor(Species),
         ID_SURV = as.factor(ID_SURV))

## deleting the 'Site', 'Project', 'Plot_type','Date' and 'Plot_no' columns:
rvc_IDs3<-select(rvc_IDs2,-c("Site","Project","Plot_no","Plot_type","Date"))

rvc<-rvc_IDs3

### Data exploration -----------------------------------------------------------------

# Building histograms:
hist(rvc$Fish_no) #all very zero-inflated
hist(rvc$Avg_size)
hist(rvc$Min_size)
hist(rvc$Max_size)

# Building boxplots:
# elasmobranch (and barracuda) sway the avg / max / min size

ggplot(rvc,aes(Family,Fish_no))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL

ggplot(rvc,aes(Family,Avg_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL

ggplot(rvc,aes(Family,Min_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL

ggplot(rvc,aes(Family,Max_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL

# Building new dataset without elasmobranchs or barracuda:
rvc <- rvc %>% 
  filter(Family != "Elasmobranch")
rvc <- rvc %>% 
  filter(Family != "Barracuda")

View (rvc)

# re-building boxplots:

ggplot(rvc,aes(Family,Fish_no))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL
# highest number of damselfish, grunts and wrasses
# then jackfish, parrotfish, snapper and surgeonfish

ggplot(rvc,aes(Family,Avg_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL
# filefish has largest average size

ggplot(rvc,aes(Family,Min_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL
# filefish has largest min size, followed by trumpetfish

ggplot(rvc,aes(Family,Max_size))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  NULL
# filefish has largest max size, followed by trumpetfish

# Pairplot 
pairs(rvc)

### Log transformation histograms of variables:----------------------------------------------------------------

ggplot(rvc,aes(x=log(Fish_no)))+
  geom_histogram()
# still quite right-skewed

ggplot(rvc,aes(x=log(Avg_size)))+
  geom_histogram()
# more normally distributed, slightly left skewed

ggplot(rvc,aes(x=log(Max_size)))+
  geom_histogram()
# more normally distributed, slightly left skewed

ggplot(rvc,aes(x=log(Min_size)))+
  geom_histogram()
# more normally distributed

### Creating new columns with log transformations:---------------------------------------------------------------

rvc<-rvc %>% 
  mutate(Log_Ave_size = log(Avg_size))
rvc<-rvc %>% 
  mutate(Log_Max_size = log(Max_size))
rvc<-rvc %>% 
  mutate(Log_Min_size = log(Min_size))

View(rvc)

# Unit conversions ---------------------------------------------------------------

# Convert Fish_no (fish per 176.7 m^2) to Density (fish per 100 m2):

rvc2<-rvc %>% 
  mutate(Density=(100 * Fish_no)/(3.14*7.5^2))

View(rvc2)

# Convert fish data to biomass:
fish <- validate_names(c("Pomacanthus arcuatus","Holacanthus tricolor","Pomacanthus paru","Holacanthus ciliaris", "Sphyraena barracuda"))



# https://www.davidzeleny.net/anadat-r/doku.php/en:confusions 

###### hello
 