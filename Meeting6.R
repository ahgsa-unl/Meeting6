# R Club Meeting #6 - 10/11

# 1. Using RStudio Projects for data management
# Create subfolders and project
# Open and save a script to code folder
# Add cotton.csv to data folder

# Installing libraries (if not installed yet)
install.packages("dplyr")
install.packages("tidyr")
install.packages("magrittr")
install.packages("ggplot2")

# Load packages (needed everytime a new R session is opened)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

# Reading in cotton dataset
cotton<- read.csv("data/cotton.csv")

# Transforming lineage into factor
cotton$lineage<- factor(cotton$lineage)

# 2. Intro to dplyr and tidyr
# Filter site=Lima, lineage=2 using filter() (works on rows)
cotton2<- filter(cotton, site=="Lima", lineage=="2")

# Select only site and yield variables using filter() (works on columns)
cotton3<- select(cotton, site, yield) 

# Transform yield to kg/ha (assuming it is in lbs/ac) using mutate()
cotton_kgha<- mutate(cotton, yield_kgha=yield*0.453592/0.404686)

# Unite site and epoca into one variable unite()
cotton4<- unite(cotton, site_epoca, site, epoca)

# 3.	Using dplyr and tidyr to calculate means 
# and standard error of the mean (SEM)

# Getting yield means and SEM for site
site_means <- group_by(cotton, site)

class(cotton)
class(site_means)

site_means2<- summarise(site_means, mean=mean(yield),
                        sem=sd(yield)/sqrt(length(yield)),
                        n=length(yield))

# 4. Using magrittr for function piping
site_means_piped<- cotton %>% 
  group_by(site) %>% 
  summarise(mean=mean(yield),
            sem=sd(yield)/sqrt(length(yield)),
            n=length(yield))

# Challenge:
# Getting yield means (kg/ha) and SEM for site x lineage
site_lineage_means <- cotton %>% 
  mutate(yield_kgha=yield*0.453592/0.404686) %>%
  group_by(site, lineage) %>% 
  summarise(mean=mean(yield_kgha),
            sem=sd(yield_kgha)/sqrt(length(yield_kgha)),
            n=length(yield_kgha)) %>%
  unite(site_lineage, site, lineage, remove = F)

# Ploting means and SEM with site_lineage
ggplot(data=site_lineage_means, aes(x=site_lineage, y=mean, color=site))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.3, color="black")+
  geom_point(size=3)+
  ggsave("output/site_lineage_means.png")

# Ploting means and SEM with site and lineage
ggplot(data=site_lineage_means, aes(x=site, y=mean, color=site))+
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.3, color="black")+
  geom_point(size=3)+
  facet_grid(~lineage)
