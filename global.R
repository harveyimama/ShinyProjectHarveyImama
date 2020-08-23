setwd('/Users/harveyimama')
population.NY = read.csv('Annual_Population_Estimates_for_New_York_State_and_Counties__Beginning_1970.csv')
tonnage.NY = read.csv('DSNY_Monthly_Tonnage_Data.csv')

library('ggplot2')
library('dplyr')

convert_name <- function(name){
  if(name == 'Kings')
    newname = 'Brooklyn'
  else if (name == 'Richmond')
    newname = 'Staten Island'
  else if (name == 'New York')
    newname = 'Manhattan'
  else
    newname =  name
  return(newname)
}

population.NY.clean = filter(population.NY,Geography == 'Bronx County' | Geography ==  'Queens County' | Geography ==  'New York County' | Geography ==  'Kings County' | Geography ==  'Richmond County') %>%
  mutate (.,BOROUGH = gsub(' County', '',Geography))

population.NY.clean$BOROUGH = sapply(population.NY.clean$BOROUGH,convert_name)

tonnage.NY.clean = mutate(tonnage.NY,MON = substr(MONTH,1,4)) %>%
  group_by (.,MON =as.integer(MON),BOROUGH,COMMUNITYDISTRICT) %>%
  summarise(.,REFUSETONSCOLLECTED= sum(REFUSETONSCOLLECTED))

tonnage.borough.clean =  group_by (tonnage.NY.clean,MON,BOROUGH) %>%
  summarise(.,REFUSETONSCOLLECTED =sum(REFUSETONSCOLLECTED)) %>%
  inner_join(.,population.NY.clean, by = c("MON" = "Year","BOROUGH"="BOROUGH")) %>%
  filter(., Program.Type == 'Intercensal Population Estimate') %>%
  select (., MON,BOROUGH,Population,REFUSETONSCOLLECTED)

all.boros = unique(tonnage.borough.clean$BOROUGH)
all.years = tonnage.borough.clean$MON

tonnage.borough.clean.by.year = group_by(tonnage.borough.clean,MON) %>%
  summarise(.,Population=sum(Population),REFUSETONSCOLLECTED=sum(REFUSETONSCOLLECTED))






