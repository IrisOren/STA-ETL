#The script prepares a dataset with weight and carbon "type" category appended
#wght is a file of weights for subset of items in the inventory that have been categorised
#the names of items in inventory are an approximate match to names in wght
#match_str to get approximate matches and append to inventory
library(tidyverse)
library(stringr)
library(readxl)


get_match <- function(name, match_vec){
  # name: str
  # match_vec: vector of strings
  # match name to item in match vec
  # return match or NaN if none
  
  ret = NaN
  for(i in 1:length(match_vec)){
    mtch <- str_match(name %>% tolower(), match_vec[i] %>% tolower())
    if(is.na(mtch[1]) == FALSE){
      ret = mtch[1]
      break
    }
  }
  ret
}

# import inventory
inv <- read.csv("../raw_data/inventory-with categories.csv")
#import data of name, weight and type classification (in terms of type 1 and type 2)
wght <- readxl::read_excel("../raw_data/CO2 data - weight, type, emission factor.xlsx", 
                           range = "A2:D36") %>%
  rename(name = `...1` , 
         weight = `...2`) %>%
  mutate(name = tolower(name))
  


#create the appended inventory file
inv <- inv %>%
  mutate(name = Name %>% 
           map(function(x) get_match(x, wght$name)) %>%
           flatten_chr()) %>%
  left_join(wght)

type1 <- read_excel("./raw_data/CO2 data - weight, type, emission factor.xlsx", 
                   range = "H2:I5", 
                   col_names = T)

type2 <- read_excel("./raw_data/CO2 data - weight, type, emission factor.xlsx", 
                    range = "H7:I20", 
                    col_names = T)

write.csv(inv, "./raw_data/inv_with_type.csv")
write.csv(type1, "./raw_data/type1.csv")
write.csv(type2, "./raw_data/type2.csv")

#Shiny app: import the type and emissions data
#           drop unclassified type items