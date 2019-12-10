## edit till data 2015-2016: MEAL LINE-----

###
# state: december 2019
# author: gian-Andrea egeler
###

#load requied packages
# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr", "here")
lapply(pack, function(x){do.call("library", list(x))})


# data from 2015 & 2016 calculate sellings PER meal_line------
dat_hs_tot <- read_delim("augmented data/verkaufsdaten täglich HS 15-16 180731.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # load data
    rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    filter(week >= 40 & week <= 51) %>% # select only between week 40 and 51
    # mutate(date = parse_date(date)) %>%
    select(date, article_description, year, week, shop_description, tot_sold, Bruttobetrag)

dat_hs_tot$week <- as.numeric(dat_hs_tot$week) # change character to numeric
dat_hs_tot$cycle <- ifelse(dat_hs_tot$week >=40 & dat_hs_tot$week <= 45, 1, 2) 
dat_hs_tot$condit <- ifelse(dat_hs_tot$week %%2 == 0 & dat_hs_tot$cycle == 1, "Basis",ifelse(dat_hs_tot$week %%2 == 1 & dat_hs_tot$cycle == 2,"Basis","Intervention")) 

df_tot <- droplevels(dat_hs_tot)

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change first letter of shop_description
source("10_function_first_letter_190114_egel.R")

df_tot$shop_description <- firstup(df_tot$shop_description) # see function above

# remove unused data sets
rm(list = c("dat_hs_tot"))