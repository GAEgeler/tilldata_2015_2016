# edit daily data: menu lines----

# status: 3.8.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "here")
lapply(pack, function(x){do.call("library", list(x))})

# load data 2017
source("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/041_load_data_180802_egel.R")
rm(list = c("buffet", "df_17", "df_7_", "envir_nutri",  "info", "info_", "info_compl", "info_orig"))

# prepare data from 2017, with locals and selling time from 11 a.m. until 2 p.m.
# assumption that locals from 2015 and 2016 are included in the till data (maybe ask michael krauer for that)
menu_tot7 <- filter(df_7, (hour(trans_date) >= 11 & hour(trans_date) <= 13) & total_amount_trans < 1000)

menu_tot7 <- menu_tot7 %>%
    group_by(article_description, date, week, year, cycle, shop_description, condit) %>% 
    summarise(tot_sold=n()) %>% # group it again, for merge with other data, no condit variable
    ungroup() %>%
    select(date, article_description, year, week, cycle, shop_description, condit, tot_sold)

# load data 2015 and 2016
dat_hs_tot <- read_delim(here("augmented data/", "verkaufsdaten täglich HS 15-16 180731.csv"), 
                         delim = ';',
                         col_types = cols(date = col_date(format="%d.%m.%Y"))) %>% # load data
    dplyr::rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    filter(week >= 40 & week <= 51) %>% # select only between week 40 and 51
    mutate(date = parse_date(date)) %>%
    select(date, article_description, year, week, shop_description, tot_sold)

dat_hs_tot$week <- as.numeric(dat_hs_tot$week) # change character to numeric
dat_hs_tot$cycle <- ifelse(dat_hs_tot$week >=40 & dat_hs_tot$week <= 45, 1, 2) # add canteen cycle
dat_hs_tot$condit <- ifelse(dat_hs_tot$week %%2 == 0 & dat_hs_tot$cycle == 1,"Basis",ifelse(dat_hs_tot$week %%2 == 1 & dat_hs_tot$cycle == 2,"Basis","Intervention")) 

# change article_description variables
dat_hs_tot$article_description <- str_replace(dat_hs_tot$article_description,"BuffetTotal","Hot and Cold")
dat_hs_tot$article_description <-  str_replace(dat_hs_tot$article_description,"KitchenTotal", "Kitchen")
dat_hs_tot$article_description <-  str_replace(dat_hs_tot$article_description,"FavoriteTotal", "Favorite")

# change shop_description variables
dat_hs_tot$shop_description <- str_replace(dat_hs_tot$shop_description,"grüental","Grüental Mensa")
dat_hs_tot$shop_description <- str_replace(dat_hs_tot$shop_description,"vista","Vista Mensa")

# merge with data 2017
menu_tot <- bind_rows(dat_hs_tot, menu_tot7)

# save data
write_delim(menu_tot, here("augmented data", "data daily line 15_17 180803 egel.csv"), delim = ",")
