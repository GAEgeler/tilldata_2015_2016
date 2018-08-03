# edit daily data----

# status: 2.8.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "here")
lapply(pack, function(x){do.call("library", list(x))})

# data from 2015 till 2017-----
df_tot <- read_delim(here("augmented data/verkaufsdaten täglich HS 15-16 180731.csv"), 
                         delim = ';',
                         col_types = cols(date = col_date(format="%d.%m.%Y"))) %>% # load data
    rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week= isoweek(date)) %>% # might get an error
    select(article_description, date, week, year, shop_description, tot_sold)

# change article_description variables-----
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change shop_description variables-----
df_tot$shop_description <- str_replace(df_tot$shop_description,"grüental","Grüental Mensa")
df_tot$shop_description <- str_replace(df_tot$shop_description,"vista","Vista Mensa")


### 2015: calculation of the relative meal content-----
# filter for 2015
dt <- filter(df_tot, year == 2015) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename
Fleisch_g$label_content <- "Fleisch" # add label content
Fleisch_g$shop_description <- "Grüental Mensa" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarisch"
Vegetarisch_g$shop_description <- "Grüental Mensa"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental Mensa"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)

names(Fleisch_v) <- c("tot_sold","date") 
Fleisch_v$label_content <- "Fleisch"
Fleisch_v$shop_description <- "Vista Mensa"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarisch"
Vegetarisch_v$shop_description <- "Vista Mensa"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista Mensa"

menu_tot5 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot5$year <- year(menu_tot5$date) # add variable year
menu_tot5$week <- isoweek(menu_tot5$date) # add week nr

### 2016: calculation of the relative meal content----
# filter for 2016
dt <- filter(df_tot, year == 2016) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename 
Fleisch_g$label_content <- "Fleisch" # add label_content
Fleisch_g$shop_description <- "Grüental Mensa" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarisch"
Vegetarisch_g$shop_description <- "Grüental Mensa"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental Mensa"

# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)

names(Fleisch_v) <- c("tot_sold","date")
Fleisch_v$label_content <- "Fleisch"
Fleisch_v$shop_description <- "Vista Mensa"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarisch"
Vegetarisch_v$shop_description <- "Vista Mensa"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista Mensa"

# concate all dataframes
menu_tot6 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot6$year <- year(menu_tot6$date) # add variable year
menu_tot6$week <- isoweek(menu_tot6$date) # add week nr


# Concatinate data frames 2015 until 2017---
menu_tot5 <- as_tibble(filter(menu_tot5, week >=40))  # filter weeks
menu_tot5$semwk <- ifelse(menu_tot5$week == 52, 15, NA) # year 2015 has more weeks than the year 2016,17, thus to calculate semweek we need to split up the data frame
menu_5 <- filter(menu_tot5, week < 52) # subset data all exept week 52 (only 3 occurencies per label_content => thus impossible to use rep() function)
menu_5$semwk <- rep(3:14,each=30) # add semester week
menu_tot5$semwk <- ifelse(is.na(menu_tot5$semwk),menu_5$semwk,menu_tot5$semwk) # update both data frames
menu_tot5 <- filter(menu_tot5, week < 52)

menu_tot6 <- as_tibble(filter(menu_tot6, week >=40)) # filter weeks
menu_tot6$semwk <- rep(3:14, each=30) # add semester week

# load data 2017
# https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
# https://stackoverflow.com/questions/42815889/r-source-and-path-to-source-files

# source_lines <- function(file, lines){
#     source(textConnection(readLines(file)[lines]))
# }

source("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/041_load_data_180802_egel.R")
rm(list = c("buffet", "df_17", "df_7_", "envir_nutri",  "info", "info_", "info_compl", "info_orig"))


# prepare data from 2017, with locals and selling time from 11 a.m. until 2 p.m.
# assumption that locals from 2015 and 2016 are included in the till data (maybe ask michael krauer for that)
menu_tot7 <- filter(df_7, (hour(trans_date) >= 11 & hour(trans_date) <= 13) & total_amount_trans < 1000)

menu_tot7 <- menu_tot7 %>%
    group_by(date, label_content, shop_description, year, week, semwk) %>%
    summarise(tot_sold = n())

#problems with bind_rows because date differs between menu_tot5,6 and 7
menu_tot <- bind_rows(menu_tot7,menu_tot6,menu_tot5) # coerc factors into characters

# remove some data from working space
rm(list=c("Hot_Cold_v","Hot_Cold_g","Fleisch_v","Fleisch_g","Vegetarisch_v","Vegetarisch_g","menu_5", "df_7","dt", "df_tot", "menu_tot5", "menu_tot6", "menu_tot7"))

# save dataset
write_delim(menu_tot, here("augmented data/", "data daily 15_17 180803 egel.csv"), delim = ';') # for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
