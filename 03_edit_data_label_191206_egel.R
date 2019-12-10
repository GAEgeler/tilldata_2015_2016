## edit till data 2015-2016: LABEL CONTENT-----

###
# state: december 2019
# author: gian-Andrea egeler
###


# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr", "here")
lapply(pack, function(x){do.call("library", list(x))})


# data from 2015 & 2016 calculate sellings PER label_content----
#same dataset as in earlier version
# attention here is causing problems, untill now no solution found => evtl. making a package
dat_hs_tot <- read_delim("augmented data/verkaufsdaten täglich HS 15-16 180731.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # load data
    dplyr::rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    select(article_description, date, week, year, shop_description, tot_sold)

df_tot <- droplevels(dat_hs_tot) # not really necessary

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change first letter of shop_description
source("10_function_first_letter_190114_egel.R")

df_tot$shop_description <- firstup(df_tot$shop_description)

### 2015: calculation of the relative meal content-----
# define meal content: version 1
dt <- filter(df_tot, year == 2015) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename
Fleisch_g$label_content <- "Meat" # add label content
Fleisch_g$shop_description <- "Grüental" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Fleisch_v) <- c("tot_sold","date") 
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista"

menu_tot5 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot5$year <- year(menu_tot5$date) # add variable year
menu_tot5$week <- isoweek(menu_tot5$date) # add week nr -- ATTENTION week() returns wrong week number

### 2016: calculation of the relative meal content----
# define meal content: version 1
dt <- filter(df_tot, year == 2016) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename 
Fleisch_g$label_content <- "Meat" # add label_content
Fleisch_g$shop_description <- "Grüental" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)


names(Fleisch_v) <- c("tot_sold","date")
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista"

menu_tot6 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot6$year <- year(menu_tot6$date) # add variable year
menu_tot6$week <- isoweek(menu_tot6$date) # add week nr -- ATTENTION week() returns wrong week number


# Concatinate data frames 2015 until 2017---
menu_tot5 <- as_tibble(filter(menu_tot5, week >=40))  # filter weeks
menu_tot5$semwk <- ifelse(menu_tot5$week == 52, 15, NA) # year 2015 has more weeks than the year 2016,17, thus to calculate semweek we need to split up the data frame
menu_5 <- filter(menu_tot5, week < 52) # subset data all exept week 52 (only 3 occurencies per label_content => thus impossible to use rep() function)
menu_5$semwk <- rep(3:14,each=30) # add semester week
menu_tot5$semwk <- ifelse(is.na(menu_tot5$semwk),menu_5$semwk,menu_tot5$semwk) # update both data frames

menu_tot6 <- as_tibble(filter(menu_tot6, week >=40)) # filter weeks
menu_tot6$semwk <- rep(3:14, each=30) # add semester week


# remove some data from working space
rm(list=c("Hot_Cold_v","Hot_Cold_g","Fleisch_v","Fleisch_g","Vegetarisch_v","Vegetarisch_g","menu_5","dat_hs_tot","df_tot","dt"))