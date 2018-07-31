#######################
## clean till data 2015 and 2016 
#######################
## status 31.7.18 / egel
#######################

#required libraries
library(dplyr)
library(readr)
library(stringr)
library(here)
library(purrr)

######################
# read data for grüental
dat_g_hs <-  
    list.files(here("clean data/"), pattern = "Grüental") %>% # list all csv with grüental in file name
    map_df(~ read_delim(here("clean data/", .x), delim = ",")) # apply read_delim to each element of the file-list (= files) and return tibble

## replace white space with underline in names of data frame
names(dat_g_hs)<- sub(" ","_",names(dat_g_hs))

##delete rows including total gruppe
dat_g <- dat_g_hs[-grep("Total", dat_g_hs$Artikel, ignore.case = T),]

##select and rename variables and save it into a new dataframe dat_g
dat_g <- dat_g %>%
    select(Artikel, name, Bruttobetrag, Nettobetrag, Verkaufte_Stücke, Gewicht_verkauft, Durchschnittlicher_Preis, `Tot_%Umsatz`,source,year) %>%
    dplyr::rename( article_description=name,umsatz_tot=`Tot_%Umsatz`, origin=source)
                 
##rename variable enties
dat_g$article_description <- str_replace(dat_g$article_description, "Küche","Catering")


##summarize the menus
## Local 1-4 Kitchen 0-4 are same Menu 
## important, Local 0 belongs to Favorite
# create variables for loop
possKitchenVariables <- c(paste0('Kitchen ',0:4),paste0('Local ',1:4)) # all possible kitchen variables
sumVariables <- c('Bruttobetrag','Nettobetrag','Verkaufte_Stücke','Gewicht_verkauft','umsatz_tot') # variables to take the sum
meanVariables <- c('Durchschnittlicher_Preis') # variable to take the mean
possBuffetVariables <- c('Garden', 'Market') # all possible hot and cold variables
possFavoriteVariables <- c("Favorite", "Local 0") # all possible favorite variables

## looping through the data frame
dat_g$date <- paste0(str_sub(dat_g$origin,-9,-5),'.',dat_g$year) # crate new date variable
for(i in unique(dat_g$date)) { # loop through date variable
    dat.date <-  dat_g[dat_g$date == i,] # create data frame with ith date entry
    dat.date.kitchen <-
        filter(dat.date, article_description %in% possKitchenVariables) # create data frame with all possible kitchen variables
    newRow <-
        list(
            Artikel = 'A1000N',article_description = 'KitchenTotal', date = i,origin =
                NA,
            year = as.integer(substr(i,7,10))
        ) # crate new list with five variables
    newRow <-
        c(newRow,unlist(colSums(dat.date.kitchen[,sumVariables]))) # add all sumVariables to the newRow resp to the list
    newRow <-
        c(newRow,Durchschnittlicher_Preis=mean(dat.date.kitchen[,meanVariables]$Durchschnittlicher_Preis)) # add all meanVariables to the newRow resp to the list
    dat_g = bind_rows(dat_g,newRow) # add new newRow to data frame dat_g
}

## looping through the data frame
## Garden and Market are same Menu
for(i in unique(dat_g$date)) {
    dat.date1 <- dat_g[dat_g$date == i,]
    dat.date.buffet <- dat.date1[dat.date1$article_description %in% possBuffetVariables,]
    newRow1 <-
        list(
            Artikel = 'A2000N',article_description = 'BuffetTotal',date = i,origin = NA,
            year = as.integer(substr(i,7,10))
        )
    newRow1 <-
        c(newRow1,unlist(colSums(dat.date.buffet[,sumVariables])))
    
    newRow1 <-
        c(newRow1,Durchschnittlicher_Preis = mean(dat.date.buffet[,meanVariables]$Durchschnittlicher_Preis))
    dat_g = bind_rows(dat_g,newRow1)
}


## looping through the data frame
## Favorite and Local 0 are same Menu

for(i in unique(dat_g$date)) {
    dat.date1 <- dat_g[dat_g$date == i,]
    dat.date.fav <- dat.date1[dat.date1$article_description %in% possFavoriteVariables,]
    newRow2 <-
        list(
            Artikel = 'A3000N',article_description = 'FavoriteTotal',date = i,origin = NA,
            year = as.integer(substr(i,7,10))
        )
    newRow2 <-
        c(newRow2,unlist(colSums(dat.date.fav[,sumVariables])))
    
    newRow2 <-
        c(newRow2,Durchschnittlicher_Preis = mean(dat.date.fav[,meanVariables]$Durchschnittlicher_Preis))
    dat_g = bind_rows(dat_g,newRow2)
}


# save data frame
write_delim(dat_g, path = here("augmented data/", "verkaufsdaten grüental HS 180731.csv"), delim = ";")


###################################################################################################################
######################
# read data for vista
dat_v_hs <-  
    list.files(here("clean data/"), pattern = "Vista") %>% # list all csv with grüental in file name
    map_df(~ read_delim(here("clean data/", .x), delim = ",")) # apply read_delim to each element of the file-list (= files) and bind the single dataframes

## replace white space with underline in names of data frame
names(dat_v_hs)<- sub(" ","_",names(dat_v_hs))

##delete rows including total gruppe and create new data frame
dat_v <- dat_v_hs[-grep("Total", dat_v_hs$Artikel, ignore.case = T),]

##select and rename variables
dat_v <- dat_v %>%
    select(Artikel, name, Bruttobetrag, Nettobetrag, Verkaufte_Stücke, Gewicht_verkauft, Durchschnittlicher_Preis, `Tot_%Umsatz`,source,year) %>%
    dplyr::rename( article_description=name,umsatz_tot=`Tot_%Umsatz`, origin=source)

##rename variable enties
dat_v$article_description <- str_replace(dat_v$article_description, "Küche","Catering")

##summarize the menus
possKitchenVariables <- c(paste0('Kitchen ',0:4),paste0('Local ',1:4)) # all possible kitchen variables
sumVariables <- c('Bruttobetrag','Nettobetrag','Verkaufte_Stücke','Gewicht_verkauft','umsatz_tot') # variables to take the sum
meanVariables <- c('Durchschnittlicher_Preis') # variable to take the mean
possBuffetVariables <- c('Garden', 'Market') # all possible hot and cold variables
possFavoriteVariables <- c("Favorite", "Local 0") # all possible favorite variables

# looping through the data frame
dat_v$date <- paste0(str_sub(dat_v$origin,-9,-5),'.',dat_v$year)
for(i in unique(dat_v$date)) {
    dat.date <- dat_v[dat_v$date == i,]
    dat.date.kitchen <-
        dat.date[dat.date$article_description %in% possKitchenVariables,]
    newRow <-
        list(
            Artikel = 'A1000N',article_description = 'KitchenTotal', date = i,origin =
                NA,
            year = as.integer(substr(i,7,10))
        )
    newRow <-
        c(newRow,unlist(colSums(dat.date.kitchen[,sumVariables])))
    newRow <-
        c(
            newRow,Durchschnittlicher_Preis = mean(dat.date.kitchen[,meanVariables]$Durchschnittlicher_Preis)
        )
    dat_v = bind_rows(dat_v,newRow)
}

# looping through the data frame
## Garden and Market are same Menu
for(i in unique(dat_v$date)) {
    dat.date1 <- dat_v[dat_v$date == i,]
    dat.date.buffet <- dat.date1[dat.date1$article_description %in% possBuffetVariables,]
    newRow1 <-
        list(
            Artikel = 'A2000N',article_description = 'BuffetTotal',date = i,origin = NA,
            year = as.integer(substr(i,7,10))
        )
    newRow1 <-
        c(newRow1,unlist(colSums(dat.date.buffet[,sumVariables])))
    
    newRow1 <-
        c(newRow1,Durchschnittlicher_Preis = mean(dat.date.buffet[,meanVariables]$Durchschnittlicher_Preis))
    dat_v = bind_rows(dat_v,newRow1)
}

# looping through the data frame
## Favorite and Local 0 are same Menu
for(i in unique(dat_v$date)) {
    dat.date1 <- dat_v[dat_v$date == i,]
    dat.date.fav <- dat.date1[dat.date1$article_description %in% possFavoriteVariables,]
    newRow2 <-
        list(
            Artikel = 'A3000N',article_description = 'FavoriteTotal',date = i,origin = NA,
            year = as.integer(substr(i,7,10))
        )
    newRow2 <-
        c(newRow2,unlist(colSums(dat.date.fav[,sumVariables])))
    
    newRow2 <-
        c(newRow2,Durchschnittlicher_Preis = mean(dat.date.fav[,meanVariables]$Durchschnittlicher_Preis))
    dat_v = bind_rows(dat_v,newRow2)
}

# save data frame
write_delim(dat_v, path = here("augmented data/", "verkaufsdaten vista HS 180731.csv"), delim = ';')

###combinde data frames
# create variable ort as shop description 
dat_g$ort <- 'grüental'
dat_v$ort <- 'vista'
dat_hs_tot <- bind_rows(dat_g,dat_v)

# save data frame
write_delim(dat_hs_tot, path = here("augmented data/", "verkaufsdaten täglich HS 15-16 180731.csv"), delim = ';')

# did a double check with an older version (*180518.csv) it seems that 120 observerions differ from the newer version (*180731.csv)
# however checkt couple of entries and they did not differ each from antother (dont know why the differences)
# only articles of no interest are affected
