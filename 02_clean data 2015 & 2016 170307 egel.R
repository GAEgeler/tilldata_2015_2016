#######################
## Simplified data Grüental HS
#######################
## status 17.5.18 / egel
#######################

#required libraries
library(dplyr)
library(readr)
library(stringr)

dat_g_s_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Sept 2015 original.csv", delim = ',', trim_ws = T)
dat_g_o_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Okt 2015 original.csv", delim = ',', trim_ws = T)
dat_g_n_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Nov 2015 original.csv", delim = ',', trim_ws = T)
dat_g_d_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Dez 2015 original.csv", delim = ',', trim_ws = T)

dat_g_s_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Sept 2016 original.csv", delim = ',', trim_ws = T)
dat_g_o_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Okt 2016 original.csv", delim = ',', trim_ws = T)
dat_g_n_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Nov 2016 original.csv", delim = ',', trim_ws = T)
dat_g_d_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Dez 2016 original.csv", delim = ',', trim_ws = T)


##merge to one data.frame
dat_g_hs <-  bind_rows(dat_g_s_15, dat_g_o_15, dat_g_n_15, dat_g_d_15,dat_g_s_16,dat_g_o_16 , dat_g_n_16, dat_g_d_16)

## replace white space with underline in names of data frame
names(dat_g_hs)<- sub(" ","_",names(dat_g_hs))

##delete rows including total gruppe
dat_g_hs <- dat_g_hs[-grep("Total", dat_g_hs$Artikel, ignore.case = T),]

##select and rename variables
dat_g <- dat_g_hs %>%
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
dat_g$date <- paste0(substr(dat_g$origin,1,5),'.',dat_g$year) # crate new date variable
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
write_delim(dat_g,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/verkaufsdaten grüental HS 180518.csv", delim = ";")


#######################
## Simplified data Vista HS
#######################
#######################
dat_v_s_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Sept 2015 original.csv",delim = ",")
dat_v_o_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Okt 2015 original.csv",delim = ",")
dat_v_n_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Nov 2015 original.csv",delim = ",")
dat_v_d_15=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Dez 2015 original.csv",delim = ",")

dat_v_s_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Sept 2016 original.csv",delim = ",")
dat_v_o_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Okt 2016 original.csv",delim = ",")
dat_v_n_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Nov 2016 original.csv",delim = ",")
dat_v_d_16=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Vista Dez 2016 original.csv",delim = ",")

##merge to one data.frame
dat_v_hs = bind_rows(dat_v_s_15, dat_v_o_15, dat_v_n_15, dat_v_d_15,dat_v_s_16, dat_v_o_16, dat_v_n_16, dat_v_d_16)

## replace white space with underline in names of data frame
names(dat_v_hs)<- sub(" ","_",names(dat_v_hs))

##delete rows including total gruppe
dat_v_hs <- dat_v_hs[-grep("Total", dat_v_hs$Artikel, ignore.case = T),]

##select and rename variables
dat_v <- dat_v_hs %>%
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
dat_v$date <- paste0(substr(dat_v$origin,1,5),'.',dat_v$year)
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
write_delim(dat_v,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/verkaufsdaten vista HS 180518.csv", delim = ',')


###combinde data frames
# create variable ort as shop description 
dat_g$ort <- 'grüental'
dat_v$ort <- 'vista'
dat_hs_tot <- bind_rows(dat_g,dat_v)

# save data frame
write_delim(dat_hs_tot,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/verkaufsdaten täglich HS 15-16 180518.csv", delim = ';')


