###########################################
##Merging data Grüental HS 2015 und HS 2016
###########################################
## Status: 18.5.18 / egel
##########################################

##create list with excel files
library(plyr)
library(dplyr)
library(readr)

##funtion to read csv
read_filename_15 <- function(filename){
    ret<-read_delim(filename, delim = ',',locale = locale(encoding = 'LATIN1'), skip=10, escape_backslash = TRUE) # read csv
    names(ret)[2] <- 'name' # rename second column
    #ret <- filter(ret, !ret$Artikel == "Total") # another way to filter data
    ret <- ret[1:grep('Total*',ret$Artikel)[1],] # greps every row until the sting in Artikel "Total ", because there are two cell containing the string total, take the first one
    ret <- ret[,!grepl('X',names(ret))] # skip all rows containg an X as variable name 
    ret$source <- filename # new variable source takes the name of the file
    ret$year <- 2015 # new variable year
    return(ret) # return data frame
}

##read csv files and merge (September 2015)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2015/September")
files=list.files(pattern="csv")
import.Sept.2015 <- ldply(files, read_filename_15)
write_delim(import.Sept.2015,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Sept 2015 original.csv", delim=',')

##read csv files and merge (Oktober 2015)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2015/Oktober")
files=list.files(pattern="csv")
import.Okt.2015 <-ldply(files, read_filename_15)
write_delim(import.Okt.2015,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Okt 2015 original.csv", delim=',')

##read csv files and merge (November 2015)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2015/November")
files=list.files(pattern="csv")
import.Nov.2015 <- ldply(files, read_filename_15)
write_delim(import.Nov.2015,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Nov 2015 original.csv", delim=',')

##read csv files and merge (Dezember 2015)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2015/Dezember")
files=list.files(pattern="csv")
import.Dez.2015 <- ldply(files, read_filename_15)
write_delim(import.Dez.2015,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Dez 2015 original.csv", delim=',')

#############################################################################
#############################################################################

read_filename_16 <- function(filename){
    ret<-read_delim(filename, delim = ',',locale = locale(encoding = 'LATIN1'), skip=10, trim_ws = TRUE)
    #names(ret)[1] <- 'Artikel'
    names(ret)[2] <- 'name'
    ret <- (ret[1:grep('Total*',ret$Artikel)[1],]) 
    ret <- ret[,!grepl('X',names(ret))]
    ret$source <- filename #EDIT
    ret$year <- 2016
    return(ret)
}

##read csv files and merge (September 2016)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2016/September")
files=list.files(pattern="csv")
import.Sept.2016 <- ldply(files, read_filename_16)
write_delim(import.Sept.2016,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Sept 2016 original.csv", delim=',')

##read csv files and merge (Oktober 2016)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2016/Oktober")
files=list.files(pattern="csv")
import.Okt.2016 <- ldply(files, read_filename_16)
write_delim(import.Okt.2016,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Okt 2016 original.csv", delim=',')

##read csv files and merge (November 2016)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2016/November")
files=list.files(pattern="csv")
import.Nov.2016 <- ldply(files, read_filename_16)
write_delim(import.Nov.2016,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Nov 2016 original.csv", delim=',')

##read csv files and merge (Dezember 2016)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/original/grüental/tagesdaten grüental/2016/Dezember")
files=list.files(pattern="csv")
import.Dez.2016 <- ldply(files, read_filename_16)
write_delim(import.Dez.2016,path="S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/files/Verkaufsdaten Grüental Dez 2016 original.csv", delim=',')


