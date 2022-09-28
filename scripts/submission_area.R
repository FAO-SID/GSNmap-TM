rm(list=ls())
#read in tables
table <- read.csv("metadata/Annex_National_Submissions.csv", header = T, sep = ";")
FAO <- read.csv("metadata/factbook.csv")

library(googlesheets4)
library(data.table)
library(tidyverse)
#Data to be imported from google sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1mi9DNWcmFUi2m_aql1NGbz9yn8uVjQUtwgcdUJboY0U/edit#gid=2054192599"
# "https://docs.google.com/spreadsheets/d/1bKnZ5ZSEmCCNWcgUb6Be-ZDGUO6-nHBIbttIEpO-Nd8/edit#gid=213765290"
##sheet of interest for the map
gs4_auth()
sheet <- "Metadata_v1.6.1"
data <- read_sheet(sheet_url, sheet =sheet)
data <- as.data.frame(data)
data <- select(data, c("id","ISO","MapSource_v1.6"))

#merge relevant columns
x <- merge(table, FAO, by.x = "Country", by.y = "name", all.x = T)
x <- select(x, c("Country","ISO","area"))
x <- merge(x,data, by = "ISO", all.x = T)

# add missing areas in km2 (complementing the factbook.csv)
c <- x[is.na(x$area),]
c$area <- c(1099000,4033,78871,1393,702,1648000,236800,25713,6020,17,17364,185180,945087,916445)
x <- merge(x, c, all = T)
x <- x[!is.na(x$area),]

#aggregate values and calculate global share in %
y <- aggregate(area ~ MapSource_v1.6, x, FUN=sum)
y$percentage <- y$area/sum(y$area)*100

# get information on the map source distribution
summary(factor(x$MapSource_v1.6))
