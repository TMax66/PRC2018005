library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(googlesheets)
library(ggplot2)
library(rpivotTable)
library(timevis)
library(janitor)

rm(list=ls())
Sys.sleep(5)


sheet <- gs_title("prc2018005")
Sys.sleep(5)
d1 <-gs_read(sheet, ws="dataset" )
Sys.sleep(2)
d2 <-gs_read(sheet, ws="massa" )
Sys.sleep(2)
d3 <-gs_read(sheet, ws="san" )
Sys.sleep(2)
d4 <-gs_read(sheet, ws="par" )
Sys.sleep(2)
d5 <-gs_read(sheet, ws="diagn" )
Sys.sleep(2)
d6 <-gs_read(sheet, ws="ben" )




# ds <-gs_read(dati, ws="dataset" )
#timing <- gs_read(sheet, ws="timing" )

#analisi<-gs_title("analisi")
#ana<-gs_read(analisi,ws="dati")

# fieldsAll<-c("mese","azienda", "ncapre","capog", "adultdead", "puppydead", "rim",
#           "abo","ter","asc","trim","tpuppy","scc","prot","cas","grasso","latt","cbt","urea", "inib",
#           "paratbc",	"agalassia",	"caev",	"mal.ascessi",	"mastite", "score")
#           


fieldsAll <- c("mese","azienda", "ncapre","capog", "adultdead", "puppydead", "rim",
               "abo","ter","asc","trim","tpuppy")

fieldsAll2<-c("mmese","mazienda", "scc","prot","cas","grasso","latt","cbt", "stau", "ureaFTIR","ureapHm", "inib")

fieldsAll3<-c("smese", "sazienda", "parat",	"agal",	"caev",	"ascessi",	"mast")

fieldsAll4<-c("pmese", "pazienda", "cat","coccidi","strGE","strPO")

fieldsAll5<-c("dmese","dazienda","dcat","necro","bat","diagnosi")

fieldsAll6<-c("bmese","bazienda", "bcompl","A","B","C","Biosic","GR")




# 
# fielsAll4<-c(	"controllo", "azienda", "score")





#####FUNZIONE LOAD DATA- CARICA IL FILE DOPO L'AGGIUNTA DI NUOVI RECORD x dati aziendali#######
loadData <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="dataset" )
  }

loadData2 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="massa")
  }

loadData3 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="san" )}

loadData4 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="par" )}
loadData5 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="diagn" )}

loadData6 <- function() {
  sheet <- gs_title("prc2018005")
  ds <-gs_read(sheet, ws="ben" )}






# 
# loadData4 <- function() {
#   sheet <- gs_title("prc2018005")
#   ds <-gs_read(sheet, ws="wellness" )}

shinyjs::useShinyjs()



#ds %>% 
#  group_by(azienda) %>% 
#  summarise_at(c("paratbc","agalassia","caev","mal.ascessi","mastite"), sum, na.rm = TRUE)

