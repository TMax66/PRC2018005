library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(googlesheets)
library(ggplot2)
library(rpivotTable)
library(timevis)
library(janitor)
library(psych)


gs_auth(token = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))
sheet <- gs_title("prc2018005")
d1 <-gs_read(sheet, ws="dataset" )
d1$azienda<-casefold(d1$azienda, upper = TRUE)
d2 <-gs_read(sheet, ws="massa" )
d2$azienda<-casefold(d2$azienda, upper = TRUE)
d3 <-gs_read(sheet, ws="san" )
d3$azienda<-casefold(d3$azienda, upper = TRUE)
d4 <-gs_read(sheet, ws="par" )
d4$azienda<-casefold(d4$azienda, upper = TRUE)
d5 <-gs_read(sheet, ws="diagn" )
d5$azienda<-casefold(d5$azienda, upper = TRUE)
d6 <-gs_read(sheet, ws="ben" )
d6$azienda<-casefold(d6$azienda, upper = TRUE)




# dt<-d1 %>% 
#  full_join(d6,  by=c("mese","azienda")) %>% 
#   full_join(d2, by=c("mese","azienda")) %>% 
#    full_join(d3,by=c("mese","azienda")) 



d2$controllo<-ifelse(d2$mese=="gennaio",1,
                     ifelse(d2$mese=="febbraio", 2,
                            ifelse(d2$mese=="marzo", 3,
                                   ifelse(d2$mese=="aprile",4,
                                          ifelse(d2$mese=="maggio",5,
                                                 ifelse(d2$mese=="giugno",6,
                                                        ifelse(d2$mese=="luglio",7,
                                                           ifelse(d2$mese=="agosto",8,
                                                                      ifelse(d2$mese=="settembre",9,
                                                                             ifelse(d2$mese=="ottobre",10,
                                                                                    ifelse(d2$mese=="novembre",11,
                                                                                           ifelse(d2$mese=="dicembre",12,d2$mese))))))))))))
d2$controllo<-as.numeric(d2$controllo)  

#####scc#
d2w<-d2 %>% 
  arrange(controllo) %>% 
  select(azienda,controllo,scc) %>% 
  group_by(azienda,controllo) %>% 
  summarise(scc=mean(scc)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=controllo,values_from=scc ,values_fn = list(scc = mean)) %>% 
  select(-11)


dwj<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) %>% 
  pivot_longer(12:20,
    names_to = "mese", values_to = "scc") %>% 
  mutate(mese=as.numeric(mese)) %>% 
  group_by(azienda, mese) %>% 
  summarise(scc=geometric.mean(scc, na.rm=T)) %>% 
  ggplot(aes(x=mese, y=scc))+geom_point()+geom_smooth()+facet_wrap(~azienda)
  
###CBT###
d2w<-d2 %>% 
  select(azienda,controllo,cbt) %>% 
  group_by(azienda,controllo) %>% 
  summarise(cbt=mean(cbt)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=controllo,values_from=cbt ,values_fn = list(cbt = mean)) %>% 
  select(-11)


dwj<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) %>% 
  pivot_longer(12:20,
               names_to = "mese", values_to = "cbt") %>% 
  mutate(mese=as.numeric(mese)) %>% 
  group_by(azienda, mese) %>% 
  summarise(proteine=geometric.mean(cbt, na.rm=T)) %>% 
  ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)

###Proteine#####
d2w<-d2 %>% 
select(azienda,controllo,proteine) %>% 
  group_by(azienda,controllo) %>% 
  summarise(proteine=mean(proteine)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=controllo,values_from=proteine ,values_fn = list(proteine = mean)) %>% 
  select(-11)


dwj<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) %>% 
  pivot_longer(12:20,
               names_to = "mese", values_to = "proteine") %>% 
  mutate(mese=as.numeric(mese)) %>% 
  group_by(azienda, mese) %>% 
  summarise(proteine=mean(proteine, na.rm=T)) %>% 
  ggplot(aes(x=mese, y=proteine))+geom_point()+geom_smooth()+facet_wrap(~azienda)

####Grasso####
d2w<-d2 %>% 
  select(azienda,controllo,grasso) %>% 
  group_by(azienda,controllo) %>% 
  summarise(grasso=mean(grasso)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=controllo,values_from=grasso ,values_fn = list(grasso = mean)) %>% 
  select(-11)


dwj<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) %>% 
  pivot_longer(12:20,
               names_to = "mese", values_to = "grasso") %>% 
  mutate(mese=as.numeric(mese)) %>% 
  group_by(azienda, mese) %>% 
  summarise(grasso=mean(grasso, na.rm=T)) %>% 
  ggplot(aes(x=mese, y=grasso))+geom_point()+geom_smooth()+facet_wrap(~azienda)

###Lattosio####
d2w<-d2 %>% 
  select(azienda,controllo,lattosio) %>% 
  group_by(azienda,controllo) %>% 
  summarise(lattosio=mean(lattosio)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=controllo,values_from=lattosio ,values_fn = list(lattosio = mean)) %>% 
  select(-11)


dwj<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) %>% 
  pivot_longer(12:20,
               names_to = "mese", values_to = "lattosio") %>% 
  mutate(mese=as.numeric(mese)) %>% 
  group_by(azienda, mese) %>% 
  summarise(lattosio=mean(lattosio, na.rm=T)) %>% 
  ggplot(aes(x=mese, y=lattosio))+geom_point()+geom_smooth()+facet_wrap(~azienda)
######################################################################################
######################################################################################
######################################################################################


x<-d1 %>% 
  select(-mese) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-1],by=c("azienda")) 
