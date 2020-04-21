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
options(scipen = 999)

gs_auth(token = "googlesheets_token.rds")
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))
sheet <- gs_title("prc2018005")
d1 <-gs_read(sheet, ws="dataset" )
d1$azienda<-casefold(d1$azienda, upper = TRUE)
d1$mese<-factor(d1$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio",  "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)


d2 <-gs_read(sheet, ws="massa" )
d2$azienda<-casefold(d2$azienda, upper = TRUE)
d2$mese<-factor(d2$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
d3 <-gs_read(sheet, ws="san" )
d3$azienda<-casefold(d3$azienda, upper = TRUE)
d3$mese<-factor(d3$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)

d4 <-gs_read(sheet, ws="par" )
d4$azienda<-casefold(d4$azienda, upper = TRUE)
d4$mese<-factor(d4$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
d5 <-gs_read(sheet, ws="diagn" )
d5$azienda<-casefold(d5$azienda, upper = TRUE)
d5$mese<-factor(d5$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)
d6 <-gs_read(sheet, ws="ben" )
d6$azienda<-casefold(d6$azienda, upper = TRUE)
d6$mese<-factor(d6$mese, levels=c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                                  "settembre", "ottobre", "novembre", "dicembre"), ordered=TRUE)



library(GGally)
ggpairs(d6[,4:9])

summary(d6$complben)
sd(d6$complben)^2

summary(d6$areaA)
sd(d6$areaA)^2

summary(d6$areaB)
sd(d6$areaB)^2

summary(d6$gr)
sd(d6$gr)^2


d4 %>% 
    filter(azienda=="039BG069") %>% 
    group_by(mese, cat) %>% 
    summarise(strongili=mean(`strongiliGE (upg)`, na.rm=T))%>% 
    ggplot(aes(x=mese, y=strongili, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)


d4 %>% 
  filter(azienda=="039BG069") %>% 
  group_by(mese, cat) %>% 
  summarise(coccidi=mean(`coccidi (upg)`, na.rm=T))%>% 
  ggplot(aes(x=mese, y=coccidi, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)




####SCC grafico####

d2 %>% 
  group_by(azienda,mese) %>% 
  summarise(scc=geometric.mean(scc, na.rm=T))%>% 
  ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)
  



d2 %>% 
  group_by(azienda,mese) %>% 
  summarise(cbt=geometric.mean(cbt, na.rm=T))%>% 
  ggplot(aes(x=mese, y=cbt, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)



d2 %>% 
  group_by(azienda,mese) %>% 
  summarise(proteine=mean(proteine, na.rm=T))%>% 
  ggplot(aes(x=mese, y=proteine, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)


d2 %>% 
  group_by(azienda,mese) %>% 
  summarise(grasso=mean(grasso, na.rm=T))%>% 
  ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)


d2 %>% 
  group_by(azienda,mese) %>% 
  summarise(grasso=mean(grasso, na.rm=T))%>% 
  ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()+facet_wrap(~azienda)










# dt<-d1 %>% 
#  full_join(d6,  by=c("mese","azienda")) %>% 
#   full_join(d2, by=c("mese","azienda")) %>% 
#    full_join(d3,by=c("mese","azienda")) 



# d2$controllo<-ifelse(d2$mese=="gennaio",1,
#                      ifelse(d2$mese=="febbraio", 2,
#                             ifelse(d2$mese=="marzo", 3,
#                                    ifelse(d2$mese=="aprile",4,
#                                           ifelse(d2$mese=="maggio",5,
#                                                  ifelse(d2$mese=="giugno",6,
#                                                         ifelse(d2$mese=="luglio",7,
#                                                            ifelse(d2$mese=="agosto",8,
#                                                                       ifelse(d2$mese=="settembre",9,
#                                                                              ifelse(d2$mese=="ottobre",10,
#                                                                                     ifelse(d2$mese=="novembre",11,
#                                                                                            ifelse(d2$mese=="dicembre",12,d2$mese))))))))))))
# d2$controllo<-as.numeric(d2$controllo)  

#####scc#
d2w<-d2 %>% 
  drop_na(azienda) %>% 
  arrange(mese) %>% 
  select(azienda,mese,scc) %>% 
  group_by(azienda,mese) %>% 
  summarise(scc=mean(scc)) %>% 
  # mutate(id=row_number()) %>% 
  # group_by(azienda,controllo) %>% 
  #arrange(controllo) %>% 
  pivot_wider(names_from=mese,values_from=scc ,values_fn = list(scc = mean))
 
  #select(-11)

##########################################




dwj<-d1 %>% 
  select(-anno) %>% 
  left_join(d2w,by=c("azienda")) %>% 
  left_join(d6[,-c(1:2)],by=c("azienda")) %>% 
  pivot_longer(13:22,
  names_to = "month", values_to = "scc") %>% 
  mutate(month=as.numeric(month)) %>% 
  group_by(azienda, month) %>% 
  summarise(scc=geometric.mean(scc, na.rm=T)) %>% 
  ggplot(aes(x=month, y=scc))+geom_point()+geom_smooth()+facet_wrap(~azienda)
  
###CBT###
d2w<-d2 %>% 
  select(azienda,cbt) %>% 
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
