#######Definir as bibliotecas a serem utilizadas#######
library(descriptr)
library(dplyr)
library(gsheet)
library(markdown)
#########

#######Carregar a tabela na variável "a" e limpar os dados########
a<-gsheet2tbl("https://docs.google.com/open?id=1qegBjG-GkGQHwhnkzOmLNbI_pddNMs7el-bwlPlDEnA")
c<-filter(a, Item_Weight != "null" & Item_Identifier != "null" & Outlet_Size != "")
rm(a)
#########


summary(c$Item_Fat_Content)
c$Item_Fat_Content[c$Item_Fat_Content=="LF"]<-"Low Fat"
c$Item_Fat_Content[c$Item_Fat_Content=="low fat"]<-"Low Fat"
d<- c %>% count(Item_Type, Outlet_Location_Type)
c$Item_WeightCat<-cut(c$Item_Weight,seq(0,50,5))
c$Item_Outlet_Sales2<-cut(c$Item_Outlet_Sales,seq(0,11000,1000))
e<-ds_twoway_table(c, Item_WeightCat, Item_Outlet_Sales2)
ggplot(e, aes(Item_WeightCat,percent))+
  geom_col()