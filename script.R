#######Definir as bibliotecas a serem utilizadas#######
library(descriptr)
library(dplyr)
library(gsheet)
library(markdown)
library(ggplot2)
#########

#######Carregar a tabela na variável "a" e limpar os dados########
a<-gsheet2tbl("https://docs.google.com/open?id=1qegBjG-GkGQHwhnkzOmLNbI_pddNMs7el-bwlPlDEnA")
c<-filter(a, Item_Weight != "null" & Item_Identifier != "null" & Outlet_Size != "")
rm(d)
#########


summary(c$Item_Fat_Content)

c$FaixadePeso<-cut(c$Item_Weight,seq(0,25,5))
c$Item_Outlet_Sales2<-cut(c$Item_Outlet_Sales,seq(0,11000,1000))
e<-ds_twoway_table(c, Item_WeightCat, Item_Outlet_Sales2)
correla<-c(rcorr(cbind(c$Item_Weight, c$Item_MRP, c$Item_Outlet_Sales)))
ggplot(c, aes(x=d$Item_MRP, y=d$Item_Outlet_Sales))+
  geom_point()

teste<-ggplot(d,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point()+
  labs(x="Preço de Fabrica",y="Quantidade de itens Vendidos")+geom_jitter(aes(colour=c$Item_Fat_Content))
