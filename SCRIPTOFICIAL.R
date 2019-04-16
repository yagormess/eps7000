##Definir os pacotes##
library(descriptr)
library(dplyr)
library(gsheet)
library(markdown)
library(ggplot2)
library(ggExtra)
##

#######Carregar a tabela na variável "a" e limpar os dados########
a<-gsheet2tbl("https://docs.google.com/open?id=1qegBjG-GkGQHwhnkzOmLNbI_pddNMs7el-bwlPlDEnA")
c<-filter(a, Item_Weight != "null" & Item_Identifier != "null" & Outlet_Size != "" & 
            Item_Fat_Content != "")
c$Item_Fat_Content[c$Item_Fat_Content=="LF"]<-"Low Fat"
c$Item_Fat_Content[c$Item_Fat_Content=="low fat"]<-"Low Fat"
c$Item_Fat_Content[c$Item_Fat_Content=="reg"]<-"Regular"
#########

TabelaItem<-c %>% group_by(Item_Identifier) %>%
  summarise(
    TotalVendas=sum(Item_Outlet_Sales),
    PreçoMedio=mean(Item_MRP),
    Peso=mean(Item_Weight)
    )

TabelaOutlet<-c %>% group_by(Outlet_Identifier) %>% summarise(
  TotalVendas=sum(Item_Outlet_Sales)
    )

Tabela1<-c %>% group_by(Item_Identifier,Item_Fat_Content) %>% summarise(TotalVendas=sum(Item_Outlet_Sales),
                                                 PreçoMedio=mean(Item_MRP),
                                                 Peso=mean(Item_Weight))
Tabela1$FaixadePreço<-cut(Tabela1$PreçoMedio,seq(30,270,30))

Tabela2<-c %>% group_by(Item_Identifier,Outlet_Identifier) %>% summarise(Vendas=sum(Item_Outlet_Sales),
                                                                PreçoMedio=mean(Item_MRP))
Tabela2$FaixadePreço<-cut(Tabela2$PreçoMedio,seq(30,270,30))

Tabela3<-c%>%group_by(Outlet_Identifier,Item_Identifier)%>%summarise(sum(Item_Visibility))

ggplot(Tabela1,aes(Peso,Item_Fat_Content))+geom_col()+coord_flip()

Grafico6<-ggplot(Tabela2,aes(Outlet_Identifier,Vendas))+geom_col()+aes(fill=FaixadePreço)+labs(title="Vendas Totais por Loja",y="Total de Vendas (R$)",x="ID da Loja")+ylim(0,2300000)
Grafico5<-ggplot(TabelaItem,aes(PreçoMedio,TotalVendas))+geom_jitter(aes(color=Peso))+geom_smooth(color="red",size=1.5)
Grafico5<-ggMarginal(Grafico5,type=c("histogram"),alpha="0.1")
Grafico4<-ggplot(c,aes(Outlet_Identifier,Item_Outlet_Sales))+geom_boxplot(fill="Tan",outlier.shape=NA)
Grafico3<-ggplot(c, aes(Item_Fat_Content,Item_Type))+geom_count(alpha="0.1")
Grafico2<-ggplot(Tabela1,aes(x="",fill=Item_Fat_Content))+geom_bar(colour="black")+labs(title="Proporção de Item LowFat/Regular",fill="",x=NULL,y=NULL)+theme(axis.line=element_blank())
Grafico2<-Grafico2+coord_polar(theta = "y")

Grafico2
Grafico3
Grafico4
Grafico5
Grafico6

