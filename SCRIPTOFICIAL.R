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
c$Outlet_Size<-factor(c$Outlet_Size)
c$Outlet_Location_Type<-factor(c$Outlet_Location_Type)
c$Outlet_Type<-factor(c$Outlet_Type)
c$Item_Fat_Content<-factor(c$Item_Fat_Content)
c$Item_Type<-factor(c$Item_Type)
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

Tabela4<-c%>%group_by(Outlet_Size)%>%summarise(SomadasVendas=sum(Item_Outlet_Sales),mediana=median(Outlet_Establishment_Year))

Tabela3<-c%>%group_by(Item_Type)%>%summarise(SomadasVendas=sum(Item_Outlet_Sales))

Grafico7<-ggplot(Tabela3,aes(SomadasVendas,Item_Type))+geom_point()+labs(title="Tipo de Item x Vendas",x="Valor total de vendas(R$)",y="Tipo do Item")
Grafico6<-ggplot(Tabela2,aes(Outlet_Identifier,Vendas))+geom_col()+aes(fill=FaixadePreço)+labs(title="Vendas Totais por Loja",y="Total de Vendas (R$)",x="ID da Loja")+ylim(0,2300000)
Grafico5<-ggplot(TabelaItem,aes(PreçoMedio,TotalVendas))+geom_jitter(aes(color=Peso))
Grafico5<-ggMarginal(Grafico5,type=c("histogram"),alpha="0.1")
Grafico4<-ggplot(c,aes(Outlet_Identifier,Item_Outlet_Sales))+geom_boxplot(fill="Tan",outlier.shape=NA)+labs(x="Codigo da Loja", y="Vendas")
Grafico3<-ggplot(c, aes(Item_Fat_Content,Item_Type))+geom_count(alpha="0.1")+labs(x=NULL,y="Tipo de Produto")
Grafico2<-ggplot(Tabela1,aes(x="",fill=Item_Fat_Content))+geom_bar(colour="black")+labs(title="Proporção de Item LowFat/Regular",fill="",x=NULL,y=NULL)+theme(axis.line=element_blank())
Grafico2<-Grafico2+coord_polar(theta = "y")+removeGrid(x=TRUE,y=TRUE)+theme_void()
Grafico1<-ggplot(Tabela4,aes(Outlet_Size,SomadasVendas))+geom_col(aes(fill=mediana))+labs(x="Tamanho da Loja",y="Total de Vendas")+theme_light()+labs(title="Partipação nas Vendas Totais x Tamanho da Loja")

Grafico1
Grafico2
Grafico3
Grafico4
Grafico5
Grafico6
Grafico7
