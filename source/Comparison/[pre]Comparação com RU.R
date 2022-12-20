# Comparação com reator único - concentração ------------------------------

ru<- read.csv2("../output/resultado_unsteady10.csv") %>% filter(res=="Jurumirim")
ru$Data <- as.Date(ru$Data,format="%d/%m/%Y" )
ru<- ru %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
ru2<- ru[,c(2,23,24)]%>% mutate(Q_Deflu=ru$Q_Deflu,Font="Model A")



#View(ru2)

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor=="Setor 1"& ano=="T25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt4<- filter(df_melt,Setor=="Setor 1"& ano=="T35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt5<- filter(df_melt,Setor=="Setor 1"& ano=="A25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt6<- filter(df_melt,Setor=="Setor 1"& ano=="A35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt7<- filter(df_melt,Setor!="Setor 1")
df_melt<- rbind(df_melt2,df_melt3,df_melt4,df_melt5,df_melt6,df_melt7)
df_melt<- df_melt %>% filter(Setor=="Setor 1")%>% mutate(C_out=Valor)%>% select(Data, C_out, ano,Q_Deflu)%>% mutate(Font="Model B")


comp<-rbind(ru2,df_melt)

comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))


x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loads.csv")


# Comparação com reator único e delft - concentração --------------------------------------------------
delft<- read.csv2("../dataset/setores/Setor1_B12_Delft.csv") 
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
delft<- delft %>% mutate(ano="B12", Q_Deflu=jur$Q_Deflu[c(366:731)], Font="Delft3D")

comp<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(ano=="B12")
comp<-rbind(comp,delft)
View(comp)
comp$Font <- factor(comp$Font, levels = c("Model A","Model B", "Delft3D"))

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_serie.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")
x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=Font), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.11,0.01),limits=c(0,0.11),expand = c(0,0),labels = seq(0.0,0.11,0.01))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.05), color="black",linetype="dashed")

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft2012 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# Comparação com reator único e delft - cargas ------------------------------------

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous()+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=C_out*Q_Deflu/1000*86400, color=Font), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_capitulos_carga - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacaoB12.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12.csv")


# A35 Comparação com reator único e delft - concentração --------------------------------------------------
ru<- read.csv2("../output/resultado_unsteady10.csv") %>% filter(res=="Jurumirim")
ru$Data <- as.Date(ru$Data,format="%d/%m/%Y" )
ru<- ru %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
ru2<- ru[,c(2,23,24)]%>% mutate(Q_Deflu=ru$Q_Deflu,Font="Model A")



#View(ru2)

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor=="Setor 1"& ano=="T25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt4<- filter(df_melt,Setor=="Setor 1"& ano=="T35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt5<- filter(df_melt,Setor=="Setor 1"& ano=="A25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt6<- filter(df_melt,Setor=="Setor 1"& ano=="A35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt7<- filter(df_melt,Setor!="Setor 1")
df_melt<- rbind(df_melt2,df_melt3,df_melt4,df_melt5,df_melt6,df_melt7)
df_melt<- df_melt %>% filter(Setor=="Setor 1")%>% mutate(C_out=Valor)%>% select(Data, C_out, ano,Q_Deflu)%>% mutate(Font="Model B")


comp<-rbind(ru2,df_melt)

comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))



delft<- read.csv2("../dataset/setores/Setor1_A35_Delft.csv") 
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
delft<- delft %>% mutate(ano="A35", Q_Deflu=jur$Q_Deflu[c(366:731)], Font="Delft3D")

comp<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(ano=="A35")
comp<-rbind(comp,delft)
View(comp)
comp$Font <- factor(comp$Font, levels = c("Model A","Model B", "Delft3D"))

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="A35")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=Font), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.4,0.1),limits=c(0,0.4),expand = c(0,0),labels = seq(0.0,0.4,0.1))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.05), color="black",linetype="dashed")

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft2035 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# A35 Comparação com reator único e delft - cargas ------------------------------------

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(ano=="A35")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous()+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=C_out*Q_Deflu/1000*86400, color=Font), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_capitulos_carga2035 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacaoA35.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsA35.csv")


