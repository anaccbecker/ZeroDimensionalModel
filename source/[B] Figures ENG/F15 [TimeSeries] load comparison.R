# Comparação - cargas - (ingles) ----------------------------------------------------

df_wide2<- filter(df_wide,Setor=="Setor 1"& Cenario=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_wide3<- filter(df_wide,Setor!="Setor 1"| Cenario!="B12")
df_carga_B <- rbind(df_wide2,df_wide3)


plot <- df_carga_B %>%
  filter(Data>= .start & Data <= .end)%>%
  filter(Cenario=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"t.d" ^ "-1"~")"))+
    scale_color_manual(
    name= "", 
    labels = c(
      "Sector 1", 
      "Sector 2", 
      "Sector 3", 
      "Sector 4"),
    values=c(
      "#66ff99",
      "#ff33cc",
      "#ffcc00",
      "#0099ff"))+
  scale_linetype_manual(
    name= "", 
    labels = c(
      "Sector 1", 
      "Sector 2", 
      "Sector 3", 
      "Sector 4"),
    values= c(
      "solid",
      "dotted",
      "twodash",
      "dashed"
    ))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=C_out*Q_Deflu/1000*86400, color=Setor, linetype=Setor), size=0.5)

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] Sectors Comparison - Loads.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

