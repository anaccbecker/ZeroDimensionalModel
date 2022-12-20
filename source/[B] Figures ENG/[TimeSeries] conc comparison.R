Sys.setlocale("LC_TIME", "English")

# Comparação (ingles) ----------------------------------------------------

plot <- df_results %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Cenario=="B12")%>%
  filter(Variable=="C_out")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name= "", labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Setor), size=0.8)

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] Sectors Comparison - Concentration.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

