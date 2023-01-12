

# Comparação (ingles) ----------------------------------------------------

plot <- df_results_B %>%
  filter(Data>= .start & Data <= .end)%>%
  filter(Cenario=="B12")%>%
  filter(Variable=="C_out")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
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
  geom_line(aes(x= Data, y=Valor, color=Setor, linetype=Setor), size=0.5)

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] Sectors Comparison - Concentration.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")



df_results_B %>%
  filter(
    Data>= .start & 
    Data <= .end &
    Cenario=="B12" & 
    Setor=="Setor 3" &
    Variable=="C_out" &
    Valor >= 0.3
  )%>%
  nrow()/366


df_results_B %>%
  filter(
    Data>= .start & 
    Data <= .end &
    Cenario=="B12"  &
    Variable=="C_out" &
    Setor=="Setor 2"
  )%>%
  select(Valor)%>%
  max()

1-df_results_B %>%
  filter(
    Data>= .start & 
    Data <= .end &
    Cenario=="B12"  &
    Variable=="C_out" &
    Valor >= 0.1 & 
    Setor=="Setor 4"
  )%>%
  nrow()/366

df_results_B %>%
  filter(
    Data>= .start & 
    Data <= .end &
    Cenario=="B12"  &
    Variable=="C_out" &
    Setor=="Setor 1"
  )%>%
  select(Valor)%>%
  max()
