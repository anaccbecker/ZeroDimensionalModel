# Volumes (inglês) --------------------------------------------------------
df_volumes<- 
  df_wide_B  %>%   
  mutate(As=As/2)%>%
  mutate(Vol=Vol*1000000)%>%
  select(-P,-Cota,-k,-set_v,-i,-t,-C_in,-C_out, -Q_Aflu, -Q_Deflu)%>%
  gather(key = "Variable", value = "Valor", -Data, -res, -Setor, -Cenario, Vol, As) 


variable_names <- list(
  "Vol" = "Volume (m³)" ,
  "As" = "Area (km²)"
)

plot <- 
  df_volumes %>%
  filter(
    Data>= .start & 
    Data <= .end)%>%
  filter(Cenario=="B12")%>%
  ggplot()+
  geom_line(
    aes(
      x= Data, 
      y=Valor/1000000, 
      color=Setor,
      linetype=Setor
    ), 
    size=0.5
  )+
  scale_x_date(
    date_labels = "%b", 
    date_breaks ="2 months",
    expand = c(0,0))+
  scale_y_continuous(
    expand = c(0,0),
    labels = function(x) format(x,  scientific = T,big.mark = " ")
  )+
  labs(
    title = " ", 
    x="", 
    y = ""
  )+
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
  theme_bw()+
  theme(
    axis.text.x = 
    element_text(angle = 45, hjust = 1), 
    legend.position="top"
  )+
  facet_grid(
    Variable~., 
    scales="free",
    labeller=variable_labeller
  )

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] volumes and areas.png", plot= plot, device = "png", width = 15, height = 18, units = "cm")

