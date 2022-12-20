# Volumes (inglês) --------------------------------------------------------
df_volumes<- 
  df_wide  %>%   
  mutate(As=As/2)%>%
  select(-P,-Cota,-k,-set_v,-i,-t,-C_in,-C_out, -Q_Aflu, -Q_Deflu)%>%
  gather(key = "Variable", value = "Valor", -Data, -res, -Setor, -Cenario, Vol, As) 


variable_names <- list(
  "Vol" = "Volume (hm³)" ,
  "As" = "Area (km²)"
)

Sys.setlocale("LC_TIME", "English")
plot <- 
  df_volumes %>%
  filter(
    Data>= as.Date("2012-01-01") & 
    Data <= as.Date("2012-12-31"))%>%
  filter(Cenario=="B12")%>%
  ggplot()+
  scale_x_date(
    date_labels = "%b", 
    date_breaks ="2 months",
    expand = c(0,0))+
  scale_y_continuous(
    expand = c(0,0)
  )+
  theme_bw()+
  facet_grid(
    Variable~., 
    scales="free",
    labeller=variable_labeller
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
  theme(
    axis.text.x = 
    element_text(angle = 45, hjust = 1), 
    legend.position="top"
  )+
  geom_line(
    aes(
      x= Data, 
      y=Valor/1000000, 
      color=Setor
    ), 
    linewidth=0.8
  )

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] volumes and areas.png", plot= plot, device = "png", width = 15, height = 18, units = "cm")

