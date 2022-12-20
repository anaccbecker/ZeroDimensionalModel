Sys.setlocale("LC_TIME", "English")

df_vazoes <- df_wide %>% 
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t,-C_in,-C_out)%>%
    gather(key = "Variable", value = "Valor", -Data, -res, -Setor, -Cenario, Q_Aflu, Q_Deflu) 

# Vazões (inglês) --------------------------------------------------------

variable_names <- c(
  "Setor 1" = "Sector 1" ,
  "Setor 2" = "Sector 2" ,
  "Setor 3" = "Sector 3" ,
  "Setor 4" = "Sector 4" 
)


plot <- df_vazoes%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Cenario=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(.~Setor,labeller = labeller(Setor = variable_names))+
  labs(title = " ", x="", y = "Flow (m³/s)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Inflow", "Outflow"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "img/[B] Figures ENG/[TimeSeries] flow.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")
