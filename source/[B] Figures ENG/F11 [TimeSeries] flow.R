

df_vazoes_B <- df_wide_B %>% 
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t,-C_in,-C_out)%>%
    gather(key = "Variable", value = "Valor", -Data, -res, -Setor, -Cenario, Q_Aflu, Q_Deflu) 

# Vazões (inglês) --------------------------------------------------------

variable_names <- c(
  "Setor 1" = "Sector 1" ,
  "Setor 2" = "Sector 2" ,
  "Setor 3" = "Sector 3" ,
  "Setor 4" = "Sector 4" 
)


plot <- df_vazoes_B%>%
  filter(Data>= .start & Data <= .end)%>%
  filter(Cenario=="B12")%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable, linetype= Variable), size=0.3)+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  scale_color_manual(name= "", labels = c("Inflow", "Outflow"),values=c("#FF7664","#000000"))+
  scale_linetype_manual(name= "", labels = c("Inflow", "Outflow"), values=c("solid", "twodash"))+
  labs(title = " ", x="", y = bquote("Flow" ~~ "("~"m³.s" ^ "-1"~")"))+
  facet_grid(.~Setor,labeller = labeller(Setor = variable_names))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

plot
ggsave(filename = "img/[B] Figures ENG/[TimeSeries] flow.png", plot= plot, device = "png", width = 16, height = 10, units = "cm")
