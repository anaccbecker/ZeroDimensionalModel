

df_vazoes <- df_wide_A %>% 
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t,-C_in,-C_out, -Q_Vertida,-Q_Turb,-NAs_calc)%>%
    gather(key = "Variable", value = "Valor", -Data, -res, -Cenario, Q_Aflu, Q_Deflu) 



plot <- df_vazoes %>%
  filter(Data>= .start & Data <= .end) %>%
  filter(Cenario=="B12") %>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable, linetype = Variable), size=0.4)+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_color_manual(name= "", labels = c("Inflow", "Outflow"), values=c("#F8766D", "#000000"))+
  scale_linetype_manual(name= "", labels = c("Inflow", "Outflow"), values=c("solid", "twodash"))+
  labs(title = " ", x="", y = bquote("Flow" ~~ "("~"m³.s" ^ "-1"~")"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")

ggsave(filename = "img/[A] Figures ENG/[TimeSeries] flow (Figure 9).png", plot= plot, device = "png", width = 16, height = 8, units = "cm")
