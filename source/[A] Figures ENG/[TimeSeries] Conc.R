
# Gráfico cargas - ING ----------------------------------------------------

plot <- df_wide_A %>% select(Data, Cenario, res, C_in, C_out)%>%
  filter(Data>= .start & Data <= .end)%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Var), linewidth = 0.4)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(.~Cenario, scales="free_y")+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))

ggsave(filename = "img/[A] Figures ENG/[TimeSeries] Concentrações.png", plot= plot, device = "png", width = 20, height = 14, units = "cm")

