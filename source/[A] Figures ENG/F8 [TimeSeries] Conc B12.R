
.df <- df_wide_A %>% 
  filter(Cenario=="B12")%>%
  select(Data, Cenario, res, C_in, C_out)%>%
  filter(Data>= .start & Data <= .end)%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) 
  
plot <-  ggplot()+
  geom_rect(aes(xmin = .start, ymin = 0.000,
                xmax = .end,   ymax = 0.020, 
                fill = "Class 1"), 
                alpha = 0.6)+
  geom_rect(aes(xmin = .start, ymin = 0.020,
                xmax = .end,   ymax = 0.030, 
                fill = "Class 2"), 
                alpha = 0.6)+
  geom_rect(aes(xmin = .start, ymin = 0.030,
                xmax = .end,   ymax = 0.050, 
                fill = "Class 3"), 
                alpha = 0.6)+
  geom_rect(aes(xmin = .start, ymin = 0.050,
                xmax = .end,   ymax = max(.df$Valor), 
                fill = "Class 4"), 
                alpha = 0.6)+
  scale_fill_manual(values= c("Class 1"="#BDD8FF",
                              "Class 2"="#B4FF9E",
                              "Class 3"="#EEBDFF",
                              "Class 4"="#FFF79F"), 
                    name=" ")+
  geom_line(data= .df ,aes( x= Data, y=Valor, color=Var, linetype=Var), linewidth = 0.4)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months", expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), labels = function(x) format(x,  scientific = FALSE,big.mark = " "))+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_color_manual(name= "", labels = c("Input", "Output"), values=c("#FF7664","#000000"))+
  scale_linetype_manual(name= "", labels = c("Input", "Output"), values=c("solid", "twodash"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")


ggsave(filename = "img/[A] Figures ENG/[TimeSeries] Concentrações B12 (Figure 8).png", plot= plot, device = "png", width = 16, height = 14, units = "cm")




