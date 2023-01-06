
plot <- ggplot()+
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
                xmax = .end,   ymax = 0.11, 
                fill = "Class 4"), 
                alpha = 0.6)+
  scale_fill_manual(values= c("Class 1"="#BDD8FF",
                              "Class 2"="#B4FF9E",
                              "Class 3"="#EEBDFF",
                              "Class 4"="#FFF79F"), 
                    name=" ")+
  geom_line(data=comp, aes(x= Data, y=Valor, color=Font, linetype=Font), size=0.8)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.11,0.01),limits=c(0,0.11),expand = c(0,0),labels = seq(0.0,0.11,0.01))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_color_manual(labels=c("Model A", "Model B", "Delft3D"),name = " ", values= c("#F8766D","#000000","#00BA38"))+
  scale_linetype_manual(labels=c("Model A", "Model B", "Delft3D"),name = " ",values = c("dashed","dotted","solid"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")


ggsave(filename = "img/Comparison/[TimeSeries].png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

