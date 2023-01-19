variable_names <- c(
  "Setor 1" = "Sector 1" ,
  "Setor 2" = "Sector 2" ,
  "Setor 3" = "Sector 3" ,
  "Setor 4" = "Sector 4" 
)



plot <- ggplot()+
  geom_line(data=comp_sector_B12, aes(x= Data, y=Valor, color=model, linetype=model), size=0.8)+
  facet_grid(.~Setor,labeller = labeller(Setor = variable_names))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.4,0.1),limits=c(0,0.4),expand = c(0,0),labels = seq(0.0,0.4,0.1))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_color_manual(labels=c( "Model B", "Delft3D"),name = " ", values= c("#000000","#00BA38"))+
  scale_linetype_manual(labels=c( "Model B", "Delft3D"),name = " ",values = c("dotted","solid"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")


ggsave(filename = "img/Comparison/[TimeSeries] Sectors.png", plot= plot, device = "png", width = 16, height = 10, units = "cm")


