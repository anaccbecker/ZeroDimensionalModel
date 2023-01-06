
plot <- ggplot()+ 
  geom_boxplot(data=comp_sector, aes(y=Valor, fill=model), size=0.3, outlier.size = 0.4, outlier.alpha=0.4, outlier.color='grey')+
  facet_grid(Cenario~Setor)+
  scale_y_continuous(breaks = seq(0.0,0.4,0.1),limits=c(0,0.5),expand = c(0,0),labels = seq(0.0,0.4,0.1))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_fill_manual(labels=c( "Model B", "Delft3D"),name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")


ggsave(filename = "img/Comparison/[Boxplot] Sectors.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


