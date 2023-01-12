
plot <- ggplot()+ 
  geom_boxplot(data=comp_sector, aes(x = model, y=Valor, fill=model), size=0.3, outlier.size = 0.4, outlier.alpha=0.4, outlier.color='grey')+
  facet_grid(Cenario~Setor)+
  scale_y_continuous(breaks = seq(0.0,1.0,0.2),limits=c(0,1.1),expand = c(0,0),labels = seq(0.0,1.0,0.2))+
  theme_bw()+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_fill_manual(labels=c( "Model B", "Delft3D"),name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")


ggsave(filename = "img/Comparison/[Boxplot] Sectors.png", plot= plot, device = "png", width = 14, height = 20, units = "cm")


comp_sector%>% View()

