
# Setores - ENTRADA (Inglês) ---------------------------------------
plot <- df_results %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(Setor~Cenario)+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Ultraoligotrófico"="#BDD8FF",
                             "Oligotrófico"="#EEBDFF",
                             "Mesotrófico"="#FFCCBB",
                             "Eutrófico"="#FFF79F",
                             "Supereutrófico"="#B4FF9E",
                             "Hipereutrófico"="#67E1B0"), 
                    name="Trophic State Index",
                    labels= c("Ultraoligotrófico"="Ultraoligotrophic",
                              "Oligotrófico"="Oligotrophic",
                              "Mesotrófico"="Mesotrophic",
                              "Eutrófico"="Eutrophic",
                              "Supereutrófico"="Supereutrophic",
                              "Hipereutrófico"="Hipereutrophic"))+
  labs(title="Trophic State Index (Inflow)")

ggsave(filename = "img/[B] Figures ENG/[pieCharts] TSI Input.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")
table <- df_results %>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%filter(Variable=="C_in")%>% group_by(TSI,Setor,Cenario) %>% count(TSI) %>% spread(key = "Cenario", value =  "n")
#write.csv2(table, file = "../output/setores/[Setores] IET ENTRADA.csv")


# Setores - SAÍDA (Inglês) ------------------------------------------
plot <- df_results %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(Setor~Cenario)+
  theme_bw()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Ultraoligotrófico"="#BDD8FF",
                             "Oligotrófico"="#EEBDFF",
                             "Mesotrófico"="#FFCCBB",
                             "Eutrófico"="#FFF79F",
                             "Supereutrófico"="#B4FF9E",
                             "Hipereutrófico"="#67E1B0"), 
                    name="Trophic State Index",
                    labels= c("Ultraoligotrófico"="Ultraoligotrophic",
                              "Oligotrófico"="Oligotrophic",
                              "Mesotrófico"="Mesotrophic",
                              "Eutrófico"="Eutrophic",
                              "Supereutrófico"="Supereutrophic",
                              "Hipereutrófico"="Hipereutrophic"))+
  labs(title="Trophic State Index (Outflow)")

ggsave(filename = "img/[B] Figures ENG/[pieCharts] TSI Output.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")
table<- df_results%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(Variable=="C_out")%>% group_by(TSI,Setor,Cenario) %>% count(TSI) %>% spread(key = "Cenario", value =  "n")
#write.csv2(table, file = "../output/setores/[Setores] IET SAÍDA.csv")
