

variable_names <- c(
  "Setor 1" = "Sector 1" ,
  "Setor 2" = "Sector 2" ,
  "Setor 3" = "Sector 3" ,
  "Setor 4" = "Sector 4" 
)
variable_names2 <- c(
  "B12" = "B12" ,
  "T25" = "T25" ,
  "T35" = "T35" ,
  "A25" = "A25" ,
  "A35" = "A35" 
)

# Gráfico (Inglês) -------------------------------------------------------
plot <- df_results_B %>%
  filter(Data>= .start & Data <= .end)%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="3 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(Setor~Cenario,labeller = labeller(Setor = variable_names, ano = variable_names2))+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.005,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=9, 
                fill="Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("Ultraoligotrophic"="#BDD8FF",
                              "Oligotrophic"="#EEBDFF",
                              "Mesotrophic"="#FFCCBB",
                              "Eutrophic"="#FFF79F",
                              "Supereutrophic"="#B4FF9E",
                              "Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] TSI B12.png", plot= plot, device = "png", width = 16, height = 10, units = "cm")



.df <- df_results_B %>%
  filter(Data>= .start & Data <= .end)
plot <-   .df  %>%
  ggplot()+
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
  scale_x_date(date_labels = "%b", date_breaks ="3 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(Setor~Cenario,labeller = labeller(Setor = variable_names, ano = variable_names2))+
  labs(title = " ", x="", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.1)
  

ggsave(filename = "img/[B] Figures ENG/[TimeSeries] CONAMA B12.png", plot= plot, device = "png", width = 16, height = 16, units = "cm")
