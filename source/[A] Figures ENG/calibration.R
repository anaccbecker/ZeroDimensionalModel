# Gráfico 2012 (inglês) ---------------------------------------------------

dat_text <- data.frame(
  label = c(paste("k =", filter(todosP,res=="Jurumirim")[1,"k"]*86400,"/day;  apparent settling velocity =",
                  filter(todosP,res=="Jurumirim")[1,"set_v"]*86400,"m/day;  \n mean (measured) =",
                  round(mean(jur_res$Valor), digits = 3),"mg/L;  mean (simulated) =",
                  round(mean(jurP12$C_out), digits = 3),"mg/L"),
            paste("k =", filter(todosP,res=="Chavantes")[1,"k"]*86400,"/day;  apparent settling velocity =",
                  filter(todosP,res=="Chavantes")[1,"set_v"]*86400,"m/day;  \n mean (measured) =",
                  round(mean(cha_res$Valor), digits = 3),"mg/L;  mean (simulated) =",
                  round(mean(chaP12$C_out), digits = 3),"mg/L"), 
            paste("k =", filter(todosP,res=="Capivara")[1,"k"]*86400,"/day;  apparent settling velocity =",
                  filter(todosP,res=="Capivara")[1,"set_v"]*86400,"m/day;  \n mean (measured) =",
                  round(mean(cap_res$Valor), digits = 3),"mg/L;  mean (simulated) =",
                  round(mean(capP12$C_out), digits = 3),"mg/L")),
  res   = c("Jurumirim", "Chavantes", "Capivara"),
  #y_label = c(0.7, 0.75, 0.2)
  y_label = c(0.7, 0.7, 0.7)
)
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_text(data = dat_text,mapping = aes(x = as.Date("2012-10-01"), y = y_label, label = label), 
            size=3.5, vjust = 1)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - Calibração - 2012 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")
