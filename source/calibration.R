# Comparação k e v ########################################################

###########################################################################

.df <- left_join(jurP,jur)  %>%
    rename(P=PT)%>%
    mutate (res = "Jurumirim") %>%
    select(Data, P, Cenario)

k_list <- c(0, 0.001, 0.005, 0.010)
v_list <- c(0, 0.001, 0.005, 0.010)

df_calib <- data.frame()
for (k in k_list){
  for (v in v_list){
    .df <- .df %>%
      mutate(
        k_= paste0("k=",k),
        v_= paste0("v=",v),
        k = k/86400,
        v = v/86400
      )
    for (j in unique(.df$Cenario)){
        df_cenario <- .df %>% filter(Cenario == j)
        df_calib <- rbind(df_calib, model(df_cenario))
    }
    df_calib <- rbind(df_calib, model(df_cenario))
  }
  df_calib <- rbind(df_calib, model(df_cenario))
}







# old

obs <- read.csv2("dataset/DadosConsolidados.csv", sep=";", dec=",", check.names = F)
obs<-obs %>% gather(key="Var",value = "Valor", c(16:62), na.rm=T)
obs$Valor <- as.numeric(obs$Valor)
obs <- obs %>%
  group_by(Data, km, Estacao, Var, Reservatorio, Local) %>%
  summarize(Valor = mean(Valor, na.rm = TRUE))
obs$Data<- as.Date(obs$Data,format="%d/%m/%Y")
obs_P <- obs %>%
  filter(Var=="PT")%>%
  filter(Valor < 10)
#View(obs_P)


# Cálculo de médias -------------------------------------------------------
jur_res <- obs_P %>% filter(Reservatorio=="Jurumirim")


df<-bind_rows(read.csv2("csv/k e v/TAB2_k=0_v=0.01.csv")        %>% mutate (k="k=0   ",v="v=0.01")   %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.001_v=0.01.csv")    %>% mutate (k="k=0.001",v="v=0.01")  %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.005_v=0.01.csv")    %>% mutate (k="k=0.005",v="v=0.01")  %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.01_v=0.01.csv")     %>% mutate (k="k=0.01",v="v=0.01")   %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0_v=0.001.csv")       %>% mutate (k="k=0   ",v="v=0.001")  %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.001_v=0.001.csv")   %>% mutate (k="k=0.001",v="v=0.001") %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.005_v=0.001.csv")   %>% mutate (k="k=0.005",v="v=0.001") %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.01_v=0.001.csv")    %>% mutate (k="k=0.01",v="v=0.001")  %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0_v=0.csv")           %>% mutate (k="k=0   ",v="v=0")      %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.001_v=0.csv")       %>% mutate (k="k=0.001",v="v=0")     %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.005_v=0.csv")       %>% mutate (k="k=0.005",v="v=0")     %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB2_k=0.01_v=0.csv")        %>% mutate (k="k=0.01",v="v=0")      %>% filter(X>=366) %>% mutate(Data=seq(.start,.end, by=1)),
              read.csv2("csv/k e v/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0.01", Data=NA),
              read.csv2("csv/k e v/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0.001", Data=NA),
              read.csv2("csv/k e v/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0", Data=NA)      
              )

plot<- df%>%
       ggplot()+
       geom_boxplot(aes(x=k,y=Jurumirim))+
       facet_grid(v~.)+
       theme_bw()+
       geom_hline(yintercept = median(jur_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
       labs(x = "", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"), title = "Jurumirim")
ggsave(filename = "img/Calibration/[Boxplot] - k_v_Jurumirim.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")
#theme(axis.text.x = element_blank(),axis.ticks.x.bottom = element_blank())

plot<- df%>%
  ggplot()+
  geom_boxplot(aes(x=k,y=Jurumirim))+
  facet_grid(v~.)+
  theme_bw()+
  geom_hline(yintercept = median(jur_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"), title = "Jurumirim - ZOOM")+
  scale_y_continuous(limits=c(0,0.1))
ggsave(filename = "img/Calibration/[Boxplot] - k_v_Jurumirim_zoom.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")
#theme(axis.text.x = element_blank(),axis.ticks.x.bottom = element_blank())


median(jur_res$Valor)
desvio <- df %>% 
group_by(k,v) %>%
summarize (Simulado=median(Jurumirim)) %>%
mutate(
  Observado=median(jur_res$Valor),
  Desvio=abs(Simulado-Observado),
  DesvioPercentual=((Desvio/Observado)-1)*100,
  DesvioPercentual2=((Observado/Desvio)-1)*100,
) 
desvio
write.csv2(desvio, "csv/[Calibração] Desvio.csv")
min(desvio[1:12,]$Desvio)
filter(desvio, Desvio==min(desvio[1:12,]$Desvio))

df2<-bind_rows(read.csv2("csv/k e v/TAB2_k=0_v=0.01.csv")            %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0   ",v="v=0.01"),
               read.csv2("csv/k e v/TAB2_k=0.001_v=0.01.csv")        %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.001",v="v=0.01"),
               read.csv2("csv/k e v/TAB2_k=0.005_v=0.01.csv")        %>% filter(X<366)  %>% mutate (Data=seq(as.Date("2011-01-01"),as.Date("2011-12-31"), by=1), Selected="Not selected",k="k=0.005",v="v=0.01"),
               read.csv2("csv/k e v/TAB2_k=0.005_v=0.01.csv")        %>% filter(X>=366) %>% mutate (Data=seq(.start,.end, by=1), Selected="Selected",k="k=0.005",v="v=0.01"),
               read.csv2("csv/k e v/TAB2_k=0.01_v=0.01.csv")         %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.01",v="v=0.01"),
               read.csv2("csv/k e v/TAB2_k=0_v=0.001.csv")           %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0   ",v="v=0.001"),
               read.csv2("csv/k e v/TAB2_k=0.001_v=0.001.csv")       %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.001",v="v=0.001"),
               read.csv2("csv/k e v/TAB2_k=0.005_v=0.001.csv")       %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.005",v="v=0.001"),
               read.csv2("csv/k e v/TAB2_k=0.01_v=0.001.csv")        %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.01",v="v=0.001"),
               read.csv2("csv/k e v/TAB2_k=0_v=0.csv")               %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0   ",v="v=0"),
               read.csv2("csv/k e v/TAB2_k=0.001_v=0.csv")           %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.001",v="v=0"),
               read.csv2("csv/k e v/TAB2_k=0.005_v=0.csv")           %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.005",v="v=0"),
               read.csv2("csv/k e v/TAB2_k=0.01_v=0.csv")            %>% mutate (Data=seq(as.Date("2011-01-01"),.end, by=1), Selected="Not selected",k="k=0.01",v="v=0")
)

plot<- df2 %>%
  filter(k != "OBS") %>% 
  gather(key = "res",value = "valor", c(2:4))%>%
  filter(res=="Jurumirim")%>%
  ggplot()+
  geom_rect(aes(xmin = as.Date("2011-01-01"), ymin = -Inf,
                xmax = as.Date("2011-12-31"),   ymax = Inf, 
                fill = "Warm-up"), 
                alpha = 1)+
  geom_rect(aes(xmin = .start, ymin = -Inf,
                xmax = .end,   ymax = Inf, 
                fill = "Simulation"), 
                alpha = 1)+
  scale_fill_manual(values= c("Warm-up"="#FFF79F",
                              "Simulation"="#BDD8FF"), 
                    name=" ")+
  geom_line(aes(x=Data,y=valor, color=Selected), size=0.5)+
  facet_grid(v~k)+
  theme_bw()+
  labs(x = " ", y = bquote("Phosphorus" ~~ "("~"mg.L" ^ "-1"~")"))+
  scale_color_manual(name= " ", values=c("#FF7664","#000000"))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="4 months",expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="right")

ggsave(filename = "img/Calibration/[TimeSeries] - k_v_Jurumirim.png", plot= plot, device = "png", width = 20, height = 11.5, units = "cm")
#
