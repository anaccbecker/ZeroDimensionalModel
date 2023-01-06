

load_duration_input <- df_cargas_A %>% 
  select(Data, Cenario, res, w_in, w_lim_in) %>% 
  filter(Data>= .start & Data <= .end)%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(Cenario, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))%>%
  mutate(local="input")

load_duration_input$Cenario<- factor(load_duration_input$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))

plot <-  
  load_duration_input %>% 
  ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(.~Cenario, scales="free")+
  labs(title = " ", x="", y = "Phosphorus (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Input limit"),values = c("Black","Red"))+
  scale_linetype_manual(values = c("solid","dashed"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))

ggsave(filename = "img/[A] Figures ENG/[LoadDuration] Input.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Perm saida ing---------------------------------------------------------

load_duration_output <- df_cargas_A %>% 
  select(Data, Cenario, res, w_out, w_lim_out) %>% 
  filter(Data>= .start & Data <= .end)%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(Cenario, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))%>%
  mutate(local="output")

load_duration_output$Cenario<- factor(load_duration_output$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))

plot <-  
  load_duration_output %>% 
  ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(.~Cenario)+
  labs(title = " ", x="", y = "Phosphorus (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Output limit", "Output"),values = c("Red","Black"))+
  scale_linetype_manual(values = c("dashed","solid"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))

ggsave(filename = "img/[A] Figures ENG/[LoadDuration] Output.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


load_duration <- rbind(load_duration_input, load_duration_output)
plot <-  
  load_duration%>% 
  ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(local~Cenario)+
  labs(title = " ", x="", y = "Phosphorus (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c( "Values", "Limit","Limit", "Values"),values = c("Black","Red","Red","Black"))+
  scale_linetype_manual(values = c( "solid","dashed","dashed","solid"), guide = 'none')+
  theme(legend.position="none")+
  #scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
  scale_y_log10(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))

ggsave(filename = "img/[A] Figures ENG/[LoadDuration] In and Out.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


medias <- df_cargas_A%>%
groupby()
