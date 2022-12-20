library(tidyverse)
library (reshape2)
library(ggpubr)
library(scales)
library(lubridate)


# Leitura dados de vazão e volume -----------------------------------------

jur <- read.table("../dataset/Dados_Jurumirim/vazao_SAR_JURUMIRIM.txt", head=T)
jur$Data <- as.Date(jur$Data,format="%Y-%m-%d" )
jur <- jur %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
names(jur)[7] <- "Vol"
cha <- read.table("../dataset/Dados_Chavantes/vazao_SAR_CHAVANTES.txt", head=T)
cha$Data <- as.Date(cha$Data,format="%Y-%m-%d" )
cha <- cha %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
names(cha)[7] <- "Vol"
cap <- read.table("../dataset/Dados_Capivara/vazao_SAR_CAPIVARA.txt", head=T)
cap$Data <- as.Date(cap$Data,format="%Y-%m-%d" )
cap <- cap %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
names(cap)[7] <- "Vol"


# Calculando os volumes (hm³) ---------------------------------------------

jur$Vol <- jur$Vol* 7007.51  /100   #hm³
cha$Vol <- cha$Vol* 8795.10  /100   #hm³
cap$Vol <- cap$Vol* 10541.35 /100   #hm³


# Calculando os Níveis d'água (m) - NAs -----------------------------------
jur <- jur %>% mutate(NAs_calc =             0*Vol^4 + 7.32046E-12*Vol^3 - 2.54809E-7*Vol^2 + 4.72244E-3*Vol + 5.44901E2)
cha <- cha %>% mutate(NAs_calc =             0*Vol^4 + 1.02859E-11*Vol^3 - 3.67479E-7*Vol^2 + 6.57360E-3*Vol + 4.37613E2)
cap <- cap %>% mutate(NAs_calc = - 6.26514E-16*Vol^4 + 2.78073E-11*Vol^3 - 5.25369E-7*Vol^2 + 6.48478E-3*Vol + 2.99183E2)


#plot<-jur%>% 
#  select(Data, Cota, NAs_calc) %>% 
#  melt(measure.vars = c("Cota","NAs_calc"),
#             id.vars = c("Data"), 
#             variable.name="Variable",
#             value.name = "Valor")%>%
#ggplot()+
#  geom_line(aes(x= Data,y= Valor, color=Variable))+
#scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
#  labs(title = " ", x="", y = "Cota")+
#  theme_bw()+
#  scale_color_discrete(name= "", labels = c("Cota medida", "Cota calculada - CAV"))+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
#plot
#ggsave(filename = "../img/unsteady12/[TimeSeries]Cotas - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Calculando as áreas de interface (km²) - As -----------------------------

jur <- jur %>% mutate(As = 2*(0*Cota^4 +          0*Cota^3 + 4.00081E-1*Cota^2 - 4.359540E2*Cota + 1.189940E5))
cha <- cha %>% mutate(As = 2*(0*Cota^4 +          0*Cota^3 + 7.61249E-2*Cota^2 - 5.952470E1*Cota + 1.151130E4))
cap <- cap %>% mutate(As = 2*(0*Cota^4 + 2.20196E-2*Cota^3 - 2.13817E+1*Cota^2 + 6.938390E3*Cota - 7.520320E5))


# Ajuste de unidades [Vol - m³ / As - m²] ---------------------------------

jur$Vol <- jur$Vol*1000000
cha$Vol <- cha$Vol*1000000
cap$Vol <- cap$Vol*1000000
jur$As <- jur$As*1000000
cha$As <- cha$As*1000000
cap$As <- cap$As*1000000


# Leitura dados de fósforo 2012 -------------------------------------------

jurP12 <- read.table("../dataset/Dados_Jurumirim/PT_SS_AR1.txt", head=T)[,c(1,2)]
jurP12$Data <- as.Date(jurP12$Data,format="%Y-%m-%d" )
jurP12 <- jurP12 %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
chaP12 <- read.table("../dataset/Dados_Chavantes/PT_SS_AR1.txt", head=T)[,c(1,2)]
chaP12$Data <- as.Date(chaP12$Data,format="%Y-%m-%d" )
chaP12 <- chaP12 %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
capP12 <- read.table("../dataset/Dados_Capivara/PT_SS_AR1.txt", head=T)[,c(1,2)]
capP12$Data <- as.Date(capP12$Data,format="%Y-%m-%d" )
capP12 <- capP12 %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))


# Leitura dados de fósforo 2025 tendencial --------------------------------

jurP25t <- read.table("../dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2025_Tendencial.txt", head=T)[,c(1,7)]
jurP25t$Data <- as.Date(jurP25t$Data,format="%Y-%m-%d" )
jurP25t <- jurP25t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
chaP25t <- read.table("../dataset/Cenarios_Chavantes/Entradas_GLM_Cenario_2025_Tendencial.txt", head=T)[,c(1,7)]
chaP25t$Data <- as.Date(chaP25t$Data,format="%Y-%m-%d" )
chaP25t <- chaP25t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
capP25t <- read.table("../dataset/Cenarios_Capivara/Entradas_GLM_Cenario_2025_Tendencial.txt", head=T)[,c(1,7)]
capP25t$Data <- as.Date(capP25t$Data,format="%Y-%m-%d" )
capP25t <- capP25t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))



# Leitura dados de fósforo 2025 acelerado ---------------------------------

jurP25a <- read.table("../dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2025_Acelerado.txt", head=T)[,c(1,7)]
jurP25a$Data <- as.Date(jurP25a$Data,format="%Y-%m-%d" )
jurP25a <- jurP25a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
chaP25a <- read.table("../dataset/Cenarios_Chavantes/Entradas_GLM_Cenario_2025_Acelerado.txt", head=T)[,c(1,7)]
chaP25a$Data <- as.Date(chaP25a$Data,format="%Y-%m-%d" )
chaP25a <- chaP25a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
capP25a <- read.table("../dataset/Cenarios_Capivara/Entradas_GLM_Cenario_2025_Acelerado.txt", head=T)[,c(1,7)]
capP25a$Data <- as.Date(capP25a$Data,format="%Y-%m-%d" )
capP25a <- capP25a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))


# Leitura dados de fósforo 2035 tendencial --------------------------------

jurP35t <- read.table("../dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2035_Tendencial.txt", head=T)[,c(1,7)]
jurP35t$Data <- as.Date(jurP35t$Data,format="%Y-%m-%d" )
jurP35t <- jurP35t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
chaP35t <- read.table("../dataset/Cenarios_Chavantes/Entradas_GLM_Cenario_2035_Tendencial.txt", head=T)[,c(1,7)]
chaP35t$Data <- as.Date(chaP35t$Data,format="%Y-%m-%d" )
chaP35t <- chaP35t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
capP35t <- read.table("../dataset/Cenarios_Capivara/Entradas_GLM_Cenario_2035_Tendencial.txt", head=T)[,c(1,7)]
capP35t$Data <- as.Date(capP35t$Data,format="%Y-%m-%d" )
capP35t <- capP35t %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))


# Leitura dados de fósforo 2035 acelerado ---------------------------------

jurP35a <- read.table("../dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2035_Acelerado.txt", head=T)[,c(1,7)]
jurP35a$Data <- as.Date(jurP35a$Data,format="%Y-%m-%d" )
jurP35a <- jurP35a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
chaP35a <- read.table("../dataset/Cenarios_Chavantes/Entradas_GLM_Cenario_2035_Acelerado.txt", head=T)[,c(1,7)]
chaP35a$Data <- as.Date(chaP35a$Data,format="%Y-%m-%d" )
chaP35a <- chaP35a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
capP35a <- read.table("../dataset/Cenarios_Capivara/Entradas_GLM_Cenario_2035_Acelerado.txt", head=T)[,c(1,7)]
capP35a$Data <- as.Date(capP35a$Data,format="%Y-%m-%d" )
capP35a <- capP35a %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))


# Cargas Laterais, k e set_v ----------------------------------------------
jurP12  <- bind_cols(jur,jurP12)   %>% mutate (res = "Jurumirim",  W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.010/86400)
chaP12  <- bind_cols(cha, chaP12)   %>% mutate (res = "Chavantes", W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
capP12  <- bind_cols(cap,capP12)    %>% mutate (res = "Capivara",  W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
jurP25t <- bind_cols(jur,jurP25t)   %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.010/86400)
chaP25t <- bind_cols(cha, chaP25t)  %>% mutate (res = "Chavantes", W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
capP25t <- bind_cols(cap,capP25t)   %>% mutate (res = "Capivara",  W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
jurP25a <- bind_cols(jur,jurP25a)   %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.010/86400)
chaP25a <- bind_cols(cha, chaP25a)  %>% mutate (res = "Chavantes", W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
capP25a <- bind_cols(cap,capP25a)   %>% mutate (res = "Capivara",  W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
jurP35t <- bind_cols(jur,jurP35t)   %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.010/86400)
chaP35t <- bind_cols(cha, chaP35t)  %>% mutate (res = "Chavantes", W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
capP35t <- bind_cols(cap,capP35t)   %>% mutate (res = "Capivara",  W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
jurP35a <- bind_cols(jur,jurP35a)   %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.010/86400)
chaP35a <- bind_cols(cha, chaP35a)  %>% mutate (res = "Chavantes", W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)
capP35a <- bind_cols(cap,capP35a)   %>% mutate (res = "Capivara",  W =  0E3/24/60/60, k=  0.010/86400, set_v = 0.010/86400)


# Modelo ------------------------------------------------------------------

model <- function(df){
  df<- df %>% mutate(i=1:nrow(df), t=86400*i, C_in= df$PT, C_out= NA)
  df[1,"C_out"]= 0
  for (i in 2:nrow(df)){
    df[i, "C_out"] = (df[i-1,"C_out"]+(df[i, "t"]-df[i-1, "t"])/df[i, "Vol"] *(df[i, "Q_Aflu"]*df[i, "C_in"]+df[i,"W"]))/(1+(df[i, "t"]-df[i-1, "t"])/df[i,"Vol"]*(df[i,"Q_Deflu"]+df[i,"k"]*df[i,"Vol"]+ df[i,"set_v"] *df[i,"As"]+(df[i,"Vol"]-df[i-1,"Vol"])/(df[i,"t"]-df[i-1,"t"])))
    
    if ( df[i, "C_out"] < 0){
      df[i, "C_out"] = 0 
    }   
 
    } 
    print(df)
}

  jurP12  <- model(jurP12)
  chaP12  <- model(chaP12)
  capP12  <- model(capP12)
  jurP25t <- model(jurP25t)
  chaP25t <- model(chaP25t)
  capP25t <- model(capP25t)
  jurP25a <- model(jurP25a)
  chaP25a <- model(chaP25a)
  capP25a <- model(capP25a)
  jurP35t <- model(jurP35t)
  chaP35t <- model(chaP35t)
  capP35t <- model(capP35t)
  jurP35a <- model(jurP35a)
  chaP35a <- model(chaP35a)
  capP35a <- model(capP35a)
  
  
# Juntando os dados em um mesmo data frame --------------------------------

todosP12  <- bind_rows(jurP12,chaP12,capP12)    %>% mutate(ano="B12")
todosP25t <- bind_rows(jurP25t,chaP25t,capP25t) %>% mutate(ano="T25")
todosP25a <- bind_rows(jurP25a,chaP25a,capP25a) %>% mutate(ano="A25")
todosP35t <- bind_rows(jurP35t,chaP35t,capP35t) %>% mutate(ano="T35")
todosP35a <- bind_rows(jurP35a,chaP35a,capP35a) %>% mutate(ano="A35")

todosP <- bind_rows(todosP12,todosP25t,todosP25a,todosP35t,todosP35a)
todosP$res <- factor(todosP$res, levels = c("Jurumirim", "Chavantes","Capivara"))
todosP$ano <- factor(todosP$ano, levels = c("B12","T25", "A25","T35","A35"))
#View(todosP)
write.csv2(todosP, file = "../output/resultado_unsteady12.csv")
#nrow(todosP)

# Leitura Dados Observados APENAS PT --------------------------------------

obs<- read.csv2("../dataset/DadosConsolidados.csv")
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
nrow(jur_res)
mean(jur_res$Valor)
mean(jurP12$C_out)

cha_res <- obs_P %>% filter(Reservatorio=="Chavantes")
nrow(cha_res)
mean(cha_res$Valor)
mean(chaP12$C_out)

cap_res <- obs_P %>% filter(Reservatorio=="Capivara")
nrow(cap_res)
mean(cap_res$Valor)
mean(capP12$C_out)

#tabela<-data.frame(Reservatorio = c("Jurumirim","Chavantes","Capivara"),
#           N= c(nrow(jur_res),nrow(cha_res),nrow(cap_res)),
#           Med_obs= c(mean(jur_res$Valor),mean(cha_res$Valor),mean(cap_res$Valor)),
#           Med_sim= c(mean(jurP12$C_out2),mean(chaP12$C_out2),mean(capP12$C_out2)))
#View(tabela)
#write.csv2(tabela, file = "../output/unsteady13/TAB2_k=0.005_v=0.01.csv")

#tabela2<-data.frame(Jurumirim = jurP12$C_out2,
#                    Chavantes = chaP12$C_out2,
#                    Capivara  = capP12$C_out2)
 
#write.csv2(tabela2, file = "../output/unsteady13/TAB2_k=0_v=0.csv") ###############################################

#tabela3<-bind_cols(Jurumirim = jur_res$Valor,
#                    Chavantes = cha_res$Valor,
#                    Capivara  = cap_res$Valor)
#
#write.csv2(tabela3, file = "../output/unsteady12/TAB3_OBS.csv")

# Derretendo --------------------------------------------------------------
df_melt<- todosP  %>%   melt(measure.vars = c("C_in","C_out"),
                          id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res"), 
                          variable.name="Variable",
                          value.name = "Valor")
#View(df_melt)


# IET ---------------------------------------------------------------------

TSI <- function(p){
      if(p<0.012){
        TSI="6-Ultraoligotrófico"
      }
      else if(p<0.032){
        TSI="5-Oligotrófico"
      }
      else if(p<0.125){
        TSI="4-Mesotrófico"
      }
      else if(p<0.269){
        TSI="3-Eutrófico"
      }
      else if(p<0.581){
        TSI="2-Supereutrófico"
      }
      if (p>0.581){
        TSI="1-Hipereutrófico"
      }
      return(TSI)
    }

TSI(3)
df_melt<-df_melt%>% mutate(TSI=NA)
df_melt$TSI<- sapply(df_melt$Valor, FUN= TSI)


# CONAMA -----------------------------------------------------------------

classe2 <- function(p) {
  if(p<=0.03){
    return("Em conformidade")
  }
  else if(p<=0.09){
    return("Supera em até 03x")
  }
  else if(p<=0.3){
    return("Supera em até 10x")
  }
  else{
    return("Supera em mais de 10 x")
  }
}
classe2(0.09)

df_melt<-df_melt%>% mutate(classe2=NA)
df_melt$classe2<- sapply(df_melt$Valor,classe2)
#View(df_melt)
#write.csv2(df_melt,"../output/df_melt.csv")


# Gráficos Calibração #####################################################

###########################################################################

#View(df_melt)

# Gráfico 2012 (português) ------------------------------------------------

dat_text <- data.frame(
  label = c(paste("k =", filter(todosP,res=="Jurumirim")[1,"k"]*86400,"/dia;  velocidade aparente de sedimentação =",
                  filter(todosP,res=="Jurumirim")[1,"set_v"]*86400,"m/dia;  \n média observada =",
                  round(mean(jur_res$Valor), digits = 3),"mg/L;  média simulada =",
                  round(mean(jurP12$C_out), digits = 3),"mg/L"),
            paste("k =", filter(todosP,res=="Chavantes")[1,"k"]*86400,"/dia;  velocidade aparente de sedimentação =",
                  filter(todosP,res=="Chavantes")[1,"set_v"]*86400,"m/dia;  \n média observada =",
                  round(mean(cha_res$Valor), digits = 3),"mg/L;  média simulada =",
                  round(mean(chaP12$C_out), digits = 3),"mg/L"), 
            paste("k =", filter(todosP,res=="Capivara")[1,"k"]*86400,"/dia;  velocidade aparente de sedimentação =",
                  filter(todosP,res=="Capivara")[1,"set_v"]*86400,"m/dia;  \n média observada =",
                  round(mean(cap_res$Valor), digits = 3),"mg/L;  média simulada =",
                  round(mean(capP12$C_out), digits = 3),"mg/L")),
  res   = c("Jurumirim", "Chavantes", "Capivara"),
  #y_label = c(0.7, 0.75, 0.2)
  y_label = c(0.7, 0.7, 0.7)
)

Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_text(data = dat_text,mapping = aes(x = as.Date("2012-10-01"), y = y_label, label = label), 
            size=3.5, vjust = 1)
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - Calibração - 2012 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


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

# Gráficos Cargas #########################################################

###########################################################################

# Teste Cargas ------------------------------------------------------------

todosP <- todosP %>% mutate(w_in= Q_Aflu*C_in/1E3*24*60*60, w_out=Q_Deflu*C_out/1E3*24*60*60, 
                            w_lim_in=Q_Aflu*0.03/1E3*24*60*60, w_lim_out=Q_Deflu*0.03/1E3*24*60*60)
View(todosP)

#OBS: cargas transformadas para kg/dia

write.csv2(todosP,file = "../output/todosP_cargas_oficial.csv")

# Gráfico cargas - POR ----------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- todosP%>%select(Data, ano, res, w_in, w_out)%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Var))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(res~ano, scales="free_y")+
  labs(title = " ", x="", y = "Fósforo (kg/dia)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
ggsave(filename = "../img/unsteady12/01 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Gráfico cargas - ING ----------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- todosP%>%select(Data, ano, res, w_in, w_out)%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Var))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(res~ano, scales="free_y")+
  labs(title = " ", x="", y = "Phosphorus (kg/day)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
ggsave(filename = "../img/unsteady12/01 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Perm entrada port--------------------------------------------------------

Sys.setlocale("LC_TIME", "Portuguese")
df <- todosP %>% 
  select(Data, ano, res, w_in, w_lim_in) %>% 
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(ano, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))
plot <-  df %>% ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(res~ano, scales="free")+
  labs(title = " ", x="", y = "Fósforo (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Carga de entrada", "Limite entrada"),values = c("Black","Red"))+
  scale_linetype_manual(values = c("solid","dashed"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
#dir.create("../img/unsteady12")
ggsave(filename = "../img/unsteady12/03 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Perm entrada ing---------------------------------------------------------

Sys.setlocale("LC_TIME", "English")
df <- todosP %>% 
  select(Data, ano, res, w_in, w_lim_in) %>% 
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(ano, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))
plot <-  df %>% ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(res~ano, scales="free")+
  labs(title = " ", x="", y = "Phosphorus (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Input limit"),values = c("Black","Red"))+
  scale_linetype_manual(values = c("solid","dashed"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
ggsave(filename = "../img/unsteady12/03 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Perm saida port--------------------------------------------------------

Sys.setlocale("LC_TIME", "Portuguese")
df <- todosP %>% 
  select(Data, ano, res, w_out, w_lim_out) %>% 
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(ano, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))
plot <-  df %>% ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(res~ano, scales="free")+
  labs(title = " ", x="", y = "Fósforo (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Limite saída","Carga de saída"),values = c("Red","Black"))+
  scale_linetype_manual(values = c("dashed","solid"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
#dir.create("../img/unsteady12")
ggsave(filename = "../img/unsteady12/04 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Perm saida ing---------------------------------------------------------

Sys.setlocale("LC_TIME", "English")
df <- todosP %>% 
  select(Data, ano, res, w_out, w_lim_out) %>% 
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  group_by(ano, res, Var)%>%
  mutate(rank= 100*cume_dist(desc(Valor)))
plot <-  df %>% ggplot()+
  geom_line(aes(x= rank, y= Valor, color=Var, linetype=Var))+
  facet_grid(res~ano, scales="free")+
  labs(title = " ", x="", y = "Phosphorus (kg/dia)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Output limit", "Output"),values = c("Red","Black"))+
  scale_linetype_manual(values = c("dashed","solid"), guide = 'none')+
  theme(legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
ggsave(filename = "../img/unsteady12/04 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Boxplot cargas - POR ----------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- todosP%>%select(Data, ano, res, w_in, w_out)%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  ggplot()+
  geom_boxplot(aes(x= Var, y=Valor, color=Var ),show.legend = FALSE)+
  facet_grid(res~ano, scales="free_y")+
  labs(title = " ", x="", y = "Fósforo (kg/dia)")+
  theme_bw()+
  scale_x_discrete(labels=c("Entrada","Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
#dir.create("../img/unsteady12")
ggsave(filename = "../img/unsteady12/05 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Boxplot cargas - ING ----------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- todosP%>%select(Data, ano, res, w_in, w_out)%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  gather(key="Var",value = "Valor", c(4:5), na.rm=T) %>% 
  ggplot()+
  geom_boxplot(aes(x= Var, y=Valor, color=Var ),show.legend = FALSE)+
  facet_grid(res~ano, scales="free_y")+
  labs(title = " ", x="", y = "Phosphorus (kg/day)")+
  theme_bw()+
  scale_x_discrete(labels=c("Input","Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
plot
#dir.create("../img/unsteady12")
ggsave(filename = "../img/unsteady12/05 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# TMDL --------------------------------------------------------------------

todosPA <- todosP %>% 
          filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
          select(res,ano,w_in,w_out,w_lim_in,w_lim_out)%>%
          group_by(res,ano) %>% 
          summarise(w_in = sum(w_in),w_out = sum(w_out),w_lim_in = sum(w_lim_in),w_lim_out = sum(w_lim_out))
View(todosPA)

todosPA_melt<- todosPA  %>%   melt(measure.vars = c("w_in","w_out","w_lim_in","w_lim_out"),
                             id.vars = c("ano","res"), 
                             variable.name="Variable",
                             value.name = "Valor")%>% 
               mutate (lim=NA,local=NA)
for (i in 1:nrow(todosPA_melt)){
    if ( todosPA_melt[i, "Variable"] == "w_in" |todosPA_melt[i, "Variable"] == "w_out" ){
      todosPA_melt[i, "lim"] = "carga 2012" 
    }  
    if ( todosPA_melt[i, "Variable"] == "w_lim_in" |todosPA_melt[i, "Variable"] == "w_lim_out" ){
      todosPA_melt[i, "lim"] = "limite" 
    }  
} 
for (i in 1:nrow(todosPA_melt)){
  if ( todosPA_melt[i, "Variable"] == "w_in" |todosPA_melt[i, "Variable"] == "w_lim_in" ){
    todosPA_melt[i, "local"] = "in" 
  }  
  if ( todosPA_melt[i, "Variable"] == "w_out" |todosPA_melt[i, "Variable"] == "w_lim_out" ){
    todosPA_melt[i, "local"] = "out" 
  }  
} 
View(todosPA_melt)
write.csv2(todosPA, file = "../output/todos_PA_cargas.csv")

# total anual - pt --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor, fill=lim),stat="identity")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (ton/ano)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Carga Anual", "Limite Anual"))+
  scale_x_discrete(labels=c("Entrada","Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - Carga - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

  
# total anual - en --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor, fill=lim),stat="identity")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (ton/year)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Annual load", "Annual limits"))+
  scale_x_discrete(labels=c("Input","Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - Carga - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# TMDL - pt --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor/366, fill=lim),stat="identity")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (ton/dia)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Carga diária", "Limite diário"))+
  scale_x_discrete(labels=c("Entrada","Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - TMDL - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# TMDL - en --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor/366, fill=lim),stat="identity")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Daily Load", "Total Maximum Daily Load"))+
  scale_x_discrete(labels=c("Input","Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - TMDL - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# total anual - pt --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor, fill=lim),stat="identity")+
  facet_grid(ano~res)+
  labs(title = " ", x="", y = "Fósforo (ton/ano)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Carga Anual", "Limite Anual"))+
  scale_x_discrete(labels=c("Entrada","Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - Carga folha - Por.png", plot= plot, device = "png", width = 18, height = 25, units = "cm")


# total anual - en --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor, fill=lim),stat="identity")+
  facet_grid(ano~res)+
  labs(title = " ", x="", y = "Phosphorus (ton/year)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Annual load", "Annual limits"))+
  scale_x_discrete(labels=c("Input","Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - Carga folha- Eng.png", plot= plot, device = "png", width = 18, height = 25, units = "cm")

# TMDL - pt --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor/366, fill=lim),stat="identity")+
  facet_grid(ano~res)+
  labs(title = " ", x="", y = "Fósforo (ton/dia)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Carga diária", "Limite diário"))+
  scale_x_discrete(labels=c("Entrada","Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - TMDL folha - Por.png", plot= plot, device = "png", width = 18, height = 25, units = "cm")


# TMDL - en --------------------------------------------------------

plot<- todosPA_melt %>% ggplot()+
  geom_bar(aes(local,y=Valor/366, fill=lim),stat="identity")+
  facet_grid(ano~res)+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  theme_bw()+
  scale_fill_discrete(name= "", labels = c("Daily Load", "Total Maximum Daily Load"))+
  scale_x_discrete(labels=c("Input","Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  scale_y_continuous(labels = function(x) format(x,  scientific = FALSE,big.mark = " "))
ggsave(filename = "../img/unsteady12/[Barpot] - TMDL folha - Eng.png", plot= plot, device = "png", width = 18, height = 25, units = "cm")

# Gráficos Limite CONAMA ##################################################

###########################################################################


# Gráfico (português) -----------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico (inglês) --------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Gráfico (inglês) JURU --------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(res=="Jurumirim")%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  facet_grid(ano~.)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - JURU - Eng.png", plot= plot, device = "png", width = 15, height = 15, units = "cm")

# Gráfico (inglês) JURU --------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(res=="Jurumirim")%>%
  filter(Variable=="C_out")%>%
  ggplot()+
  geom_boxplot(aes(x= ano, y=Valor))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  #scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  #facet_grid(ano~.)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_y_continuous(breaks = c(seq(0.00,0.08,0.01)), limits = c(0,0.085))+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - CONAMA - JURU - Eng.png", plot= plot, device = "png", width = 15, height = 15, units = "cm")

# Gráfico (inglês) JURU --------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(res=="Jurumirim")%>%
  #filter(Variable=="C_out")%>%
  ggplot()+
  geom_boxplot(aes(x= ano, y=Valor,color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  #scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months")+
  #facet_grid(ano~.)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  #scale_y_continuous(breaks = c(seq(0.00,0.08,0.01)), limits = c(0,0.085))+
  scale_y_log10(breaks = c(0.01,0.10,1), limits = c(0.01,1))+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - CONAMA - JURU - Eng.png", plot= plot, device = "png", width = 15, height = 15, units = "cm")

# Gráfico 2012 (português) ------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - 2012 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico 2012 (inglês) ---------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - 2012 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Gráfico 2012 (inglês) JURU ---------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12")%>%
  filter(res=="Jurumirim")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  #facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - CONAMA - 2012 JURU - Eng.png", plot= plot, device = "png", width = 15, height = 11, units = "cm")


# Derretendo novamente ----------------------------------------------------
df_melt2<- todosP  %>%   melt(measure.vars = c("Q_Aflu","Q_Deflu"),
                              id.vars = c("Data","C_in","C_out","ano","res"), 
                              variable.name="Variable",
                              value.name = "Valor")
#View(df_melt2)


# Gráfico 2012 vazão (português) ------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt2%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano, scale="free_y")+
  labs(title = " ", x="", y = "Vazão (m³/s)")+
  theme_bw()+
  scale_color_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - 2012 - Vazão - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico 2012 vazão (inglês) ---------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt2%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  facet_grid(res~ano, scale="free_y")+
  labs(title = " ", x="", y = "Flow (m³/s)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Inflow", "Outflow"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - 2012 - Vazão - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Gráfico 2012 vazão (inglês) JURU---------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt2%>%
  filter(ano=="B12")%>%
  filter(res=="Jurumirim")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_line(aes(x= Data, y=Valor, color=Variable))+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month")+
  #facet_grid(res~ano, scale="free_y")+
  labs(title = " ", x="", y = "Flow (m³/s)")+
  theme_bw()+
  scale_color_discrete(name= " ", labels = c("Inflow", "Outflow"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - 2012 - Vazão - JURU - Eng.png", plot= plot, device = "png", width = 15, height = 11, units = "cm")

# Boxplot (português) --------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_boxplot(aes(x= Variable, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_x_discrete(name= "", labels = c("Entrada", "Saída"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  theme(legend.position="none")
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - CONAMA - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Boxplot (inglês) ------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  geom_boxplot(aes(x= Variable, y=Valor, color=Variable))+
  geom_hline(yintercept = 0.03, color="black",linetype="dashed")+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_x_discrete(name= " ", labels = c("Input", "Output"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  theme(legend.position="none")
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - CONAMA - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráficos Limites IET ####################################################

###########################################################################

# Gráfico (português) -----------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Entrada", "Saída"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.005,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1.8, 
                fill="1-Hipereutrófico"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrófico"="#BDD8FF",
                              "5-Oligotrófico"="#EEBDFF",
                              "4-Mesotrófico"="#FFCCBB",
                              "3-Eutrófico"="#FFF79F",
                              "2-Supereutrófico"="#B4FF9E",
                              "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)
  
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - IET - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")


# Gráfico (inglês) --------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.005,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1.8, 
                fill="1-Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrophic"="#BDD8FF",
                              "5-Oligotrophic"="#EEBDFF",
                              "4-Mesotrophic"="#FFCCBB",
                              "3-Eutrophic"="#FFF79F",
                              "2-Supereutrophic"="#B4FF9E",
                              "1-Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - IET - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")


# Gráfico 2012 (português) ------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Entrada", "Saída"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.005,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1.8, 
                fill="1-Hipereutrófico"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrófico"="#BDD8FF",
                              "5-Oligotrófico"="#EEBDFF",
                              "4-Mesotrófico"="#FFCCBB",
                              "3-Eutrófico"="#FFF79F",
                              "2-Supereutrófico"="#B4FF9E",
                              "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - IET - 2012 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico 2012 (inglês) ---------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.005,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1.8, 
                fill="1-Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrophic"="#BDD8FF",
                              "5-Oligotrophic"="#EEBDFF",
                              "4-Mesotrophic"="#FFCCBB",
                              "3-Eutrophic"="#FFF79F",
                              "2-Supereutrophic"="#B4FF9E",
                              "1-Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - IET - 2012 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Boxplot (português) --------------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_y_log10(expand = c(0,0),labels = function(x) format(x,  scientific = FALSE,big.mark = " "))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  scale_x_discrete(name= " ", labels = c("Entrada", "Saída"))+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Entrada", "Saída"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=-Inf,ymin=0.005,
                xmax= Inf,ymax=0.012, 
                fill="6-Ultraoligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.012,
                xmax= Inf,ymax=0.032, 
                fill="5-Oligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.032,
                xmax= Inf,ymax=0.125, 
                fill="4-Mesotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.125,
                xmax= Inf,ymax=0.269, 
                fill="3-Eutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.269,
                xmax= Inf,ymax=0.581, 
                fill="2-Supereutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.581,
                xmax= Inf,ymax=1.8, 
                fill="1-Hipereutrófico"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrófico"="#BDD8FF",
                              "5-Oligotrófico"="#EEBDFF",
                              "4-Mesotrófico"="#FFCCBB",
                              "3-Eutrófico"="#FFF79F",
                              "2-Supereutrófico"="#B4FF9E",
                              "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  geom_boxplot(aes(x= Variable, y=Valor, color=Variable))
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - IET - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")


# Boxplot (inglês) ------------------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_y_log10(expand = c(0,0),labels = function(x) format(x,  scientific = FALSE,big.mark = " "))+
  facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_x_discrete(name= " ", labels = c("Input", "Output"))+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=-Inf,ymin=0.005,
                xmax= Inf,ymax=0.012, 
                fill="6-Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.012,
                xmax= Inf,ymax=0.032, 
                fill="5-Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.032,
                xmax= Inf,ymax=0.125, 
                fill="4-Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.125,
                xmax= Inf,ymax=0.269, 
                fill="3-Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.269,
                xmax= Inf,ymax=0.581, 
                fill="2-Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=-Inf,ymin=0.581,
                xmax= Inf,ymax=1.8, 
                fill="1-Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrophic"="#BDD8FF",
                              "5-Oligotrophic"="#EEBDFF",
                              "4-Mesotrophic"="#FFCCBB",
                              "3-Eutrophic"="#FFF79F",
                              "2-Supereutrophic"="#B4FF9E",
                              "1-Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_boxplot(aes(x= Variable, y=Valor, color=Variable))
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - IET - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")


# Setores - ENTRADA (Português) ------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("6-Ultraoligotrófico"="#BDD8FF",
                               "5-Oligotrófico"="#EEBDFF",
                               "4-Mesotrófico"="#FFCCBB",
                               "3-Eutrófico"="#FFF79F",
                               "2-Supereutrófico"="#B4FF9E",
                               "1-Hipereutrófico"="#67E1B0"), 
                             name="Índice de Estado Trófico")+
  labs(title="Índice de Estado Trófico na entrada dos reservatórios")
plot
ggsave(filename = "../img/unsteady12/[Setores] - IET Entrada - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

table<- df_melt%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%filter(Variable=="C_in")%>% group_by(TSI,res,ano) %>% count(TSI) %>% spread(key = "ano", value =  "n")
write.csv2(table, file = "../output/unsteady12/2[Setores] IET ENTRADA.csv")

# Setores - ENTRADA (Inglês) ---------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("6-Ultraoligotrófico"="#BDD8FF",
                             "5-Oligotrófico"="#EEBDFF",
                             "4-Mesotrófico"="#FFCCBB",
                             "3-Eutrófico"="#FFF79F",
                             "2-Supereutrófico"="#B4FF9E",
                             "1-Hipereutrófico"="#67E1B0"), 
                    name="Trophic State Index",
                    labels= c("6-Ultraoligotrófico"="6-Ultraoligotrophic",
                              "5-Oligotrófico"="5-Oligotrophic",
                              "4-Mesotrófico"="4-Mesotrophic",
                              "3-Eutrófico"="3-Eutrophic",
                              "2-Supereutrófico"="2-Supereutrophic",
                              "1-Hipereutrófico"="1-Hipereutrophic"))+
  labs(title="Trophic State Index (Inflow)")
plot
ggsave(filename = "../img/unsteady12/[Setores] - IET Entrada - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

# Setores - SAÍDA (Português) --------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("6-Ultraoligotrófico"="#BDD8FF",
                             "5-Oligotrófico"="#EEBDFF",
                             "4-Mesotrófico"="#FFCCBB",
                             "3-Eutrófico"="#FFF79F",
                             "2-Supereutrófico"="#B4FF9E",
                             "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  labs(title="Índice de Estado Trófico na saída dos reservatórios")
plot
ggsave(filename = "../img/unsteady12/[Setores] - IET Saída - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

table<- df_melt%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(Variable=="C_out")%>% group_by(TSI,res,ano) %>% count(TSI) %>% spread(key = "ano", value =  "n")
write.csv2(table, file = "../output/unsteady12/2[Setores] IET SAÍDA.csv")

# Setores - SAÍDA (Inglês) ------------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("6-Ultraoligotrófico"="#BDD8FF",
                             "5-Oligotrófico"="#EEBDFF",
                             "4-Mesotrófico"="#FFCCBB",
                             "3-Eutrófico"="#FFF79F",
                             "2-Supereutrófico"="#B4FF9E",
                             "1-Hipereutrófico"="#67E1B0"), 
                    name="Trophic State Index",
                    labels= c("6-Ultraoligotrófico"="6-Ultraoligotrophic",
                              "5-Oligotrófico"="5-Oligotrophic",
                              "4-Mesotrófico"="4-Mesotrophic",
                              "3-Eutrófico"="3-Eutrophic",
                              "2-Supereutrófico"="2-Supereutrophic",
                              "1-Hipereutrófico"="1-Hipereutrophic"))+
  labs(title="Trophic State Index (Outflow)")
plot
ggsave(filename = "../img/unsteady12/[Setores] - IET Saída - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

# Setores - ENTRADA (Português) CONAMA------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill = classe2))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Supera em mais de 10 x"="#ff8080",
                             "Supera em até 10x"="#FFCCBB",
                             "Supera em até 03x"="#FFF79F",
                             "Em conformidade"="#67E1B0"), 
                    name="Conformidade")+
  labs(title="Conformidade na entrada dos reservatórios")
plot
ggsave(filename = "../img/unsteady12/[Setores] - CONAMA Entrada - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

table<- df_melt%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(Variable=="C_in")%>% group_by(classe2,res,ano) %>% count(classe2) %>% spread(key = "ano", value =  "n")
write.csv2(table, file = "../output/unsteady12/2[Setores] classe 2 ENTRADA.csv")

# Setores - ENTRADA (Inglês) CONAMA--------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill=classe2))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Supera em mais de 10 x"="#ff8080",
                             "Supera em até 10x"="#FFCCBB",
                             "Supera em até 03x"="#FFF79F",
                             "Em conformidade"="#67E1B0"), 
                    name="Exceedance",
                    labels= c("Supera em mais de 10 x"="Exceeds the limit more than 10 times",
                              "Supera em até 10x"="Exceeds the limit until 10 times",
                              "Supera em até 03x"="Exceeds the limit until 03 times",
                              "Em conformidade"="Does not exceed the limit"))+
  labs(title="Exceedance (Inflow)")
plot
ggsave(filename = "../img/unsteady12/[Setores] - CONAMA Entrada - Eng.png", plot= plot, device = "png", width = 35, height = 18, units = "cm")

# Setores - SAÍDA (Português) CONAMA--------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=classe2))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Supera em mais de 10 x"="#ff8080",
                             "Supera em até 10x"="#FFCCBB",
                             "Supera em até 03x"="#FFF79F",
                             "Em conformidade"="#67E1B0"), 
                    name="Conformidade")+
  labs(title="Conformidade na saída dos reservatórios")
plot
ggsave(filename = "../img/unsteady12/[Setores] - CONAMA Saída - Por.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")

table<- df_melt%>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(Variable=="C_out")%>% group_by(classe2,res,ano) %>% count(classe2) %>% spread(key = "ano", value =  "n")
write.csv2(table, file = "../output/unsteady12/2[Setores] classe2 SAÍDA.csv")

# Setores - SAÍDA (Inglês) CONAMA------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=classe2))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(res~ano)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  theme(
    axis.text= element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    panel.grid = element_blank()
  )+
  scale_fill_manual(values=c("Supera em mais de 10 x"="#ff8080",
                             "Supera em até 10x"="#FFCCBB",
                             "Supera em até 03x"="#FFF79F",
                             "Em conformidade"="#67E1B0"), 
                    name="Exceedance",
                    labels= c("Supera em mais de 10 x"="Exceeds the limit more than 10 times",
                              "Supera em até 10x"="Exceeds the limit until 10 times",
                              "Supera em até 03x"="Exceeds the limit until 03 times",
                              "Em conformidade"="Does not exceed the limit"))+
  labs(title="Exceedance (Outflow)")
plot
ggsave(filename = "../img/unsteady12/[Setores] - CONAMA Saída - Eng.png", plot= plot, device = "png", width = 35, height = 18, units = "cm")


# Gráficos Comparação Vitória #############################################

###########################################################################
# Leitura dados vitória ---------------------------------------------------
vit <- read.table("../dataset/PT_vitoria.txt", head=T)
vit$ï..data <- as.Date(vit$ï..data,format="%d-%m-%Y")

#View(vit)
str(vit)

jurP12  <- bind_cols(jurP12,vit=vit$PT_analitica)
chaP12  <- chaP12%>% mutate(vit=NA)
capP12  <- capP12%>% mutate(vit=NA)
jurP25t <- jurP25t%>% mutate(vit=NA)
chaP25t <- chaP25t%>% mutate(vit=NA)
capP25t <- capP25t%>% mutate(vit=NA)
jurP25a <- jurP25a%>% mutate(vit=NA)
chaP25a <- chaP25a%>% mutate(vit=NA)
capP25a <- capP25a%>% mutate(vit=NA)
jurP35t <- jurP35t%>% mutate(vit=NA)
chaP35t <- chaP35t%>% mutate(vit=NA)
capP35t <- capP35t%>% mutate(vit=NA)
jurP35a <- jurP35a%>% mutate(vit=NA)
chaP35a <- chaP35a%>% mutate(vit=NA)
capP35a <- capP35a%>% mutate(vit=NA)


# Juntando os dados em um mesmo data frame --------------------------------

todosP12  <- bind_rows(jurP12,chaP12,capP12)    %>% mutate(ano="B12")
todosP25t <- bind_rows(jurP25t,chaP25t,capP25t) %>% mutate(ano="T25")
todosP25a <- bind_rows(jurP25a,chaP25a,capP25a) %>% mutate(ano="A25")
todosP35t <- bind_rows(jurP35t,chaP35t,capP35t) %>% mutate(ano="T35")
todosP35a <- bind_rows(jurP35a,chaP35a,capP35a) %>% mutate(ano="A35")

todosP <- bind_rows(todosP12,todosP25t,todosP25a,todosP35t,todosP35a)
todosP$res <- factor(todosP$res, levels = c("Jurumirim", "Chavantes","Capivara"))
todosP$ano <- factor(todosP$ano, levels = c("B12","T25", "A25","T35","A35"))
#View(todosP)
#write.csv2(todosP12, file = "../output/dados para comparar com a vitória.csv")
#nrow(todosP)

# Derretendo --------------------------------------------------------------
df_melt<- todosP  %>%   melt(measure.vars = c("vit","C_out2"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res"), 
                             variable.name="Variable",
                             value.name = "Valor")
#View(df_melt)

# IET ---------------------------------------------------------------------

df_melt<-df_melt%>% mutate(TSI=NA)
df_melt$TSI<- sapply(df_melt$Valor, FUN= TSI)


# CONAMA -----------------------------------------------------------------

df_melt<-df_melt%>% mutate(classe2=NA)
df_melt$classe2<- sapply(df_melt$Valor,classe2)
#View(df_melt)
#write.csv2(df_melt,"../output/df_melt.csv")



# Gráfico 2012 (português) log---------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(ano=="B12",res=="Jurumirim")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  #facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Vitória", "Ana Becker"),values=c("blue","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.004,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=3.038, 
                fill="1-Hipereutrófico"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrófico"="#BDD8FF",
                              "5-Oligotrófico"="#EEBDFF",
                              "4-Mesotrófico"="#FFCCBB",
                              "3-Eutrófico"="#FFF79F",
                              "2-Supereutrófico"="#B4FF9E",
                              "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
dir.create("../img/ANAxVIT")
ggsave(filename = "../img/unsteady12/[TimeSeries] - ANAxVIT1 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico 2012 (inglês) log------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12",res=="Jurumirim")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  #facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Vitória", "Ana Becker"),values=c("blue","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.004,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=3.038, 
                fill="1-Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrophic"="#BDD8FF",
                              "5-Oligotrophic"="#EEBDFF",
                              "4-Mesotrophic"="#FFCCBB",
                              "3-Eutrophic"="#FFF79F",
                              "2-Supereutrophic"="#B4FF9E",
                              "1-Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - ANAxVIT1 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")

# Gráfico 2012 (português) ------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(ano=="B12",res=="Jurumirim")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #facet_grid(res~ano)+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Vitória", "Ana Becker"),values=c("blue","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.00,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrófico"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1, 
                fill="1-Hipereutrófico"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrófico"="#BDD8FF",
                              "5-Oligotrófico"="#EEBDFF",
                              "4-Mesotrófico"="#FFCCBB",
                              "3-Eutrófico"="#FFF79F",
                              "2-Supereutrófico"="#B4FF9E",
                              "1-Hipereutrófico"="#67E1B0"), 
                    name="Índice de Estado Trófico")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - ANAxVIT2 - Por.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")


# Gráfico 2012 (inglês) ---------------------------------------------------
Sys.setlocale("LC_TIME", "English")
plot <- df_melt%>%
  filter(ano=="B12",res=="Jurumirim")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="1 month",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #facet_grid(res~ano)+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Vitória", "Ana Becker"),values=c("blue","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.00,
                xmax=as.Date(c("2012-12-31")),ymax=0.012, 
                fill="6-Ultraoligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.012,
                xmax=as.Date(c("2012-12-31")),ymax=0.032, 
                fill="5-Oligotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.032,
                xmax=as.Date(c("2012-12-31")),ymax=0.125, 
                fill="4-Mesotrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.125,
                xmax=as.Date(c("2012-12-31")),ymax=0.269, 
                fill="3-Eutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.269,
                xmax=as.Date(c("2012-12-31")),ymax=0.581, 
                fill="2-Supereutrophic"), alpha=0.3)+
  geom_rect(aes(xmin=as.Date(c("2012-01-01")),ymin=0.581,
                xmax=as.Date(c("2012-12-31")),ymax=1, 
                fill="1-Hipereutrophic"), alpha=0.3)+
  scale_fill_manual(values= c("6-Ultraoligotrophic"="#BDD8FF",
                              "5-Oligotrophic"="#EEBDFF",
                              "4-Mesotrophic"="#FFCCBB",
                              "3-Eutrophic"="#FFF79F",
                              "2-Supereutrophic"="#B4FF9E",
                              "1-Hipereutrophic"="#67E1B0"), 
                    name="Trophic State Index")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - ANAxVIT2 - Eng.png", plot= plot, device = "png", width = 25, height = 18, units = "cm")



# Comparação k e v ########################################################

###########################################################################



df<-bind_rows(read.csv2("../output/unsteady13/TAB2_k=0_v=0.01.csv")        %>% mutate (k="k=0   ",v="v=0.01")   %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.01.csv")    %>% mutate (k="k=0.001",v="v=0.01")  %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.01.csv")    %>% mutate (k="k=0.005",v="v=0.01")  %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.01.csv")     %>% mutate (k="k=0.01",v="v=0.01")   %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0_v=0.001.csv")       %>% mutate (k="k=0   ",v="v=0.001")  %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.001.csv")   %>% mutate (k="k=0.001",v="v=0.001") %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.001.csv")   %>% mutate (k="k=0.005",v="v=0.001") %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.001.csv")    %>% mutate (k="k=0.01",v="v=0.001")  %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0_v=0.csv")           %>% mutate (k="k=0   ",v="v=0")      %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.csv")       %>% mutate (k="k=0.001",v="v=0")     %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.csv")       %>% mutate (k="k=0.005",v="v=0")     %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.csv")        %>% mutate (k="k=0.01",v="v=0")      %>% filter(X>=366),
              read.csv2("../output/unsteady13/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0.01"),
              read.csv2("../output/unsteady13/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0.001"),
              read.csv2("../output/unsteady13/TAB3_obs.csv")               %>% mutate (k="OBS   ",v="v=0")      
              )

plot<- df%>%
       ggplot()+
       geom_boxplot(aes(x=k,y=Jurumirim))+
       facet_grid(v~.)+
       theme_bw()+
       geom_hline(yintercept = median(jur_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
       labs(x = "", y = "Phosphorus (mg/L)", title = "Jurumirim")
ggsave(filename = "../img/unsteady12/2[Boxplot] - k_v_Jurumirim.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")
#theme(axis.text.x = element_blank(),axis.ticks.x.bottom = element_blank())

plot<- df%>%
  ggplot()+
  geom_boxplot(aes(x=k,y=Jurumirim))+
  facet_grid(v~.)+
  theme_bw()+
  geom_hline(yintercept = median(jur_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = "Phosphorus (mg/L)", title = "Jurumirim - ZOOM")+
  scale_y_continuous(limits=c(0,0.1))
ggsave(filename = "../img/unsteady12/2[Boxplot] - k_v_Jurumirim_zoom.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")
#theme(axis.text.x = element_blank(),axis.ticks.x.bottom = element_blank())

plot<- df%>%
  ggplot()+
  geom_boxplot(aes(x=k, y=Chavantes))+
  facet_grid(v~.)+
  theme_bw()+
  geom_hline(yintercept = median(cha_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = "Phosphorus (mg/L)", title = "Chavantes")
ggsave(filename = "../img/unsteady12/2[Boxplot] - k_v_Chavantes.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")

plot<- df%>%
  ggplot()+
  geom_boxplot(aes(x=k, y=Chavantes))+
  facet_grid(v~.)+
  theme_bw()+
  geom_hline(yintercept = median(cha_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = "Phosphorus (mg/L)", title = "Chavantes - ZOOM")+
  scale_y_continuous(limits=c(0,0.1))
ggsave(filename = "../img/unsteady12/2[Boxplot] - k_v_Chavantes_zoom.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")


plot<- df%>%
  ggplot()+
  geom_boxplot(aes(x=k,y=Capivara))+
  facet_grid(v~.)+
  theme_bw()+
  geom_hline(yintercept = median(cap_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = "Phosphorus (mg/L)", title = "Capivara")
ggsave(filename = "../img/unsteady12/2[Boxplot] - k_v_Capivara.png", plot= plot, device = "png", width = 18, height = 18, units = "cm")


df2<-bind_rows(read.csv2("../output/unsteady13/TAB2_k=0_v=0.01.csv")    %>% mutate (k="k=0   ",v="v=0.01"),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.01.csv")    %>% mutate (k="k=0.001",v="v=0.01"),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.01.csv")    %>% mutate (k="k=0.005",v="v=0.01"),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.01.csv") %>% mutate (k="k=0.01",v="v=0.01"),
              read.csv2("../output/unsteady13/TAB2_k=0_v=0.001.csv")    %>% mutate (k="k=0   ",v="v=0.001"),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.001.csv") %>% mutate (k="k=0.001",v="v=0.001"),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.001.csv") %>% mutate (k="k=0.005",v="v=0.001"),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.001.csv") %>% mutate (k="k=0.01",v="v=0.001"),
              read.csv2("../output/unsteady13/TAB2_k=0_v=0.csv")    %>% mutate (k="k=0   ",v="v=0"),
              read.csv2("../output/unsteady13/TAB2_k=0.001_v=0.csv") %>% mutate (k="k=0.001",v="v=0"),
              read.csv2("../output/unsteady13/TAB2_k=0.005_v=0.csv") %>% mutate (k="k=0.005",v="v=0"),
              read.csv2("../output/unsteady13/TAB2_k=0.01_v=0.csv") %>% mutate (k="k=0.01",v="v=0")
)


plot<- df2%>%
  gather(key = "res",value = "valor", c(2:4))%>%
  filter(k != "OBS")%>%
  filter(res=="Jurumirim")%>%
  ggplot()+
  geom_point(aes(x=X,y=valor), size=1, color="#4db8ff")+
  facet_grid(v~k)+
  theme_bw()+
  labs(x = "Days of simulation", y = "Phosphorus (mg/L)")+
  geom_vline(xintercept = 366, color="gray", linetype="dashed")+
  scale_color_discrete(name= " ")
plot
ggsave(filename = "../img/unsteady12/[TimeSeries] - k_v_lines_juru.png", plot= plot, device = "png", width = 15, height = 11.5, units = "cm")
#theme(axis.text.x = element_blank(),axis.ticks.x.bottom = element_blank())

df55<-bind_rows(read.csv2("../output/unsteady13/TAB3_obs.csv"))%>% gather(key = "res",value = "C", c(2:4))  
View(df55)
df55$res <- factor(df55$res, levels = c("Jurumirim", "Chavantes","Capivara"))
plot<- df55%>%
  ggplot()+
  geom_boxplot(aes(x=res,y=C,fill=res))+
  theme_bw()+
  #geom_hline(yintercept = median(jur_res$Valor), color="#2ECCFA", linetype="solid", size=0.7,alpha=1)+
  labs(x = "", y = "Phosphorus (mg/L)", title = "Measured data")
plot
ggsave(filename = "../img/unsteady12/[Boxplot] - Measured data.png", plot= plot, device = "png", width = 15, height = 10, units = "cm")


# TMDL --------------------------------------------------------------------

test<- jur %>% select(Data,Q_Deflu) %>% mutate(classe1=0.02*Q_Deflu/1000*86400, classe2=0.03*Q_Deflu/1000*86400, classe3=0.04*Q_Deflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_jur_means_deflu.csv")
write.csv2(sums, file = "../output/tmdl_jur_sums_deflu.csv")

test<- jur %>% select(Data,Q_Aflu) %>% mutate(classe1=0.02*Q_Aflu/1000*86400, classe2=0.03*Q_Aflu/1000*86400, classe3=0.04*Q_Aflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_jur_means_aflu.csv")
write.csv2(sums, file = "../output/tmdl_jur_sums_aflu.csv")

test<- cha %>% select(Data,Q_Deflu) %>% mutate(classe1=0.02*Q_Deflu/1000*86400, classe2=0.03*Q_Deflu/1000*86400, classe3=0.04*Q_Deflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_cha_means_deflu.csv")
write.csv2(sums, file = "../output/tmdl_cha_sums_deflu.csv")

test<- cha %>% select(Data,Q_Aflu) %>% mutate(classe1=0.02*Q_Aflu/1000*86400, classe2=0.03*Q_Aflu/1000*86400, classe3=0.04*Q_Aflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_cha_means_aflu.csv")
write.csv2(sums, file = "../output/tmdl_cha_sums_aflu.csv")

test<- cap %>% select(Data,Q_Deflu) %>% mutate(classe1=0.02*Q_Deflu/1000*86400, classe2=0.03*Q_Deflu/1000*86400, classe3=0.04*Q_Deflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_cap_means_deflu.csv")
write.csv2(sums, file = "../output/tmdl_cap_sums_deflu.csv")

test<- jur %>% select(Data,Q_Aflu) %>% mutate(classe1=0.02*Q_Aflu/1000*86400, classe2=0.03*Q_Aflu/1000*86400, classe3=0.04*Q_Aflu/1000*86400)
means<- test %>% summarize(classe1=mean(classe1),classe2=mean(classe2),classe3=mean(classe3))
sums<- test %>% summarize(classe1=sum(classe1),classe2=sum(classe2),classe3=sum(classe3))
write.csv2(means, file = "../output/tmdl_cap_means_aflu.csv")
write.csv2(sums, file = "../output/tmdl_cap_sums_aflu.csv")
