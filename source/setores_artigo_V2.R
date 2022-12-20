library(tidyverse)
library(reshape2)
library(ggpubr)
library(scales)
library(lubridate)


# Leitura dados de cotas -----------------------------------------

jur <- read.table("dataset/Dados_Jurumirim/vazao_SAR_JURUMIRIM.txt", head=T)
jur$Data <- as.Date(jur$Data,format="%Y-%m-%d" )
jur <- jur %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))
names(jur)[7] <- "Vol"


# Leitura dados de fósforo ------------------------------------------------

jurP <- read.csv("dataset/Setores/concentracao_por_setor.csv", head=T, sep=";", dec=",")
jurP$Data <- as.Date(jurP$Data,format="%d/%m/%Y" )
jurP <- jurP %>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))


# Cargas Laterais, k e set_v ----------------------------------------------
jur1P <- jurP %>% filter(Setor=="Setor 1")
jur1P12  <- bind_cols(jur[,c(1,2,4)],jur1P[,c(2,3)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "B12","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0839895791651107, Setor = "Setor 1",Vol= 1000000*(-0.000720064221654*Cota^4 + 1.595740162907880*Cota^3 - 1322.173470830580000*Cota^2 + 485546.032222308000000*Cota- 66693262.439377400000000),  As = 2*1000000*(0.000085668766059*Cota^4 - 0.196169098968667*Cota^3 + 168.262977098037000*Cota^2 - 64069.715620183800000*Cota + 9137278.075065730000000))
jur1P25t <- bind_cols(jur[,c(1,2,4)],jur1P[,c(2,4)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0839895791651107, Setor = "Setor 1",Vol= 1000000*(-0.000720064221654*Cota^4 + 1.595740162907880*Cota^3 - 1322.173470830580000*Cota^2 + 485546.032222308000000*Cota- 66693262.439377400000000),  As = 2*1000000*(0.000085668766059*Cota^4 - 0.196169098968667*Cota^3 + 168.262977098037000*Cota^2 - 64069.715620183800000*Cota + 9137278.075065730000000))
jur1P25a <- bind_cols(jur[,c(1,2,4)],jur1P[,c(2,5)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0839895791651107, Setor = "Setor 1",Vol= 1000000*(-0.000720064221654*Cota^4 + 1.595740162907880*Cota^3 - 1322.173470830580000*Cota^2 + 485546.032222308000000*Cota- 66693262.439377400000000),  As = 2*1000000*(0.000085668766059*Cota^4 - 0.196169098968667*Cota^3 + 168.262977098037000*Cota^2 - 64069.715620183800000*Cota + 9137278.075065730000000))
jur1P35t <- bind_cols(jur[,c(1,2,4)],jur1P[,c(2,6)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0839895791651107, Setor = "Setor 1",Vol= 1000000*(-0.000720064221654*Cota^4 + 1.595740162907880*Cota^3 - 1322.173470830580000*Cota^2 + 485546.032222308000000*Cota- 66693262.439377400000000),  As = 2*1000000*(0.000085668766059*Cota^4 - 0.196169098968667*Cota^3 + 168.262977098037000*Cota^2 - 64069.715620183800000*Cota + 9137278.075065730000000))
jur1P35a <- bind_cols(jur[,c(1,2,4)],jur1P[,c(2,7)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0839895791651107, Setor = "Setor 1",Vol= 1000000*(-0.000720064221654*Cota^4 + 1.595740162907880*Cota^3 - 1322.173470830580000*Cota^2 + 485546.032222308000000*Cota- 66693262.439377400000000),  As = 2*1000000*(0.000085668766059*Cota^4 - 0.196169098968667*Cota^3 + 168.262977098037000*Cota^2 - 64069.715620183800000*Cota + 9137278.075065730000000))

jur2P <- jurP %>% filter(Setor=="Setor 2")
jur2P12  <- bind_cols(jur[,c(1,2,4)],jur2P[,c(2,3)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "B12","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.252937160096222, Setor = "Setor 2",Vol= 1000000*(-0.000544606243188*Cota^4 + 1.230324218157030*Cota^3 - 1041.038832151660000*Cota^2 + 391066.040022819000000*Cota- 55032357.354356100000000),  As = 2*1000000*(-0.000028542707748*Cota^4 + 0.062840185619947*Cota^3 - 51.832622767916000*Cota^2 + 18984.851202710700000*Cota - 2605492.209165040000000))
jur2P25t <- bind_cols(jur[,c(1,2,4)],jur2P[,c(2,4)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.252937160096222, Setor = "Setor 2",Vol= 1000000*(-0.000544606243188*Cota^4 + 1.230324218157030*Cota^3 - 1041.038832151660000*Cota^2 + 391066.040022819000000*Cota- 55032357.354356100000000),  As = 2*1000000*(-0.000028542707748*Cota^4 + 0.062840185619947*Cota^3 - 51.832622767916000*Cota^2 + 18984.851202710700000*Cota - 2605492.209165040000000))
jur2P25a <- bind_cols(jur[,c(1,2,4)],jur2P[,c(2,5)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.252937160096222, Setor = "Setor 2",Vol= 1000000*(-0.000544606243188*Cota^4 + 1.230324218157030*Cota^3 - 1041.038832151660000*Cota^2 + 391066.040022819000000*Cota- 55032357.354356100000000),  As = 2*1000000*(-0.000028542707748*Cota^4 + 0.062840185619947*Cota^3 - 51.832622767916000*Cota^2 + 18984.851202710700000*Cota - 2605492.209165040000000))
jur2P35t <- bind_cols(jur[,c(1,2,4)],jur2P[,c(2,6)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.252937160096222, Setor = "Setor 2",Vol= 1000000*(-0.000544606243188*Cota^4 + 1.230324218157030*Cota^3 - 1041.038832151660000*Cota^2 + 391066.040022819000000*Cota- 55032357.354356100000000),  As = 2*1000000*(-0.000028542707748*Cota^4 + 0.062840185619947*Cota^3 - 51.832622767916000*Cota^2 + 18984.851202710700000*Cota - 2605492.209165040000000))
jur2P35a <- bind_cols(jur[,c(1,2,4)],jur2P[,c(2,7)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.252937160096222, Setor = "Setor 2",Vol= 1000000*(-0.000544606243188*Cota^4 + 1.230324218157030*Cota^3 - 1041.038832151660000*Cota^2 + 391066.040022819000000*Cota- 55032357.354356100000000),  As = 2*1000000*(-0.000028542707748*Cota^4 + 0.062840185619947*Cota^3 - 51.832622767916000*Cota^2 + 18984.851202710700000*Cota - 2605492.209165040000000))

jur3P <- jurP %>% filter(Setor=="Setor 3")
jur3P12  <- bind_cols(jur[,c(1,2,4)],jur3P[,c(2,3)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "B12","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0226115293112673, Setor = "Setor 3",Vol= 1000000*(-0.000260132010078*Cota^4 + 0.589715146929719*Cota^3 - 500.934206652143000*Cota^2 + 188979.601745476000000*Cota - 26716335.134011700000000),  As = 2*1000000*(-0.000035060061477*Cota^4 + 0.078335645303437*Cota^3 - 65.611450422384900*Cota^2 + 24415.633729096300000*Cota - 3406012.723440820000000))
jur3P25t <- bind_cols(jur[,c(1,2,4)],jur3P[,c(2,4)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0226115293112673, Setor = "Setor 3",Vol= 1000000*(-0.000260132010078*Cota^4 + 0.589715146929719*Cota^3 - 500.934206652143000*Cota^2 + 188979.601745476000000*Cota - 26716335.134011700000000),  As = 2*1000000*(-0.000035060061477*Cota^4 + 0.078335645303437*Cota^3 - 65.611450422384900*Cota^2 + 24415.633729096300000*Cota - 3406012.723440820000000))
jur3P25a <- bind_cols(jur[,c(1,2,4)],jur3P[,c(2,5)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0226115293112673, Setor = "Setor 3",Vol= 1000000*(-0.000260132010078*Cota^4 + 0.589715146929719*Cota^3 - 500.934206652143000*Cota^2 + 188979.601745476000000*Cota - 26716335.134011700000000),  As = 2*1000000*(-0.000035060061477*Cota^4 + 0.078335645303437*Cota^3 - 65.611450422384900*Cota^2 + 24415.633729096300000*Cota - 3406012.723440820000000))
jur3P35t <- bind_cols(jur[,c(1,2,4)],jur3P[,c(2,6)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0226115293112673, Setor = "Setor 3",Vol= 1000000*(-0.000260132010078*Cota^4 + 0.589715146929719*Cota^3 - 500.934206652143000*Cota^2 + 188979.601745476000000*Cota - 26716335.134011700000000),  As = 2*1000000*(-0.000035060061477*Cota^4 + 0.078335645303437*Cota^3 - 65.611450422384900*Cota^2 + 24415.633729096300000*Cota - 3406012.723440820000000))
jur3P35a <- bind_cols(jur[,c(1,2,4)],jur3P[,c(2,7)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.0226115293112673, Setor = "Setor 3",Vol= 1000000*(-0.000260132010078*Cota^4 + 0.589715146929719*Cota^3 - 500.934206652143000*Cota^2 + 188979.601745476000000*Cota - 26716335.134011700000000),  As = 2*1000000*(-0.000035060061477*Cota^4 + 0.078335645303437*Cota^3 - 65.611450422384900*Cota^2 + 24415.633729096300000*Cota - 3406012.723440820000000))

jur4P <- jurP %>% filter(Setor=="Setor 4")
jur4P12  <- bind_cols(jur[,c(1,2,4)],jur4P[,c(2,3)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "B12","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.6404617314274, Setor = "Setor 4",Vol= 1000000*(-0.001179672493933*Cota^4 + 2.678578662380230*Cota^3 - 2278.480533305400000*Cota^2 + 860589.991537997000000*Cota- 121785247.137767000000000),  As = 2*1000000*(0.000097824573642*Cota^4 - 0.227462601181334*Cota^3  + 198.190956637696000*Cota^2 - 76691.214884574900000*Cota + 11119933.621090200000000))
jur4P25t <- bind_cols(jur[,c(1,2,4)],jur4P[,c(2,4)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.6404617314274, Setor = "Setor 4",Vol= 1000000*(-0.001179672493933*Cota^4 + 2.678578662380230*Cota^3 - 2278.480533305400000*Cota^2 + 860589.991537997000000*Cota- 121785247.137767000000000),  As = 2*1000000*(0.000097824573642*Cota^4 - 0.227462601181334*Cota^3  + 198.190956637696000*Cota^2 - 76691.214884574900000*Cota + 11119933.621090200000000))
jur4P25a <- bind_cols(jur[,c(1,2,4)],jur4P[,c(2,5)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A25","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.6404617314274, Setor = "Setor 4",Vol= 1000000*(-0.001179672493933*Cota^4 + 2.678578662380230*Cota^3 - 2278.480533305400000*Cota^2 + 860589.991537997000000*Cota- 121785247.137767000000000),  As = 2*1000000*(0.000097824573642*Cota^4 - 0.227462601181334*Cota^3  + 198.190956637696000*Cota^2 - 76691.214884574900000*Cota + 11119933.621090200000000))
jur4P35t <- bind_cols(jur[,c(1,2,4)],jur4P[,c(2,6)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "T35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.6404617314274, Setor = "Setor 4",Vol= 1000000*(-0.001179672493933*Cota^4 + 2.678578662380230*Cota^3 - 2278.480533305400000*Cota^2 + 860589.991537997000000*Cota- 121785247.137767000000000),  As = 2*1000000*(0.000097824573642*Cota^4 - 0.227462601181334*Cota^3  + 198.190956637696000*Cota^2 - 76691.214884574900000*Cota + 11119933.621090200000000))
jur4P35a <- bind_cols(jur[,c(1,2,4)],jur4P[,c(2,7)])%>% filter(Data>= as.Date("2011-01-01") & Data <= as.Date("2012-12-31"))%>% rename(PT = "A35","Q_Aflu"="Vazão") %>% mutate (res = "Jurumirim", W =  0E3/24/60/60, k=  0.005/86400, set_v = 0.01/86400, Q_Deflu=Q_Deflu*0.6404617314274, Setor = "Setor 4",Vol= 1000000*(-0.001179672493933*Cota^4 + 2.678578662380230*Cota^3 - 2278.480533305400000*Cota^2 + 860589.991537997000000*Cota- 121785247.137767000000000),  As = 2*1000000*(0.000097824573642*Cota^4 - 0.227462601181334*Cota^3  + 198.190956637696000*Cota^2 - 76691.214884574900000*Cota + 11119933.621090200000000))

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


#jur1P12  <- model(jur1P12)
#jur1P25t <- model(jur1P25t)
#jur1P25a <- model(jur1P25a)
#jur1P35t <- model(jur1P35t)
#jur1P35a <- model(jur1P35a)

jur2P12  <- model(jur2P12)
jur2P25t <- model(jur2P25t)
jur2P25a <- model(jur2P25a)
jur2P35t <- model(jur2P35t)
jur2P35a <- model(jur2P35a)

jur3P12  <- model(jur3P12)
jur3P25t <- model(jur3P25t)
jur3P25a <- model(jur3P25a)
jur3P35t <- model(jur3P35t)
jur3P35a <- model(jur3P35a)

jur4P12  <- model(jur4P12)
jur4P25t <- model(jur4P25t)
jur4P25a <- model(jur4P25a)
jur4P35t <- model(jur4P35t)
jur4P35a <- model(jur4P35a)


modelS1 <- function(df, df2, df3, df4){
  df<- df %>% mutate(i=1:nrow(df), t=86400*i, C_in= df$PT, C_out= NA)
  df[1,"C_out"]= 0
  for (i in 2:nrow(df)){
    df[i, "C_out"] = (df[i-1,"C_out"]+(df[i, "t"]-df[i-1, "t"])/df[i, "Vol"] *(df[i, "Q_Aflu"]*df[i, "C_in"]+df2[i, "Q_Deflu"]*df2[i,"C_out"]+df3[i, "Q_Deflu"]*df3[i,"C_out"]+df4[i, "Q_Deflu"]*df4[i,"C_out"]))/(1+(df[i, "t"]-df[i-1, "t"])/df[i,"Vol"]*(df[i,"Q_Deflu"]+df[i,"k"]*df[i,"Vol"]+ df[i,"set_v"] *df[i,"As"]+(df[i,"Vol"]-df[i-1,"Vol"])/(df[i,"t"]-df[i-1,"t"])))
    if ( df[i, "C_out"] < 0){
      df[i, "C_out"] = 0 
    }   
  }
  print(df)
}

jur1P12  <- modelS1(jur1P12 ,jur2P12 ,jur3P12 ,jur4P12 )
jur1P25t <- modelS1(jur1P25t,jur2P25t,jur3P25t,jur4P25t)
jur1P25a <- modelS1(jur1P25a,jur2P25a,jur3P25a,jur4P25a)
jur1P35t <- modelS1(jur1P35t,jur2P35t,jur3P35t,jur4P35t)
jur1P35a <- modelS1(jur1P35a,jur2P35a,jur3P35a,jur4P35a)
  
# Juntando os dados em um mesmo data frame --------------------------------

todosP12  <- bind_rows(jur1P12 , jur2P12 , jur3P12 , jur4P12 ) %>% mutate(ano="B12")
todosP25t <- bind_rows(jur1P25t, jur2P25t, jur3P25t, jur4P25t) %>% mutate(ano="T25")
todosP25a <- bind_rows(jur1P25a, jur2P25a, jur3P25a, jur4P25a) %>% mutate(ano="A25")
todosP35t <- bind_rows(jur1P35t, jur2P35t, jur3P35t, jur4P35t) %>% mutate(ano="T35")
todosP35a <- bind_rows(jur1P35a, jur2P35a, jur3P35a, jur4P35a) %>% mutate(ano="A35")
                 
todosP <- bind_rows(todosP12,todosP25t,todosP25a,todosP35t,todosP35a)
todosP$ano <- factor(todosP$ano, levels = c("B12","T25", "A25","T35","A35"))


#write.csv2(todosP, file = "../output/resultado_setores_jurumirim_com_soma.csv")
#
## Leitura Dados Observados APENAS PT --------------------------------------
#
#obs<- read.csv2("../dataset/DadosConsolidados.csv")
#obs<-obs %>% gather(key="Var",value = "Valor", c(16:62), na.rm=T)
#obs$Valor <- as.numeric(obs$Valor)
#obs <- obs %>%
#  group_by(Data, km, Estacao, Var, Reservatorio, Local) %>%
#  summarize(Valor = mean(Valor, na.rm = TRUE))
#obs$Data<- as.Date(obs$Data,format="%d/%m/%Y")
#obs_P <- obs %>%
#  filter(Var=="PT")%>%
#  filter(Valor < 10)
#View(obs_P)
#
#
## Cálculo de médias -------------------------------------------------------
#jur_s3 <- obs_P %>% filter(Estacao=="JU-07-S")
#jur_s3_jus <- obs_P %>% filter(Estacao=="JU-02-CP"|Estacao=="64214000"|Estacao=="JURU 02500")
#nrow(jur_s3)
#nrow(jur_s3_jus)
#mean(jur_s3$Valor)
#mean(jur_s3_jus$Valor)
#mean(jurP12$C_out)

# Derretendo --------------------------------------------------------------
df_melt<- todosP  %>%   melt(measure.vars = c("C_in","C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
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

# Gráfico (português) -----------------------------------------------------
Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="3 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(Setor~ano)+
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
                xmax=as.Date(c("2012-12-31")),ymax=9, 
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
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - com adição - 2012 - Por.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")

# Gráfico (Inglês) -------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
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

plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="3 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(Setor~ano,labeller = labeller(Setor = variable_names, ano = variable_names2))+
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
                xmax=as.Date(c("2012-12-31")),ymax=9, 
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
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - com adição - 2012 - Eng.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")

# Gráfico (Inglês) -------------------------------------------------------
Sys.setlocale("LC_TIME", "English")
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

plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="3 months",expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  facet_grid(Setor~ano,labeller = labeller(Setor = variable_names, ano = variable_names2))+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Input", "Output"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="right")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.03), color="black",linetype="dashed")
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")
  

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - CONAMA2 - 2012 - Eng.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")

# Setores - ENTRADA (Inglês) ---------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_in")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(Setor~ano)+
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
ggsave(filename = "../img/setores_artigo/[Setores] - IET Entrada - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")
table<- df_melt%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%filter(Variable=="C_in")%>% group_by(TSI,Setor,ano) %>% count(TSI) %>% spread(key = "ano", value =  "n")
#write.csv2(table, file = "../output/setores/[Setores] IET ENTRADA.csv")


# Setores - SAÍDA (Inglês) ------------------------------------------
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(Variable=="C_out")%>%
  ggplot(aes(x=1, fill=TSI))+
  geom_bar()+
  coord_polar("y")+
  facet_grid(Setor~ano)+
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
ggsave(filename = "../img/setores_artigo/[Setores] - IET Saída - Eng.png", plot= plot, device = "png", width = 30, height = 18, units = "cm")
table<- df_melt%>%  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(Variable=="C_out")%>% group_by(TSI,Setor,ano) %>% count(TSI) %>% spread(key = "ano", value =  "n")
#write.csv2(table, file = "../output/setores/[Setores] IET SAÍDA.csv")

# Vazões (português) -----------------------------------------------------
df_melt4<- todosP  %>%   melt(measure.vars = c("Q_Aflu","Q_Deflu"),
                             id.vars = c("Data","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")

Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt4 %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(Setor~.)+
  labs(title = " ", x="", y = "Vazão (m³/s)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Entrada", "Saída"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - vazões - Por.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")

# Vazões (inglês) --------------------------------------------------------
df_melt4<- todosP  %>%   melt(measure.vars = c("Q_Aflu","Q_Deflu"),
                              id.vars = c("Data","ano","res","Setor"), 
                              variable.name="Variable",
                              value.name = "Valor")
variable_names <- c(
  "Setor 1" = "Sector 1" ,
  "Setor 2" = "Sector 2" ,
  "Setor 3" = "Sector 3" ,
  "Setor 4" = "Sector 4" 
)

Sys.setlocale("LC_TIME", "English")
plot <- df_melt4 %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(Setor~.,labeller = labeller(Setor = variable_names))+
  labs(title = " ", x="", y = "Flow (m³/s)")+
  theme_bw()+
  scale_color_manual(name= "", labels = c("Inflow", "Outflow"),values=c("#FF7664","#000000"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Variable), size=0.3)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - vazões - Eng.png", plot= plot, device = "png", width = 22, height = 13, units = "cm")


# Volumes (português) -----------------------------------------------------
df_melt5<- todosP  %>%   mutate(As=As/2)%>%
  melt(measure.vars = c("Vol","As"),
                              id.vars = c("Data","ano","res","Setor"), 
                              variable.name="Variable",
                              value.name = "Valor")
variable_names <- list(
  "Vol" = "Volume (hm³)" ,
  "As" = "Área (km²)"
)
variable_labeller <- function(variable,value){
  return(variable_names[value])
}

Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt5 %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  facet_grid(Variable~., scales="free",labeller=variable_labeller)+
  labs(title = " ", x="", y = "")+
  scale_color_manual(name= "", labels = c("Setor 1", "Setor 2", "Setor 3", "Setor 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor/1000000, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - volumes - Por.png", plot= plot, device = "png", width = 15, height = 18, units = "cm")

# Volumes (inglês) --------------------------------------------------------
df_melt5<- todosP  %>%   mutate(As=As/2)%>%
  melt(measure.vars = c("Vol","As"),
       id.vars = c("Data","ano","res","Setor"), 
       variable.name="Variable",
       value.name = "Valor")
variable_names <- list(
  "Vol" = "Volume (hm³)" ,
  "As" = "Area (km²)"
)

Sys.setlocale("LC_TIME", "English")
plot <- df_melt5 %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  facet_grid(Variable~., scales="free",labeller=variable_labeller)+
  labs(title = " ", x="", y = "")+
  scale_color_manual(name= "", labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor/1000000, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - volumes - Eng.png", plot= plot, device = "png", width = 15, height = 18, units = "cm")


# Comparação (portugues) -------------------------------------------------

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")

Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  scale_color_manual(name= "", labels = c("Setor 1", "Setor 2", "Setor 3", "Setor 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_setores - Por.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

# Comparação (ingles) ----------------------------------------------------

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")

Sys.setlocale("LC_TIME", "English")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name= "", labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_setores - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# Comparação - cargas - (portugues) -------------------------------------------------

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor!="Setor 1"| ano!="B12")
df_melt<- rbind(df_melt2,df_melt3)

Sys.setlocale("LC_TIME", "Portuguese")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = "Fósforo (ton/dia)")+
  scale_color_manual(name= "", labels = c("Setor 1", "Setor 2", "Setor 3", "Setor 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor*Q_Deflu/1000*86400, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_setores_cargas - Por.png", plot= plot, device = "png",width = 16, height = 14, units = "cm")

# Comparação - cargas - (ingles) ----------------------------------------------------

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor!="Setor 1"| ano!="B12")
df_melt<- rbind(df_melt2,df_melt3)

Sys.setlocale("LC_TIME", "English")
plot <- df_melt %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  scale_color_manual(name= "", labels = c("Sector 1", "Sector 2", "Sector 3", "Sector 4"),values=c("#66ff99","#ff33cc","#ffcc00","#0099ff"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=Valor*Q_Deflu/1000*86400, color=Setor), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_setores_cargas - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# Comparação com reator único - concentração ------------------------------

ru<- read.csv2("../output/resultado_unsteady10.csv") %>% filter(res=="Jurumirim")
ru$Data <- as.Date(ru$Data,format="%d/%m/%Y" )
ru<- ru %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
ru2<- ru[,c(2,23,24)]%>% mutate(Q_Deflu=ru$Q_Deflu,Font="Model A")



#View(ru2)

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor=="Setor 1"& ano=="T25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt4<- filter(df_melt,Setor=="Setor 1"& ano=="T35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt5<- filter(df_melt,Setor=="Setor 1"& ano=="A25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt6<- filter(df_melt,Setor=="Setor 1"& ano=="A35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt7<- filter(df_melt,Setor!="Setor 1")
df_melt<- rbind(df_melt2,df_melt3,df_melt4,df_melt5,df_melt6,df_melt7)
df_melt<- df_melt %>% filter(Setor=="Setor 1")%>% mutate(C_out=Valor)%>% select(Data, C_out, ano,Q_Deflu)%>% mutate(Font="Model B")


comp<-rbind(ru2,df_melt)

comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))


x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loads.csv")


# Comparação com reator único e delft - concentração --------------------------------------------------
delft<- read.csv2("../dataset/setores/Setor1_B12_Delft.csv") 
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
delft<- delft %>% mutate(ano="B12", Q_Deflu=jur$Q_Deflu[c(366:731)], Font="Delft3D")

comp<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(ano=="B12")
comp<-rbind(comp,delft)
View(comp)
comp$Font <- factor(comp$Font, levels = c("Model A","Model B", "Delft3D"))

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_serie.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")
x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12_serie.csv")

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=Font), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.11,0.01),limits=c(0,0.11),expand = c(0,0),labels = seq(0.0,0.11,0.01))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.05), color="black",linetype="dashed")

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft2012 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# Comparação com reator único e delft - cargas ------------------------------------

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(ano=="B12")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous()+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=C_out*Q_Deflu/1000*86400, color=Font), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_capitulos_carga - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacaoB12.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsB12.csv")


# A35 Comparação com reator único e delft - concentração --------------------------------------------------
ru<- read.csv2("../output/resultado_unsteady10.csv") %>% filter(res=="Jurumirim")
ru$Data <- as.Date(ru$Data,format="%d/%m/%Y" )
ru<- ru %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))
ru2<- ru[,c(2,23,24)]%>% mutate(Q_Deflu=ru$Q_Deflu,Font="Model A")



#View(ru2)

df_melt<- todosP  %>%   melt(measure.vars = c("C_out"),
                             id.vars = c("Data","Q_Aflu","Q_Deflu","ano","res","Setor"), 
                             variable.name="Variable",
                             value.name = "Valor")


df_melt2<- filter(df_melt,Setor=="Setor 1"& ano=="B12")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt3<- filter(df_melt,Setor=="Setor 1"& ano=="T25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt4<- filter(df_melt,Setor=="Setor 1"& ano=="T35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt5<- filter(df_melt,Setor=="Setor 1"& ano=="A25")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt6<- filter(df_melt,Setor=="Setor 1"& ano=="A35")%>% mutate(Q_Deflu=jur$Q_Deflu)
df_melt7<- filter(df_melt,Setor!="Setor 1")
df_melt<- rbind(df_melt2,df_melt3,df_melt4,df_melt5,df_melt6,df_melt7)
df_melt<- df_melt %>% filter(Setor=="Setor 1")%>% mutate(C_out=Valor)%>% select(Data, C_out, ano,Q_Deflu)%>% mutate(Font="Model B")


comp<-rbind(ru2,df_melt)

comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))



delft<- read.csv2("../dataset/setores/Setor1_A35_Delft.csv") 
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
delft<- delft %>% mutate(ano="A35", Q_Deflu=jur$Q_Deflu[c(366:731)], Font="Delft3D")

comp<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% filter(ano=="A35")
comp<-rbind(comp,delft)
View(comp)
comp$Font <- factor(comp$Font, levels = c("Model A","Model B", "Delft3D"))

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="A35")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=Font), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.4,0.1),limits=c(0,0.4),expand = c(0,0),labels = seq(0.0,0.4,0.1))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  geom_hline(yintercept = c(0.05), color="black",linetype="dashed")

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft2035 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


# A35 Comparação com reator único e delft - cargas ------------------------------------

Sys.setlocale("LC_TIME", "English")
plot <- comp  %>%
  filter(ano=="A35")%>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  ggplot()+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous()+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (ton/day)")+
  scale_color_manual(name = " ", values= c("#F8766D","#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  geom_line(aes(x= Data, y=C_out*Q_Deflu/1000*86400, color=Font), size=2)

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_capitulos_carga2035 - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

x<- comp %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(C_out)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacaoA35.csv")

x<- comp %>% mutate (y=C_out*Q_Deflu/1000*86400) %>% filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>% group_by(ano, Font) %>% summarise(mean=mean(y)) %>% spread(key=Font, value = mean)
#write.csv2(x, file = "../output/setores/setores_comparacao_loadsA35.csv")




# Delft -------------------------------------------------------------------
delft1_B12<- read.csv2("../dataset/setores/Setor1_B12_Delft.csv") %>% mutate(Setor="Setor 1", ano="B12", model="Delft3D")
delft1_A35<- read.csv2("../dataset/setores/Setor1_A35_Delft.csv") %>% mutate(Setor="Setor 1", ano="A35", model="Delft3D")
delft2_B12<- read.csv2("../dataset/setores/Setor2_B12_Delft.csv") %>% mutate(Setor="Setor 2", ano="B12", model="Delft3D")
delft2_A35<- read.csv2("../dataset/setores/Setor2_A35_Delft.csv") %>% mutate(Setor="Setor 2", ano="A35", model="Delft3D")
delft3_B12<- read.csv2("../dataset/setores/Setor3_B12_Delft.csv") %>% mutate(Setor="Setor 3", ano="B12", model="Delft3D")
delft3_A35<- read.csv2("../dataset/setores/Setor3_A35_Delft.csv") %>% mutate(Setor="Setor 3", ano="A35", model="Delft3D")
delft4_B12<- read.csv2("../dataset/setores/Setor4_B12_Delft.csv") %>% mutate(Setor="Setor 4", ano="B12", model="Delft3D")
delft4_A35<- read.csv2("../dataset/setores/Setor4_A35_Delft.csv") %>% mutate(Setor="Setor 4", ano="A35", model="Delft3D")
delft<-rbind(delft1_B12,delft1_A35,delft2_B12,delft2_A35,delft3_B12,delft3_A35,delft4_B12,delft4_A35)
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
View(delft)
cstr<-df_melt %>% filter (res=="Jurumirim", Variable=="C_out")%>% select(Data, Valor,Setor, ano) %>% mutate(model="Model B")
colnames(cstr)<- c("Data", "C_out", "Setor", "ano","model")
View(cstr)
comp<-rbind(delft,cstr)
comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))
comp$model <- factor(comp$model, levels = c("Model B", "Delft3D"))
View(comp)


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


plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=model), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,0.45,0.2),limits=c(0,0.45),labels = seq(0.0,0.45,0.2))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(.~Setor,labeller = labeller(Setor = variable_names, ano = variable_names2))

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft_cstr_setores - Eng.png", plot= plot, device = "png", width = 16, height = 10, units = "cm")


plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12"|ano=="A35")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=model), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,1.2,0.2),limits=c(0,1.2),labels = seq(0.0,1.2,0.2))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  scale_color_manual(name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(ano~Setor,labeller = labeller(Setor = variable_names, ano = variable_names2))

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft_cstr_setores_anos - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12"|ano=="A35")%>%
  ggplot()+
  geom_boxplot(aes(x= model, y=C_out, fill=model), size=0.01)+
  scale_fill_manual(name= "", breaks = c("Delft3D", "Model B"),labels = c("Delft3D", "Model B"), values= c("#00BA38","#00B9E3"))+
  scale_y_continuous(breaks = seq(0.0,1.2,0.2),limits=c(0,1.2),labels = seq(0.0,1.2,0.2))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  #scale_fill_manual(name = " ", values= c("#00BA38","#00B9E3"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(ano~Setor,labeller = labeller(Setor = variable_names, ano = variable_names2))

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12")%>%
  ggplot()+
  geom_boxplot(aes(x= model, y=C_out, fill=model), size=0.01)+
  scale_fill_manual(name= "", breaks = c("Delft3D", "Model B"),labels = c("Delft3D", "Model B"), values= c("#00BA38","#00B9E3"))+
  scale_y_continuous(breaks = seq(0.0,0.45,0.1),limits=c(0,0.45),labels = seq(0.0,0.45,0.1))+
  theme_bw()+
  labs(title = " ", x="", y = "Phosphorus (mg/L)")+
  #scale_fill_manual(name = " ", values= c("#00BA38","#00B9E3"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(.~Setor,labeller = labeller(Setor = variable_names, ano = variable_names2))

plot
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[Boxplot] - comp_delft_cstr_setores - Eng.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")



# Delft pt nt -------------------------------------------------------------------
delft1_B12<- read.csv2("../dataset/setores/Setor1_B12_Delft.csv") %>% mutate(Setor="Setor 1", ano="B12", model="Delft3D")
delft1_A35<- read.csv2("../dataset/setores/Setor1_A35_Delft.csv") %>% mutate(Setor="Setor 1", ano="A35", model="Delft3D")
delft2_B12<- read.csv2("../dataset/setores/Setor2_B12_Delft.csv") %>% mutate(Setor="Setor 3", ano="B12", model="Delft3D")
delft2_A35<- read.csv2("../dataset/setores/Setor2_A35_Delft.csv") %>% mutate(Setor="Setor 3", ano="A35", model="Delft3D")
delft3_B12<- read.csv2("../dataset/setores/Setor3_B12_Delft.csv") %>% mutate(Setor="Setor 2", ano="B12", model="Delft3D")
delft3_A35<- read.csv2("../dataset/setores/Setor3_A35_Delft.csv") %>% mutate(Setor="Setor 2", ano="A35", model="Delft3D")
delft4_B12<- read.csv2("../dataset/setores/Setor4_B12_Delft.csv") %>% mutate(Setor="Setor 4", ano="B12", model="Delft3D")
delft4_A35<- read.csv2("../dataset/setores/Setor4_A35_Delft.csv") %>% mutate(Setor="Setor 4", ano="A35", model="Delft3D")
delft<-rbind(delft1_B12,delft1_A35,delft2_B12,delft2_A35,delft3_B12,delft3_A35,delft4_B12,delft4_A35)
delft$Data <- as.Date(delft$Data,format="%d/%m/%Y" )
#View(delft)
cstr<-df_melt %>% filter (res=="Jurumirim", Variable=="C_out")%>% select(Data, Valor,Setor, ano) %>% mutate(model="0D")
colnames(cstr)<- c("Data", "C_out", "Setor", "ano","model")

#write.csv2(cstr, file="../output/cstr__.csv")
cstr<-read.csv2(file="../output/cstr__.csv")
cstr$Data <- as.Date(cstr$Data,format="%d/%m/%Y" )
#View(cstr)
comp<-rbind(delft,cstr)
comp$ano <- factor(comp$ano, levels = c("B12","T25", "A25","T35","A35"))
comp$model <- factor(comp$model, levels = c("0D", "Delft3D"))
#View(comp)



Sys.setlocale("LC_TIME", "Portuguese")
plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12"|ano=="A35")%>%
  ggplot()+
  geom_line(aes(x= Data, y=C_out, color=model), size=2)+
  scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,1.2,0.2),limits=c(0,1.2),labels = seq(0.0,1.2,0.2))+
  theme_bw()+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  scale_color_manual(name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(ano~Setor)

plot
fig<- ggplotly(plot)
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[TimeSeries] - comp_delft_cstr_setores_anos - PT NT.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12"|ano=="A35")%>%
  ggplot()+
  geom_boxplot(aes(x= Setor, y=C_out, fill=model))+
  #scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,1.2,0.2),limits=c(0,1.2),labels = seq(0.0,1.2,0.2))+
  #scale_y_log10(breaks = c(0.01,0.1,1),limits=c(0.01,1.2))+
  #scale_y_log10()+
  theme_bw()+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  scale_fill_manual(name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(.~ano)

plot
fig<- ggplotly(plot)
saveWidget(fig, "../html/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.html", background = "white", libdir = "lib", lang = "en")
saveWidget(fig, "../html/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.html")
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")


plot <- comp  %>%
  filter(Data>= as.Date("2012-01-01") & Data <= as.Date("2012-12-31"))%>%
  filter(ano=="B12"|ano=="A35")%>%
  ggplot()+
  geom_boxplot(aes(x= model, y=C_out, fill=model))+
  #scale_x_date(date_labels = "%b/%Y", date_breaks ="2 months",expand = c(0,0))+
  scale_y_continuous(breaks = seq(0.0,1.2,0.2),limits=c(0,1.2),labels = seq(0.0,1.2,0.2))+
  #scale_y_log10(breaks = c(0.01,0.1,1),limits=c(0.01,1.2))+
  #scale_y_log10()+
  theme_bw()+
  labs(title = " ", x="", y = "Fósforo (mg/L)")+
  scale_fill_manual(name = " ", values= c("#00B9E3","#00BA38"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="top")+
  #geom_hline(yintercept = c(0.02), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.03), color="black",linetype="dashed")+
  #geom_hline(yintercept = c(0.05), color="black",linetype="dashed")+
  facet_grid(ano~Setor)

plot
fig<- ggplotly(plot)
saveWidget(fig, "../html/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.html", background = "white", libdir = "lib", lang = "en")
saveWidget(fig, "../html/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.html")
#dir.create("../img/setores")
ggsave(filename = "../img/setores_artigo/[Boxplot] - comp_delft_cstr_setores_anos - PT NT_v2.png", plot= plot, device = "png", width = 16, height = 14, units = "cm")

