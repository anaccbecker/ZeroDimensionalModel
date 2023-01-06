# Leitura dados de cotas e vazões -----------------------------------------

jur <- read.table("dataset/Dados_Jurumirim/vazao_SAR_JURUMIRIM.txt", head=T)
jur$Data <- as.Date(jur$Data,format="%Y-%m-%d" )
jur <- jur %>% filter(Data>= as.Date("2011-01-01") & Data <= .end)
names(jur)[7] <- "Vol_or"

# Calculando os volumes (hm³) ---------------------------------------------
jur$Vol <- jur$Vol* 7007.51  /100   #hm³

# Calculando os Níveis d'água (m) - NAs -----------------------------------
jur <- jur %>% mutate(NAs_calc =             0*Vol^4 + 7.32046E-12*Vol^3 - 2.54809E-7*Vol^2 + 4.72244E-3*Vol + 5.44901E2)

# Calculando as áreas de interface (km²) - As -----------------------------
jur <- jur %>% mutate(As = 2*(0*Cota^4 +          0*Cota^3 + 4.00081E-1*Cota^2 - 4.359540E2*Cota + 1.189940E5))

# Ajuste de unidades [Vol - m³ / As - m²] ---------------------------------

jur$Vol <- jur$Vol*1000000
jur <- jur %>% select(-Vol_or)

# Leitura dados de fósforo ------------------------------------------------

jurP <- rbind(read.table("dataset/Dados_Jurumirim/PT_SS_AR1.txt", head=T)[,c(1,2)] %>% mutate(Cenario = "B12"),
    read.table("dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2025_Tendencial.txt", head=T)[,c(1,7)] %>% mutate(Cenario = "T25"),
    read.table("dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2025_Acelerado.txt", head=T)[,c(1,7)] %>% mutate(Cenario = "A25"),
    read.table("dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2035_Tendencial.txt", head=T)[,c(1,7)] %>% mutate(Cenario = "T35"),
    read.table("dataset/Cenarios_Jurumirim/Entradas_GLM_Cenario_2035_Acelerado.txt", head=T)[,c(1,7)] %>% mutate(Cenario = "A35")
)

jurP$Data <- as.Date(jurP$Data,format="%Y-%m-%d" )
jurP <- jurP %>% filter(Data>= as.Date("2011-01-01") & Data <= .end)



# Dataframe de entrada do modelo ----------------------------------------------
df <- left_join(jurP,jur)  %>%
    rename(P=PT)%>%
    mutate (res = "Jurumirim", k=  0.005/86400, set_v = 0.010/86400) 
