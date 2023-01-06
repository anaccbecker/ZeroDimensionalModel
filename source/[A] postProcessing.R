df_results_A$Cenario<- factor(df_results_A$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))
               
df_wide_A <- df_results_A
write.csv2(df_wide_A, "csv/Reator Único - Resultados.csv")

# Formatação da tabela de resultados para geração de gráficos
df_results_A <- df_results_A %>%
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t, -Q_Vertida, -Q_Turb, -NAs_calc) %>%
    gather(key = "Variable", value = "Valor", -Data, -Q_Aflu, -Q_Deflu, -res,  -Cenario, C_in, C_out) 

# Cálculo do IET
df_results_A<-df_results_A%>% mutate(TSI=NA)
df_results_A$TSI<- sapply(df_results_A$Valor, FUN= TSI)
df_results_A$TSI<- factor(df_results_A$TSI,
    levels = c("Hipereutrófico",
               "Supereutrófico",
               "Eutrófico",
               "Mesotrófico",
               "Oligotrófico",
               "Ultraoligotrófico"))

# Verificação classe 2 CONAMA
df_results_A<-df_results_A%>% mutate(classe2=NA)
df_results_A$classe2<- sapply(df_results_A$Valor,classe2)


df_results_A$Cenario<- factor(df_results_A$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))
