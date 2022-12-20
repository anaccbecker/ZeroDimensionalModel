df_wide <- df_results
write.csv2(df_wide, "csv/Setores - Resultados.csv")

# Formatação da tabela de resultados para geração de gráficos
df_results <- df_results %>%
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t) %>%
    gather(key = "Variable", value = "Valor", -Data, -Q_Aflu, -Q_Deflu, -res, -Setor, -Cenario, C_in, C_out) 

# Cálculo do IET
df_results<-df_results%>% mutate(TSI=NA)
df_results$TSI<- sapply(df_results$Valor, FUN= TSI)
df_results$TSI<- factor(df_results$TSI,
    levels = c("Hipereutrófico",
               "Supereutrófico",
               "Eutrófico",
               "Mesotrófico",
               "Oligotrófico",
               "Ultraoligotrófico"))

# Verificação classe 2 CONAMA
df_results<-df_results%>% mutate(classe2=NA)
df_results$classe2<- sapply(df_results$Valor,classe2)


df_results$Cenario<- factor(df_results$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))
