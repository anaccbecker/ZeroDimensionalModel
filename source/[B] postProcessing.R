df_wide_B <- df_results_B
write.csv2(df_wide_B, "csv/Setores - Resultados.csv")

# Formatação da tabela de resultados para geração de gráficos
df_results_B <- df_results_B %>%
    select(-P,-Cota,-k,-set_v,-Vol,-As,-i,-t) %>%
    gather(key = "Variable", value = "Valor", -Data, -Q_Aflu, -Q_Deflu, -res, -Setor, -Cenario, C_in, C_out) 

# Cálculo do IET
df_results_B<-df_results_B%>% mutate(TSI=NA)
df_results_B$TSI<- sapply(df_results_B$Valor, FUN= TSI)
df_results_B$TSI<- factor(df_results_B$TSI,
    levels = c("Hipereutrófico",
               "Supereutrófico",
               "Eutrófico",
               "Mesotrófico",
               "Oligotrófico",
               "Ultraoligotrófico"))

# Verificação classe 2 CONAMA
df_results_B<-df_results_B%>% mutate(classe2=NA)
df_results_B$classe2<- sapply(df_results_B$Valor,classe2)


df_results_B$Cenario<- factor(df_results_B$Cenario,
    levels = c("B12",
               "T25",
               "A25",
               "T35",
               "A35"))
