# Modelo de Reator Único

df_results_A <- data.frame()
for (j in unique(df$Cenario)){
    df_cenario <- df %>% filter(Cenario == j)
    df_results_A <- rbind(df_results_A, model(df_cenario))
}

