# Modelo de Reator Único

df_results <- data.frame()
for (j in unique(df$Cenario)){
    df_cenario <- df %>% filter(Cenario == j)
    df_results <- rbind(df_results, model(df_cenario))
}

