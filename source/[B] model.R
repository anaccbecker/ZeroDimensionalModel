# Model for sectors 2, 3 e 4

df_results_B <- data.frame()
df234 <- df %>% filter(Setor != "Setor 1")
for(i in unique(df234$Setor)){
    df_setor <- df234 %>% filter(Setor == i)
    for (j in unique(df$Cenario)){
        df_cenario <- df_setor %>% filter(Cenario == j)
        df_results_B <- rbind(df_results_B,model(df_cenario))
    }
}

# Model for sector 1, applying the output of sectors 2, 3 e 4

for (j in unique(df$Cenario)){
    df_r <- df_results_B %>% filter(Cenario == j)
    result <- modelS1 (
        df %>% filter(Setor == "Setor 1") %>% filter(Cenario == j), 
        df_r %>% filter(Setor == "Setor 2"), 
        df_r %>% filter(Setor == "Setor 3"), 
        df_r %>% filter(Setor == "Setor 4")
        )
    df_results_B <- rbind(df_results_B,result)
}