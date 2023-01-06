df_cargas_A <- 
    df_wide_A %>% 
    mutate(
        w_in= Q_Aflu*C_in/1E3*24*60*60, 
        w_out=Q_Deflu*C_out/1E3*24*60*60, 
        w_lim_in=Q_Aflu*0.03/1E3*24*60*60, 
        w_lim_out=Q_Deflu*0.03/1E3*24*60*60
    )

#OBS: cargas transformadas para kg/dia