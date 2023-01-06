
# Dados para F16
comp <- rbind(
    df_results_A %>% filter(Variable=="C_out" & Cenario =="B12") %>% select(-TSI, -classe2,-res,-Q_Aflu,-Q_Deflu, -Cenario, -Variable)%>% mutate(model="Model A"),
    df_results_B %>% filter(Setor=="Setor 1" & Variable=="C_out" & Cenario =="B12")%>% select(-TSI, -classe2,-res,-Q_Aflu,-Q_Deflu, -Setor,-Cenario, -Variable)%>% mutate(model="Model B"),
    delft %>% filter(Cenario=="B12" & Setor=="Setor 1") %>% select(Data, C_out, model)%>% rename(Valor=C_out )
)%>% 
filter(Data>= .start & Data <= .end)

comp$model <- factor(comp$model, levels = c("Model A", "Model B", "Delft3D"))

# Dados para F17
comp_sector_B12 <- rbind(
    df_results_B %>% filter(Variable=="C_out" & Cenario =="B12")%>% select(Data, Valor, Setor)%>% mutate(model="Model B"),
    delft %>% filter(Cenario=="B12") %>% select(Data, C_out, model, Setor)%>% rename(Valor=C_out)
)%>% 
filter(Data>= .start & Data <= .end)

comp_sector_B12$model <- factor(comp_sector_B12$model, levels = c( "Model B", "Delft3D"))
View(comp_sector)
comp_sector_B12%>% 
group_by(Setor, model)%>% 
summarise(Valor=mean(Valor))

# Dados para F18 boxplot
comp_sector <- rbind(
    df_results_B %>% filter(Variable=="C_out")%>% select(Data, Valor, Setor,Cenario)%>% mutate(model="Model B"),
    delft %>% select(Data, C_out, model, Setor,Cenario)%>% rename(Valor=C_out)
)%>% 
filter(Data>= .start & Data <= .end)%>% 
filter(Cenario=="B12"|Cenario=="A35")

comp_sector$model <- factor(comp_sector$model, levels = c( "Model B", "Delft3D"))




# Desvio
comp2 <- rbind(
    df_results_A %>% filter(Variable=="C_out") %>% select(-TSI, -classe2,-res,-Q_Aflu,-Q_Deflu,  -Variable)%>% mutate(model="Model A"),
    df_results_B %>% filter(Setor=="Setor 1" & Variable=="C_out" )%>% select(-TSI, -classe2,-res,-Q_Aflu,-Q_Deflu, -Setor, -Variable)%>% mutate(model="Model B"),
    delft %>% filter( Setor=="Setor 1") %>% select(Data, C_out, model, Cenario)%>% rename(Valor=C_out )
)%>% 
filter(Data>= .start & Data <= .end)%>%
spread(key=model, 
    value=Valor)

write.csv2(comp2, "csv/[Comparison] deviation.csv")

head(comp2)
comp2$Model A