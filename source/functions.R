# Aplica o modelo zero-dimensional ---------------------------------------------------------------------
model <- function(df){
  df<- df %>% mutate(i=1:nrow(df), t=86400*i, C_in= df$P, C_out= NA)
  df[1,"C_out"]= 0
  for (i in 2:nrow(df)){
     df[i, "C_out"] = (df[i-1,"C_out"]+(df[i, "t"]-df[i-1, "t"])/df[i, "Vol"] *(df[i, "Q_Aflu"]*df[i, "C_in"]))/(1+(df[i, "t"]-df[i-1, "t"])/df[i,"Vol"]*(df[i,"Q_Deflu"]+df[i,"k"]*df[i,"Vol"]+ df[i,"set_v"] *df[i,"As"]+(df[i,"Vol"]-df[i-1,"Vol"])/(df[i,"t"]-df[i-1,"t"])))
    if ( df[i, "C_out"] < 0){
      df[i, "C_out"] = 0 
    }   
  }
  return(df)
}

# Aplica o modelo zero-dimensional para um setor considerando a saída dos outros três ------------------
modelS1 <- function(df, df2, df3, df4){ 
  df<- df %>% mutate(i=1:nrow(df), t=86400*i, C_in= df$P, C_out= NA) 
  df[1,"C_out"]= 0
  for (i in 2:nrow(df)){
    df[i, "C_out"] = (df[i-1,"C_out"]+(df[i, "t"]-df[i-1, "t"])/df[i, "Vol"] *(df[i, "Q_Aflu"]*df[i, "C_in"]+df2[i, "Q_Deflu"]*df2[i,"C_out"]+df3[i, "Q_Deflu"]*df3[i,"C_out"]+df4[i, "Q_Deflu"]*df4[i,"C_out"]))/(1+(df[i, "t"]-df[i-1, "t"])/df[i,"Vol"]*(df[i,"Q_Deflu"]+df[i,"k"]*df[i,"Vol"]+ df[i,"set_v"] *df[i,"As"]+(df[i,"Vol"]-df[i-1,"Vol"])/(df[i,"t"]-df[i-1,"t"]))) 
    if ( df[i, "C_out"] < 0){
      df[i, "C_out"] = 0
    }
  }
  return(df)
}

# Calcula o Índice de Estado Trófico (IET) ---------------------------------------------------------------------

TSI <- function(p){
  if(p<0.012){
    TSI="Ultraoligotrófico"
  }
  else if(p<0.032){
    TSI="Oligotrófico"
  }
  else if(p<0.125){
    TSI="Mesotrófico"
  }
  else if(p<0.269){
    TSI="Eutrófico"
  }
  else if(p<0.581){
    TSI="Supereutrófico"
  }
  if (p>0.581){
    TSI="Hipereutrófico"
  }
  return(TSI)
}

# Verifica conformidade com a CONAMA -----------------------------------------------------------------

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

variable_labeller <- function(variable,value){
  return(variable_names[value])
}
