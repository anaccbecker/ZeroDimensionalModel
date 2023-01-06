# Leitura dados de cotas e vazões -----------------------------------------

jur <- read.table("dataset/Dados_Jurumirim/vazao_SAR_JURUMIRIM.txt", head=T)

jur$Data <- 
    as.Date(
        jur$Data,
        format="%Y-%m-%d" 
    )
jur <- 
    jur %>% 
    filter(
        Data>= as.Date("2011-01-01") & 
        Data <= .end
    )
names(jur)[7] <- "Vol_or"


# Leitura dados de fósforo na entrada do reservatório  ------------------------------------------------

jurP <- 
    read.csv(
        "dataset/Setores/concentracao_por_setor.csv", 
        head=T, sep=";", dec=",")
jurP$Data <- 
    as.Date(jurP$Data,
    format="%d/%m/%Y" )
jurP <- 
    jurP %>% 
    filter(
        Data>= as.Date("2011-01-01") &
        Data <= .end
    )

# Coeficientes CAV
coef <- 
    data.frame(
        Setor = c("Setor 1","Setor 2","Setor 3","Setor 4"),
        coef1 = c(-0.000720064221654, -0.000544606243188, -0.000260132010078, -0.001179672493933),
        coef2 = c(+ 1.595740162907880, + 1.230324218157030,+ 0.589715146929719,+ 2.678578662380230),
        coef3 = c(- 1322.173470830580000,- 1041.038832151660000,- 500.9342066521430000,- 2278.480533305400000),
        coef4 = c(+ 485546.032222308000000,+ 391066.040022819000000,+ 188979.601745476000000,+ 860589.991537997000000),
        coef5 = c(- 66693262.4393774000000000,- 55032357.3543561000000000,- 26716335.1340117000000000,- 121785247.137767000000000),
        coef6 = c( 0.000085668766059, -0.000028542707748, -0.000035060061477, 0.000097824573642),
        coef7 = c(- 0.196169098968667,+ 0.062840185619947,+ 0.078335645303437,- 0.227462601181334),
        coef8 = c(+ 168.262977098037000,- 51.832622767916000,- 65.611450422384900, + 198.190956637696000),
        coef9 = c(- 64069.715620183800000, 18984.851202710700000, 24415.633729096300000, - 76691.214884574900000),
        coef10 = c( + 9137278.075065730000000, - 2605492.209165040000000,- 3406012.723440820000000, + 11119933.621090200000000))

Ad <- 
    data.frame(
        Setor = c("Setor 1","Setor 2","Setor 3","Setor 4"),
        Ad = c(0.0839895791651107,0.252937160096222,0.0226115293112673,0.6404617314274)
    )

# Dataframe de entrada do modelo    
df <- 
    jurP %>%
    gather(key = "Cenario", value = "P", 3:7) %>% 
    left_join(jur) %>% 
    left_join(Ad) %>%
    select(-Q_Aflu, -Q_Vertida, -Q_Turb) %>% 
    rename(Q_Aflu = Vazao) %>% 
    mutate(Q_Deflu = Q_Deflu*Ad) %>%
    select(-Ad)%>%
    mutate(
        res = "Jurumirim", 
        k=  0.005/86400, 
        set_v = 0.01/86400
    ) %>% 
    left_join(coef) %>%
    mutate(
        Vol= 1000000*(coef1*Cota^4 + coef2*Cota^3 + coef3*Cota^2 + coef4*Cota + coef5),  
        As = 2*1000000*(coef6*Cota^4 +coef7*Cota^3 +coef8 *Cota^2 +coef9*Cota +coef10)) %>% 
    select(-coef1,-coef2,-coef3,-coef4,-coef5,-coef6,-coef7,-coef8,-coef9,-coef10, -Vol_or)

