
delft1_B12<- read.csv2("dataset/setores/Setor1_B12_Delft.csv") %>% mutate(Setor="Setor 1", Cenario="B12", model="Delft3D")
delft1_A35<- read.csv2("dataset/setores/Setor1_A35_Delft.csv") %>% mutate(Setor="Setor 1", Cenario="A35", model="Delft3D")
delft2_B12<- read.csv2("dataset/setores/Setor2_B12_Delft.csv") %>% mutate(Setor="Setor 2", Cenario="B12", model="Delft3D")
delft2_A35<- read.csv2("dataset/setores/Setor2_A35_Delft.csv") %>% mutate(Setor="Setor 2", Cenario="A35", model="Delft3D")
delft3_B12<- read.csv2("dataset/setores/Setor3_B12_Delft.csv") %>% mutate(Setor="Setor 3", Cenario="B12", model="Delft3D")
delft3_A35<- read.csv2("dataset/setores/Setor3_A35_Delft.csv") %>% mutate(Setor="Setor 3", Cenario="A35", model="Delft3D")
delft4_B12<- read.csv2("dataset/setores/Setor4_B12_Delft.csv") %>% mutate(Setor="Setor 4", Cenario="B12", model="Delft3D")
delft4_A35<- read.csv2("dataset/setores/Setor4_A35_Delft.csv") %>% mutate(Setor="Setor 4", Cenario="A35", model="Delft3D")

delftX<-rbind(delft1_B12,delft1_A35,delft2_B12,delft2_A35,delft3_B12,delft3_A35,delft4_B12,delft4_A35)
delftX$Data <- as.Date(delftX$Data,format="%d/%m/%Y" )


delft1_A25<- read.csv2("dataset/setores/A25_PT_Setor1.csv", sep=",", dec=".", skip=1, header=F) %>% mutate(Setor="Setor 1", Cenario="A25", model="Delft3D", C_out= (V2+V3+V4+V5+V6+V7+V8+V9+V10+V11)/10) %>% rename(Data = V1) %>% select(Data,Setor,Cenario,model,C_out)
delft1_T25<- read.csv2("dataset/setores/T25_PT_Setor1.csv", sep=",", dec=".", skip=1, header=F) %>% mutate(Setor="Setor 1", Cenario="T25", model="Delft3D", C_out= (V2+V3+V4+V5+V6+V7+V8+V9+V10+V11)/10) %>% rename(Data = V1) %>% select(Data,Setor,Cenario,model,C_out)
delft1_T35<- read.csv2("dataset/setores/T35_PT_Setor1.csv", sep=",", dec=".", skip=1, header=F) %>% mutate(Setor="Setor 1", Cenario="T35", model="Delft3D", C_out= (V2+V3+V4+V5+V6+V7+V8+V9+V10+V11)/10) %>% rename(Data = V1) %>% select(Data,Setor,Cenario,model,C_out)
delftY<-rbind(delft1_A25,delft1_T25,delft1_T35)
delftY$Data <- as.Date(delftY$Data,format="%Y-%m-%d %H:%M:%S")
str(delftY)

delft<-rbind(delftX,delftY)