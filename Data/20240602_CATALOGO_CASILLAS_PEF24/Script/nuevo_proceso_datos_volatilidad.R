#install.packages("openxlsx", dependencies = T)
#install.packages("writexl")
library(tmap) #Dibujar el mapa
library(sf) #Para leer el shapefile y reducir el tamaño del archivo
library(pryr) #Calcular el tamaño del archivo
library(readr) #para cargar csv
library(base) # para merge
library(reshape2) # para hacer dcast
library(plyr) # para mineria en csv 
library(dplyr) # para inner join

library(openxlsx)
library(writexl)
library(readxl)
library(foreign)
library(survey)
library(dplyr)
library(tidyverse)
library(data.table)
library(foreign)

library(survey)
library(dplyr)
library(ggpubr)

library(ggplot2)
library(ggpubr)


library("tidyverse")
library("cluster")
library("factoextra")
library("NbClust")
library("tidyr")
#install.packages("ggpubr")



#____________________________________________proceso 2012 a 2015 _____________________________________________________####
#cargamos bases de datos

dp12 = read_xlsx("DATA/DIPUTADOS_12_15_18_21/2012_SEE_DIP_FED_MR_NAL_MUN.xlsx")

dp15 = read_xlsx("DATA/DIPUTADOS_12_15_18_21/2015_SEE_DIP_FED_MR_NAL_MUN.xlsx")

GINI10 = read_xlsx("DATA/COEFICIENTE_GINI_2010.xlsx")


GINI10= GINI10 %>%mutate( 
  ENTIDAD = chartr("ÁÉÍÓÚÜ", "AEIOUU",toupper(ENTIDAD)),
  MUNICIPIO = chartr("ÁÉÍÓÚÜ", "AEIOUU",toupper(MUNICIPIO)),
  ID_MUN2 = paste(CLAVE_ENTIDAD,ENTIDAD,MUNICIPIO, sep = "_"),
  GINI = GINI*100
) %>% 
  filter(!ID_MUN2%in%c("04_CAMPECHE_CALAKMUL",
                       "07_CHIAPAS_ALDAMA",
                       "07_CHIAPAS_BENEMERITO DE LAS AMERICAS",
                       "07_CHIAPAS_MARAVILLA TENEJAPA",
                       "07_CHIAPAS_MARQUES DE COMILLAS",
                       "07_CHIAPAS_MONTECRISTO DE GUERRERO",
                       "07_CHIAPAS_SAN ANDRES DURAZNAL",
                       "07_CHIAPAS_SANTIAGO EL PINAR",
                       "11_GUANAJUATO_SILAO",
                       "17_MORELOS_TLALTIZAPAN",
                       "20_OAXACA_ELOXOCHITLAN DE FLORES MAGON",
                       "20_OAXACA_MAZATLAN VILLA DE FLORES",
                       "20_OAXACA_SAN BALTAZAR CHICHICAPAM",
                       "20_OAXACA_SAN DIONISIO DEL MAR",
                       "20_OAXACA_SAN FRANCISCO DEL MAR",
                       "20_OAXACA_SAN MATEO DEL MAR",
                       "20_OAXACA_SAN PEDRO TOTOLAPAM",
                       "20_OAXACA_SANTA MAGDALENA JICOTLAN",
                       "20_OAXACA_SANTIAGO IXTAYUTLA",
                       "20_OAXACA_SANTIAGO TEPETLAPA",
                       "23_QUINTANA ROO_TULUM",
                       "29_TLAXCALA_ATLTZAYANCA",
                       "20_OAXACA_SAN NICOLAS HIDALGO",
                       "20_OAXACA_SAN MATEO YUCUTINDO",
                       "20_OAXACA_SAN NICOLAS"
  )) %>% 
  arrange(ID_MUN2)




dp12= dp12 %>%mutate( 
  ENTIDAD = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",NOMBRE_ESTADO),
  MUNICIPIO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",MUNICIPIO),
  ID_ESTADO = ifelse(ID_ESTADO<=9, paste(0,ID_ESTADO,sep = ""),ID_ESTADO),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "_"),
  ID_MUN3 = paste(ID_ESTADO,ID_MUNICIPIO, sep = "_"),
  PAN = PAN,
  PRI = PRI +PRI_PVEM,
  PRD = PRD+PRD_PT_MC+PRD_PT+PRD_MC,
  PT= PT+PT_MC,
  MORENA = c(0:0)
  
) %>%
  filter(!ID_MUN2%in%c("11_GUANAJUATO_SILAO","20_OAXACA_SAN BALTAZAR CHICHICAPAM",
                       "20_OAXACA_SAN FRANCISCO DEL MAR" ,"20_OAXACA_SAN NICOLAS",
                       "20_OAXACA_SAN NICOLAS","20_OAXACA_SANTIAGO IXTAYUTLA" ,
                       "20_OAXACA_ZAPOTITLAN DEL RIO","17_MORELOS_TLALTIZAPAN",
                       "20_OAXACA_SAN DIONISIO DEL MAR" ,"20_OAXACA_ELOXOCHITLAN DE FLORES MAGON",
                       "20_OAXACA_SANTIAGO TEPETLAPA" ,"20_OAXACA_MAZATLAN VILLA DE FLORES" ,
                       "20_OAXACA_SANTA MAGDALENA JICOTLAN",
                       "29_TLAXCALA_ALTZAYANCA",
                       "20_OAXACA_SAN PEDRO TOTOLAPA",
                       "20_OAXACA_SAN NICOLAS HIDALGO"
  )) %>% 
  select(ID_ESTADO,NOMBRE_ESTADO,ID_MUNICIPIO,MUNICIPIO,ID_MUN2,ID_MUN3,
         PAN,PRI,PRD,PVEM,PT,MC,NVA_ALIANZA,MORENA) %>% 
  arrange(ID_MUN2)



dp15= dp15 %>%mutate( 
  ENTIDAD = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",NOMBRE_ESTADO),
  MUNICIPIO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",MUNICIPIO),
  ID_ESTADO = ifelse(ID_ESTADO<=9, paste(0,ID_ESTADO,sep = ""),ID_ESTADO),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "_"),
  ID_MUN3 = paste(ID_ESTADO,ID_MUNICIPIO, sep = "_"),
  
  PAN = PAN +PAN_NVA_ALIANZA,
  PRI = PRI +PRI_PVEM,
  PRD = PRD+ PRD_PT,
  MORENA = MORENA,
  CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5+CAND_IND6+CAND_IND7+CAND_IND8+
    CAND_IND9+CAND_IND10+ CAND_IND11+CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+
    CAND_IND17+CAND_IND18+CAND_IND19+CAND_IND20+ CAND_IND21+CAND_IND22,
  OTROS = PH+ES # SE AGREGA VARIABLE CON OTROS
) %>% 
  filter(!ID_MUN2%in%c("04_CAMPECHE_CALAKMUL","11_GUANAJUATO_SILAO DE LA VICTORIA",
                       "17_MORELOS_TLALTIZAPAN DE ZAPATA","20_OAXACA_SAN MATEO DEL MAR",
                       " 20_OAXACA_SAN MATEO YUCUTINDOO","20_OAXACA_SAN MATEO YUCUTINDOO",
                       "29_TLAXCALA_ALTZAYANCA",
                       "20_OAXACA_SAN PEDRO TOTOLAPA",
                       "20_OAXACA_SAN NICOLAS HIDALGO"
  )) %>% 
  select(ID_ESTADO,NOMBRE_ESTADO,ID_MUNICIPIO,MUNICIPIO,ID_MUN2,ID_MUN3,
         PAN,PRI,PRD,PVEM,PT,MC,NVA_ALIANZA,MORENA) %>% 
  arrange(ID_MUN2)

#anti= anti_join(df2,df3,by="ID_MUN3")

#anti= anti_join(dp15,df1,by="ID_MUN2")


# proporciones 

dp12_p =   prop.table(as.matrix(dp12[,-c(1:6)]), margin = 1)*100
dp12[is.na(dp12)]=0
dp15_p =   prop.table(as.matrix(dp15[,-c(1:6)] ), margin = 1 )*100

VOLATILIDAD =  (rowSums(abs(dp12_p-dp15_p), na.rm = T)/2)

VOLATIL12_15 = tibble("ID_ESTADO"= dp15$ID_ESTADO,"NOMBRE_ESTADO"=dp15$NOMBRE_ESTADO, "ID_MUNICIPIO" = dp15$ID_MUNICIPIO,"MUNICIPIO"= dp15$MUNICIPIO,
                   
                   "ID_MUN2"= dp15$ID_MUN2,"ID_MUN2_G" = GINI10$ID_MUN2 ,"ID_MUN3" = dp15$ID_MUN3, "VOL"= VOLATILIDAD, "GINI" = GINI10$GINI )




#________________________________________2015 a 2018____________________________________________________________####

dp15 = read_xlsx("DATA/DIPUTADOS_12_15_18_21/2015_SEE_DIP_FED_MR_NAL_MUN.xlsx")
dp18 = read_xlsx("DATA/DIPUTADOS_12_15_18_21/2018_SEE_DIP_FED_MR_NAL_MUN.xlsx")

GINI15 = read_xlsx("DATA/GINI_2015.xlsx")


dp15= dp15 %>%mutate( 
  NOMBRE_ESTADO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",NOMBRE_ESTADO),
  MUNICIPIO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",MUNICIPIO),
  ID_ESTADO = ifelse(ID_ESTADO<=9, paste(0,ID_ESTADO,sep = ""),ID_ESTADO),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "_"),
  ID_MUN3 = paste(ID_ESTADO,ID_MUNICIPIO, sep = "_"),
  
  PAN = PAN +PAN_NVA_ALIANZA,
  PRI = PRI +PRI_PVEM,
  PRD = PRD+ PRD_PT,
  MORENA = MORENA,
  CAND_IND1 = CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5+CAND_IND6+CAND_IND7+CAND_IND8+
    CAND_IND9+CAND_IND10+ CAND_IND11+CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+
    CAND_IND17+CAND_IND18+CAND_IND19+CAND_IND20+ CAND_IND21+CAND_IND22,
  OTROS = PH+ES # SE AGREGA VARIABLE CON OTROS
) %>% 
  filter(!ID_MUN2%in%c("20_OAXACA_SAN PEDRO TOTOLAPA",
                       "29_TLAXCALA_ALTZAYANCA",
                       "29_TLAXCALA_YAUHQUEMECAN",
                       
                       #no estan en gini
                       "20_OAXACA_MATIAS ROMERO AVENDAÑO",
                       "20_OAXACA_SAN FRANCISCO CHINDUA",
                       "20_OAXACA_SANTA MARIA CHIMALAPA",
                       "20_OAXACA_SANTA MARIA PETAPA",
                       "21_PUEBLA_SAN NICOLAS DE LOS RANCHOS",
                       "26_SONORA_GENERAL PLUTARCO ELIAS CALLES")) %>% 
  select(ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,ID_MUN2,ID_MUN3,PAN,PRI,PRD,PVEM,PT,MC,MORENA,NVA_ALIANZA,CAND_IND1) %>% 
  arrange(ID_MUN2)



dp18 = dp18 %>% mutate(
  NOMBRE_ESTADO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",NOMBRE_ESTADO),
  MUNICIPIO = chartr("ÁÉÍÓÚÌÔÏ", "AEIOUIOU",MUNICIPIO),
  ID_ESTADO = ifelse(ID_ESTADO<=9, paste(0,ID_ESTADO,sep = ""),ID_ESTADO),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "_"),
  ID_MUN3 = paste(ID_ESTADO,ID_MUNICIPIO, sep = "_"),
  PAN = PAN +PAN_PRD_MC + PAN_PRD + PAN_MC+ PRD_MC,
  PRI = PRI +PRI_PVEM_NA + PRI_NA + PRI_PVEM + PVEM_NA,
  MORENA = MORENA + ES +MORENA_ES + PT_MORENA_ES+ PT_MORENA +PT_ES,
  CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5+CAND_IND6+CAND_IND7+CAND_IND8+
    CAND_IND9+CAND_IND10+ CAND_IND11+CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+
    CAND_IND17+CAND_IND18+CAND_IND19+CAND_IND20+ CAND_IND21+CAND_IND22+CAND_IND23+CAND_IND24+
    CAND_IND25+CAND_IND26+CAND_IND27+CAND_IND28+CAND_IND29+CAND_IND30+ 
    CAND_IND31+CAND_IND32+CAND_IND33+CAND_IND34+CAND_IND35+CAND_IND36+CAND_IND37+ CAND_IND38,
  NVA_ALIANZA  = c(0:0)
) %>% 
  filter(!ID_MUN2%in%c("07_CHIAPAS_ALDAMA",
                       "07_CHIAPAS_BENEMERITO DE LAS AMERICAS",
                       "07_CHIAPAS_CAPITAN LUIS ANGEL VIDAL",
                       "07_CHIAPAS_EL PARRAL",
                       "07_CHIAPAS_EMILIANO ZAPATA",
                       "07_CHIAPAS_MARAVILLA TENEJAPA",
                       "07_CHIAPAS_MARQUES DE COMILLAS",
                       "07_CHIAPAS_MEZCALAPA",
                       "07_CHIAPAS_MONTECRISTO DE GUERRERO",
                       "07_CHIAPAS_RINCON CHAMULA SAN PEDRO",
                       "07_CHIAPAS_SAN ANDRES DURAZNAL",
                       "07_CHIAPAS_SANTIAGO EL PINAR",
                       "20_OAXACA_ELOXOCHITLAN DE FLORES MAGON",
                       "20_OAXACA_MAZATLAN VILLA DE FLORES",
                       "20_OAXACA_SAN BALTAZAR CHICHICAPAM",
                       "20_OAXACA_SAN DIONISIO DEL MAR",
                       "20_OAXACA_SAN FRANCISCO DEL MAR",
                       "20_OAXACA_SAN NICOLAS",
                       "20_OAXACA_SAN PEDRO TOTOLAPAM",
                       "20_OAXACA_SANTA MAGDALENA JICOTLAN",
                       "20_OAXACA_SANTIAGO IXTAYUTLA",
                       "20_OAXACA_SAN PEDRO TOTOLAPAM", 
                       "20_OAXACA_SANTA MAGDALENA JICOTLAN",
                       "20_OAXACA_SANTIAGO IXTAYUTLA",
                       "20_OAXACA_SANTIAGO TEPETLAPA",
                       "23_QUINTANA ROO_BACALAR",
                       "29_TLAXCALA_ATLTZAYANCA",
                       "29_TLAXCALA_YAUHQUEMEHCAN",
                       "30_VERACRUZ_MEDELLIN DE BRAVO",
                       "23_QUINTANA ROO_TULUM",
                       "23_QUINTANA ROO_PUERTO MORELOS",
                       
                       #no estan en gini
                       "20_OAXACA_MATIAS ROMERO AVENDAÑO",
                       "20_OAXACA_SAN FRANCISCO CHINDUA",
                       "20_OAXACA_SANTA MARIA CHIMALAPA",
                       "20_OAXACA_SANTA MARIA PETAPA",
                       "21_PUEBLA_SAN NICOLAS DE LOS RANCHOS",
                       "26_SONORA_GENERAL PLUTARCO ELIAS CALLES"
                       
                       )) %>% 
  arrange(ID_MUN2) %>% 
  select(ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,ID_MUN2,ID_MUN3,PAN, PRI, PRD, PVEM, PT, MC, MORENA,NVA_ALIANZA,CAND_IND1)




#anti1= anti_join(dp18,dp15,by="ID_MUN2")
#anti= anti_join(dp15,dp18,by="ID_MUN2")


# proporciones 
dp15[is.na(dp15)]=0
dp15_p =   prop.table(as.matrix(dp15[,-c(1:6)]), margin = 1)*100
dp18[is.na(dp18)]=0
dp18_p =   prop.table(as.matrix(dp18[,-c(1:6)] ), margin = 1 )*100

VOLATILIDAD =  (rowSums(abs(dp15_p-dp18_p), na.rm = T)/2)

VOLATIL15 = tibble("ID_ESTADO"= dp15$ID_ESTADO,"NOMBRE_ESTADO"=dp15$NOMBRE_ESTADO,"MUNICIPIO"= dp15$MUNICIPIO,
                   "ID_MUN2"= dp15$ID_MUN2 ,"ID_MUN3" = dp15$ID_MUN3, "VOL"= VOLATILIDAD )



#gini 2015

#lista de municipios que tienen valor ausentes en base GINI

#08010	Buenaventura
#08012	Carichí
#08024	Santa Isabel
#08063	Temósachic
#08065	Urique
#20057	Matías Romero Avendaño
#20140	San Francisco Chindúa
#20407	Santa María Chimalapa
#20427	Santa María Petapa
#21138	San Nicolás de los Ranchos
#26070	General Plutarco Elías Calles


GINI15= GINI15 %>%mutate( 
  ENTIDAD = chartr("ÁÉÍÓÚÜ", "AEIOUU",toupper(ENTIDAD)),
  MUNICIPIO = chartr("ÁÉÍÓÚÜ", "AEIOUU",toupper(MUNICIPIO)),
  ID_MUN2 = paste(CLAVE_ENTIDAD,ENTIDAD,MUNICIPIO, sep = "_"),
  GINI = GINI*100
) %>% 
  filter(!CLAVE_MUNICIPIO%in%c(08010,08012,08024,08063,08065,20057,
                               20140,20407,20427,21138,26070),
         !ID_MUN2%in%c("07_CHIAPAS_ALDAMA",
                       "07_CHIAPAS_BENEMERITO DE LAS AMERICAS",
                       "07_CHIAPAS_MARAVILLA TENEJAPA",
                       "07_CHIAPAS_MARQUES DE COMILLAS",
                       "07_CHIAPAS_MONTECRISTO DE GUERRERO",
                       "07_CHIAPAS_SAN ANDRES DURAZNAL",
                       "07_CHIAPAS_SANTIAGO EL PINAR",
                       "20_OAXACA_ELOXOCHITLAN DE FLORES MAGON",
                       "20_OAXACA_MAZATLAN VILLA DE FLORES",
                       "20_OAXACA_SAN BALTAZAR CHICHICAPAM",
                       "20_OAXACA_SAN DIONISIO DEL MAR",
                       "20_OAXACA_SAN FRANCISCO DEL MAR",
                       "20_OAXACA_SAN NICOLAS",
                       "20_OAXACA_SAN PEDRO TOTOLAPAM",
                       "20_OAXACA_SANTA MAGDALENA JICOTLAN",
                       "20_OAXACA_SANTIAGO IXTAYUTLA",
                       "20_OAXACA_SANTIAGO TEPETLAPA",
                       "23_QUINTANA ROO_BACALAR",
                       "23_QUINTANA ROO_TULUM",
                       "29_TLAXCALA_ATLTZAYANCA",
                       "29_TLAXCALA_YAUHQUEMEHCAN")
  ) %>% 
  arrange(ID_MUN2)



#anti1= anti_join(GINI1,dp18,by="ID_MUN2")
#anti= anti_join(dp18,GINI1,by="ID_MUN2")


VOLATIL15_18 = tibble("ID_ESTADO"= dp15$ID_ESTADO,"ID_MUNICIPIO"= dp15$ID_MUNICIPIO,"NOMBRE_ESTADO"=dp15$NOMBRE_ESTADO,"MUNICIPIO"= dp15$MUNICIPIO,
                   "ID_MUN2"= dp15$ID_MUN2,"ID_MUN3" = dp15$ID_MUN3, "VOL"= VOLATILIDAD, "GINI" = GINI15$GINI )





#_______________________________________proceso 2018 a 2021______________________________________________________####

# en la limpieza da datos hay que cambiar el DOCTOR = DR.,, EL GENERAL = GRAL.
####2018###########
dp18 = read.csv("DATA/DatosAbiertos-2018.csv")
dp21 = read.csv("DATA/DatosAbiertos-2021.csv")


GINI20 = read_xlsx("DATA/GINI_2020.xlsx")


#GINI20 = GINI20 %>% mutate(
 # EDO_MUN = paste(Entidad, Municipio, sep = "_"),
#  ID_EDO =paste(Clave_entidad, ID_MUN, sep = "_") )%>% 
  #arrange(ID_EDO) 
#7	Chiapas	172.00	07125	Honduras de la Sierra	n.d.
#Chiapas	64	112.00	07064	Oxchuc	0.371
#7	Chiapas	128.00	07080	Siltepec	0.304
#7	Chiapas	153.00	07106	Venustiano Carranza	0.361
#641	17	Morelos	34	17034	Coatetelco	0.403	Morelos_Coatetelco	17_34
#17	Morelos	35	17035	Xoxocotla	0.406	Morelos_Xoxocotla	17_35
#17	Morelos	36	17036	Hueyapan	0.411	Morelos_Hueyapan	17_36
#2	Baja California	6	2006	San Quintín	0.346	Baja California_San Quintín	2_6
#20	Oaxaca	247	20247	Capulálpam de Méndez	0.323	Oaxaca_Capulálpam de Méndez	20_247
#4	Campeche	12	04012	Seybaplaya	n.d.
#20	Oaxaca	248	20248	San Mateo del Mar	0.326
#17	Morelos	36	17036	Hueyapan	0.411	Morelos_Hueyapan	17_36	FALSO
#17	Morelos	35	17035	Xoxocotla	0.406	Morelos_Xoxocotla	17_35
#17_34_MORELOS_COATETELCO,17_35_MORELOS_XOXOCOTLA,17_36_MORELOS_HUEYAPAN,4_12_CAMPECHE_DZITBALCHE,
#4_13_CAMPECHE_SEYBAPLAYA
#filter(!ID_EDO%in%c("17_34","17_35","17_36","4_12","4_13"))
#dp<-merge(dp21[,c(setdiff(names(dp21),names(GINI20)),"ID_EDO")],GINI20,by="ID_EDO",all.x =T,sort = F)
#anti= anti_join(VOLATILIDAD20,GINI20,by="ID_EDO")
#EDO_MUN_FAL= VOLATILIDAD20$ID_EDO[VOLATILIDAD20$ID_EDO%in%GINI20$ID_EDO==F]
#EDO_MUN_FAL2= VOLATILIDAD20$ID_EDO[GINI20$ID_EDO%in%VOLATILIDAD20$ID_EDO==F]

#GINI20 = GINI20 %>% 
 # arrange(ID_EDO)


dp21 = dp21 %>% mutate(
   EDO_MUN = paste(NOMBRE_ESTADO, MUNICIPIO,ID_MUNICIPIO, sep = "_"),
   ID_EDO =paste(ID_ESTADO, ID_MUNICIPIO, sep = "_") ,
   ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "."))%>% 
  arrange(ID_MUN2) %>% 
  #17_34_MORELOS_COATETELCO,17_35_MORELOS_XOXOCOTLA,17_36_MORELOS_HUEYAPAN,4_12_CAMPECHE_DZITBALCHE,
  #4_13_CAMPECHE_SEYBAPLAYA
  filter(!ID_EDO%in%c("17_34","17_35","17_36","4_12","4_13"))

dp18 = dp18 %>% mutate(
  EDO_MUN= paste(NOMBRE_ESTADO, MUNICIPIO,ID_MUNICIPIO,sep = "_"),
  ID_EDO =paste(ID_ESTADO, ID_MUNICIPIO, sep = "_"),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = ".")) %>% 
  # !20_247_OAXACA_SAN MATEO DEL MAR,7_106_CHIAPAS_VENUSTIANO CARRANZA,7_64_CHIAPAS_OXCHUC,7_81,CHIAPAS_SILTEPEC
  arrange(ID_MUN2) %>% 
  filter(!ID_EDO%in%c("20_247","7_106","7_64","7_81"))

#dp<-merge(dp21[,c(setdiff(names(dp21),names(dp18)),"ID_EDO")],dp18,by="ID_EDO",all.x =T,sort = F)
#EDO_MUN_FAL= dp21$ID_EDO[dp21$ID_EDO%in%dp18$ID_EDO==F]
#EDO_MUN_FAL= dp18$ID_EDO[dp18$ID_EDO%in%dp21$ID_EDO==F]
#df=tibble(dp18$ID_EDO,dp21$ID_EDO,dp18$EDO_MUN,dp21$EDO_MUN)


dp18= dp18%>% 
  mutate(
    
    PAN = PAN +PAN_PRD_MC + PAN_PRD + PAN_MC+ PRD_MC,
    PRI = PRI +PRI_PVEM_NA + PRI_NA + PRI_PVEM + PVEM_NA,
    MORENA = MORENA + ES +MORENA_ES + PT_MORENA_ES+ PT_MORENA +PT_ES,
    CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5+CAND_IND6+CAND_IND7+CAND_IND8+
      CAND_IND9+CAND_IND10+ CAND_IND11+CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+
      CAND_IND17+CAND_IND18+CAND_IND19+CAND_IND20+ CAND_IND21+CAND_IND22+CAND_IND23+CAND_IND24+
      CAND_IND25+CAND_IND26+CAND_IND27+CAND_IND28+CAND_IND29+CAND_IND30+ 
      CAND_IND31+CAND_IND32+CAND_IND33+CAND_IND34+CAND_IND35+CAND_IND36+CAND_IND37+ CAND_IND38,
    OTROS = NA. # SE AGREGA VARIABLE CON OTROS

  ) %>%  
  select(ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,EDO_MUN,ID_EDO,MUNICIPIO,PAN, PRI, PRD, PVEM, PT, MC, MORENA,CAND_IND1, OTROS )





dp21 = dp21 %>% 
  mutate(
    PAN = PAN + PAN_PRI_PRD + PAN_PRI + PAN_PRD + PRI_PRD,
    MORENA = MORENA+ PVEM_PT_MORENA + PVEM_PT + PVEM_MORENA + PT_MORENA,
    CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3,
    OTROS = PES+ RSP+FXM
  ) %>%  
  select(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO,EDO_MUN,ID_EDO,MUNICIPIO,PAN, PRI, PRD, PVEM, PT, MC,MORENA,CAND_IND1, OTROS )


         
# proporciones 

dp18_p =   prop.table(as.matrix(dp18[,-c(1,2,3,4,5,6)] ), margin = 1)*100
dp21[is.na(dp21)]=0
dp21_p =   prop.table(as.matrix(dp21[,-c(1,2,3,4,5)] ), margin = 1 )*100

VOLATILIDAD =  (rowSums(abs(dp18_p-dp21_p), na.rm = T)/2)

VOLATILIDAD20 = tibble("ID_ESTADO"= dp21$ID_ESTADO,"ID_MUNICIPIO"= dp18$ID_MUNICIPIO,"NOMBRE_ESTADO"=dp21$NOMBRE_ESTADO, "MUNICIPIO"= dp21$MUNICIPIO,
                       
  "EDO_MUN"= dp21$EDO_MUN,"ID_EDO" = dp21$ID_EDO, "VOL"= VOLATILIDAD )


# comprobar que las bases volatilidad y coeficiente de gini son las mismas

GINI20 = GINI20 %>% 
  mutate(ID_MUN2 = paste(CLAVE_ENTIDAD,ENTIDAD,MUNICIPIO, sep = ".")) %>% 
  arrange(ID_MUN2)

VOLATILIDAD20 = VOLATILIDAD20 %>% 
  mutate(ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = ".")) %>% 
  arrange(ID_MUN2)

anti= anti_join(VOLATILIDAD20,GINI20,by="ID_MUN2")




# juntamos bases de volatilidad y gini

VOLATIL18_21 = tibble("ID_ESTADO"= VOLATILIDAD20$ID_ESTADO,"ID_MUNICIPIO"= VOLATILIDAD20$ID_MUNICIPIO,"NOMBRE_ESTADO"=VOLATILIDAD20$NOMBRE_ESTADO,
                "MUNICIPIO"= VOLATILIDAD20$MUNICIPIO,"EDO_MUN"= VOLATILIDAD20$EDO_MUN,
                "ID_EDO" = VOLATILIDAD20$ID_EDO, "VOL"= VOLATILIDAD , "GINI" = GINI20$COEF)

# se guardo la base de datos y se cambio el nombre a los municipios que tenian el mismo nombre 
#write_xlsx(VOLATIL18_21,"VOLATILIDAD_GINI_18_21.xlsx")


#___________________________________________________resumen descriptivo______________________________________________________####

# se dejan fuera los municipios que presentan volatilidad 0, esto es porque desde un inicio vienen municipios pero no 
#presentan valores, es decir, solo estan en la lista pero no tuvieron elecciones

VOLATIL12_15= VOLATIL12_15 %>% 
  filter(!ID_MUN2%in%c("20_OAXACA_SANTA ANA ATEIXTLAHUACA",
                       "20_OAXACA_CHAHUITES",
                       "20_OAXACA_SAN MIGUEL PIEDRAS",
                       "20_OAXACA_SANTA MARIA TEXCATITLAN",
                       "20_OAXACA_SANTA CRUZ ACATEPEC",
                       "20_OAXACA_SANTA MARIA TEOPOXCO",
                       "20_OAXACA_SAN LORENZO CUAUNECUILTITLA",
                       "20_OAXACA_HUAUTEPEC",
                       "20_OAXACA_SAN MIGUEL PANIXTLAHUACA",
                       "20_OAXACA_SAN PEDRO TEOZACOALCO",
                       "20_OAXACA_SAN FRANCISCO HUEHUETLAN",
                       "20_OAXACA_SAN JERONIMO TECOATL",
                       "20_OAXACA_SAN MIGUEL SUCHIXTEPEC",
                       "20_OAXACA_SAN FRANCISCO IXHUATAN")
         ) %>% 
  mutate("VOL_NORM"  = scale(VOL),
         "GINI_NORM"  = scale(GINI))


VOLATIL15_18= VOLATIL15_18 %>% 
  filter(!ID_MUN2%in%c("20_OAXACA_CHAHUITES",
                       "20_OAXACA_HUAUTEPEC",
                       "20_OAXACA_SAN FRANCISCO HUEHUETLAN",
                       "20_OAXACA_SAN FRANCISCO IXHUATAN",
                       "20_OAXACA_SAN JERONIMO TECOATL",
                       "20_OAXACA_SAN LORENZO CUAUNECUILTITLA",
                       "20_OAXACA_SAN MIGUEL PANIXTLAHUACA",
                       "20_OAXACA_SAN MIGUEL SUCHIXTEPEC",
                       "20_OAXACA_SANTA ANA ATEIXTLAHUACA",
                       "20_OAXACA_SANTA CRUZ ACATEPEC",
                       "20_OAXACA_SANTA MARIA TEOPOXCO",
                       "20_OAXACA_SANTA MARIA TEXCATITLAN",
                       "31_YUCATAN_SANAHCAT",
                       #no tienen coeficiente de GINI
                       "08_CHIHUAHUA_TEMOSACHIC",
                       "08_CHIHUAHUA_SANTA ISABEL",
                       "08_CHIHUAHUA_BUENAVENTURA",
                       "08_CHIHUAHUA_CARICHI",
                       "08_CHIHUAHUA_URIQUE"
                       )
  ) %>% 
  mutate("VOL_NORM"  = scale(VOL),
         "GINI_NORM"  = scale(GINI))



VOLATIL18_21= VOLATIL18_21 %>% 
  filter(!EDO_MUN%in%c("OAXACA_REFORMA DE PINEDA_72",
                       "OAXACA_SANTA MARIA MIXTEQUILLA_422",
                       "OAXACA_SANTA MARIA XADANI_442",
                       "YUCATAN_SANAHCAT_64",
                       #no tienen coeficiente de GINI
                       "TLAXCALA_LA MAGDALENA TLALTELULCO_52"
                      
  )
  ) 



RESUMEN12_15 = summary(VOLATIL12_15[,8:9])
RESUMEN15_18 = summary(VOLATIL15_18[,c("VOL","GINI")])
RESUMEN18_21 = summary(VOLATIL18_21[,c("VOL","GINI")])

VOLATIL12_15$PER = c("12_15")
VOLATIL15_18$PER = c("15_18")
VOLATIL18_21$PER = c("18_21")

#names(VOLATIL12_15)= names(VOLATIL15_18) = names(VOLATIL18_21)

#VOLATIL_GINI = rbind(VOLATIL12_15[,c(1,2,4,8,9,12)],
#                     VOLATIL15_18[,c(1,2,3,6,7,10)],
#                     VOLATIL18_21[,c(1,2,3,6,7,8)])

#write.csv(VOLATIL_GINI,"TABLA_VOLATIL_GINI_PER.csv")

# descriptivos para periodo 2012 a 2015

RESUMEN12_15 = summary(VOLATIL12_15[,8:9])

VOLATIL12_15= VOLATIL12_15 %>% 
  mutate(G_VOL = c("VOL"),
         G_GINI =c("GINI"))

df1 =VOLATIL12_15$VOL
df2 =VOLATIL12_15$GINI
df3 =VOLATIL12_15$G_VOL
df4 =VOLATIL12_15$G_GINI

df=data.frame(
  rbind(
  cbind(df1,df3),
  cbind(df2,df4)
)
)

colnames(df) =c("x","grupo")

df=df %>% mutate(x= as.numeric(as.character(x)))

library(ggplot2)

k_colors = c("#0000FF","#4682B4")

#layout_matrix <- matrix(1:3, ncol = 3)
#par(mfrow = c(1, 1))
# Especificar el diseño
#layout(layout_matrix)

ggplot(df, aes(x = x, fill = grupo)) + 
  geom_density (alpha = 0.5, position = "identity")+
  scale_fill_manual(values = k_colors, labels = c( "Coeficiente de Gini", "Volatilidad electoral ") )+
  guides(fill = guide_legend(title = " "))+
  labs(x = "Coeficientes" ,y = "Densidad" )+
  theme(legend.position = "bottom")



df %>%
  ggboxplot(x       = "grupo", 
            y       = "x",
            fill    = "grupo",
            palette = c("#2e00fa", "#4682B4"))+
  theme_bw()+
  labs(
    x = " ", y = "Coeficientes" )+
  theme(legend.position = "bottom")+ 
  scale_fill_manual(values = k_colors, labels = c("Volatilidad electoral ", "Coeficiente de Gini") )+
  guides(fill = guide_legend(title = " "))


ggplot(df, aes(x = grupo, y = x, fill = grupo)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.04) +
  theme_bw()+
  scale_fill_manual(values = c("#2e00fa", "#4682B4"))

# generar mapas


shp_municipios <- st_read("DATA/shp/889463776079_s/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00mun.shp" )

VOLATIL12_15= VOLATIL12_15 %>% 
   mutate(ID_MUNICIPIO= ifelse(ID_MUNICIPIO%in%c(1:9),paste("00",ID_MUNICIPIO ,sep = ""),
                               ifelse(ID_MUNICIPIO%in%c(10:99),paste("0",ID_MUNICIPIO,sep = ""),ID_MUNICIPIO
                               
             )),
          CVEGEO = paste(ID_ESTADO,ID_MUNICIPIO ,sep = "")
      
          )
 

# shp = shp_municipios[shp_municipios$CVEGEO%in%VOLATIL12_15$CVEGEO,]
#shp = merge(VOLATIL12_15,shp_municipios,by="CVEGEO",all.y =T,sort = F)
 
 #df= shp %>% select(CVEGEO, ID_ESTADO, NOMBRE_ESTADO,CVE_ENT,CVE_MUN,ID_MUNICIPIO,NOMGEO,MUNICIPIO)
 #write.csv(df,"shp_mapas.csv")

 shp_municipios= inner_join(shp_municipios,VOLATIL12_15, by = "CVEGEO")
 
 
 shp_municipios1 = shp_municipios %>%  
   select(CVEGEO,MUNICIPIO, GINI, geometry)
 #gini
 tm_shape(shp_municipios1)+
   tm_fill("GINI", palette = "Blues", id = "MUNICIPIO", title = "Gini") +
   tm_borders("grey25", alpha = .20)+
   tm_layout(".",
             main.title.position = "") +
   tm_view(view.legend.position = c("left", "bottom"))
 
 #menor
 mu = VOLATIL12_15 %>% 
   filter(GINI<=40 )
 
 mu = VOLATIL12_15 %>% 
   filter(GINI>=50 )

 # volatilidad 

  shp_municipios2 = shp_municipios %>%  
   select(CVEGEO,MUNICIPIO, VOL, geometry)


 tm_shape(shp_municipios2)+
   tm_fill("VOL", palette = "Blues", id = "MUNICIPIO", title = "Volatiliad") +
   tm_borders("grey25", alpha = .20)+
   tm_layout(".",main.title.position = "") +
   tm_view(view.legend.position = c("left", "bottom"))
 
 
 mu = VOLATIL15_18 %>% 
   filter(VOL<=20 )
 
 data.frame(table(mu$NOMBRE_ESTADO))
 data.frame(table(VOLATIL15_18$NOMBRE_ESTADO))
 
 
 
 mu = VOLATIL15_18 %>% 
   filter(VOL>=80 )
 
 data.frame(table(mu$NOMBRE_ESTADO))
 #_____________________________descriptivos para periodo 2015 a 2018___________________________________####
 
 RESUMEN15_18 = summary(VOLATIL15_18[,c("VOL","GINI")])
 
 VOLATIL15_18= VOLATIL15_18 %>% 
   mutate(G_VOL = c("VOL"),
          G_GINI =c("GINI"))
 
 df1 =VOLATIL15_18$VOL
 df2 =VOLATIL15_18$GINI
 df3 =VOLATIL15_18$G_VOL
 df4 =VOLATIL15_18$G_GINI
 
 df=data.frame(
   rbind(
     cbind(df1,df3),
     cbind(df2,df4)
   )
 )
 
 colnames(df) =c("x","grupo")
 
 df=df %>% mutate(x= as.numeric(as.character(x)))
 
 library(ggplot2)
 
 k_colors = c("#0000FF","#4682B4")
 
 #layout_matrix <- matrix(1:3, ncol = 3)
 #par(mfrow = c(1, 1))
 # Especificar el diseño
 #layout(layout_matrix)
 

 
 ggplot(df, aes(x = x, fill = grupo)) + 
   geom_density (alpha = 0.5, position = "identity")+
   scale_fill_manual(values = k_colors, labels = c( "Coeficiente de Gini", "Volatilidad electoral ") )+
   guides(fill = guide_legend(title = " "))+
   labs(x = "Coeficientes" ,y = "Densidad" )+
   theme(legend.position = "bottom")
 
 
 df %>%
   ggboxplot(x       = "grupo", 
             y       = "x",
             fill    = "grupo",
             palette = c("#2e00fa", "#4682B4"))+
   theme_bw()+
   labs(
     x = " ", y = "Coeficientes" )+
   theme(legend.position = "bottom")+ 
   scale_fill_manual(values = k_colors, labels = c("Volatilidad electoral ", "Coeficiente de Gini") )+
   guides(fill = guide_legend(title = " "))
 
 ggplot(df, aes(x = grupo, y = x, fill = grupo)) +
   geom_violin(trim = FALSE) + 
   geom_boxplot(width = 0.04) +
   theme_bw()+
   scale_fill_manual(values = c("#2e00fa", "#4682B4"))
 
 # generar mapas
 
 
 shp_municipios <- st_read("DATA/shp/889463776079_s/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00mun.shp" )
 
 VOLATIL15_18= VOLATIL15_18 %>% 
   mutate(ID_MUNICIPIO= ifelse(ID_MUNICIPIO%in%c(1:9),paste("00",ID_MUNICIPIO ,sep = ""),
                               ifelse(ID_MUNICIPIO%in%c(10:99),paste("0",ID_MUNICIPIO,sep = ""),ID_MUNICIPIO
                                      
                               )),
          CVEGEO = paste(ID_ESTADO,ID_MUNICIPIO ,sep = "")
          
   )
 
 
 # shp = shp_municipios[shp_municipios$CVEGEO%in%VOLATIL12_15$CVEGEO,]
 #shp = merge(VOLATIL12_15,shp_municipios,by="CVEGEO",all.y =T,sort = F)
 
 #df= shp %>% select(CVEGEO, ID_ESTADO, NOMBRE_ESTADO,CVE_ENT,CVE_MUN,ID_MUNICIPIO,NOMGEO,MUNICIPIO)
 #write.csv(df,"shp_mapas.csv")
 
 shp_municipios= inner_join(shp_municipios,VOLATIL15_18, by = "CVEGEO")
 
 shp_municipios1 = shp_municipios %>%  
   select(CVEGEO,MUNICIPIO, GINI, geometry)
 
 #gini
 tm_shape(shp_municipios1)+
   tm_fill("GINI", palette = "Blues", id = "MUNICIPIO", title = "Gini") +
   tm_borders("grey25", alpha = .20)+
   tm_layout(".",
             main.title.position = "") +
   tm_view(view.legend.position = c("left", "bottom"))
 
 #menor
 mu = VOLATIL15_18 %>% 
   filter(GINI<=40 )
 
 mu = VOLATIL15_18 %>% 
   filter(GINI>=50 )
 
 # volatilidad 
 
 shp_municipios2 = shp_municipios %>%  
   select(CVEGEO,MUNICIPIO, VOL, geometry)
 
 
 tm_shape(shp_municipios2)+
   tm_fill("VOL", palette = "Blues", id = "MUNICIPIO", title = "Volatiliad") +
   tm_borders("grey25", alpha = .20)+
   tm_layout(".",main.title.position = "") +
   tm_view(view.legend.position = c("left", "bottom"))
 
 
 mu = VOLATIL15_18 %>% 
   filter(VOL>=60 )
 


RESUMEN15_18 = summary(VOLATIL15_18[,6:7])

mu = VOLATIL15_18 %>% 
  filter(VOL<=20 )

data.frame(table(mu$NOMBRE_ESTADO))
data.frame(table(VOLATIL15_18$NOMBRE_ESTADO))



mu = VOLATIL15_18 %>% 
  filter(VOL>=80 )

data.frame(table(mu$NOMBRE_ESTADO))

#
mu = VOLATIL15_18 %>% 
  filter(GINI<=30 )

data.frame(table(mu$NOMBRE_ESTADO))
data.frame(table(VOLATIL15_18$NOMBRE_ESTADO))



mu = VOLATIL15_18 %>% 
  filter(GINI>=50 )

data.frame(table(mu$NOMBRE_ESTADO))


#_____________________________descriptivos para periodo 2018 a 2021___________________________________####



# en la limpieza da datos hay que cambiar el DOCTOR = DR.,, EL GENERAL = GRAL.
####2018###########
dp18 = read.csv("DATA/DatosAbiertos-2018.csv")
dp21 = read.csv("DATA/DatosAbiertos-2021.csv")


GINI20 = read_xlsx("DATA/GINI_2020.xlsx")


#GINI20 = GINI20 %>% mutate(
# EDO_MUN = paste(Entidad, Municipio, sep = "_"),
#  ID_EDO =paste(Clave_entidad, ID_MUN, sep = "_") )%>% 
#arrange(ID_EDO) 
#7	Chiapas	172.00	07125	Honduras de la Sierra	n.d.
#Chiapas	64	112.00	07064	Oxchuc	0.371
#7	Chiapas	128.00	07080	Siltepec	0.304
#7	Chiapas	153.00	07106	Venustiano Carranza	0.361
#641	17	Morelos	34	17034	Coatetelco	0.403	Morelos_Coatetelco	17_34
#17	Morelos	35	17035	Xoxocotla	0.406	Morelos_Xoxocotla	17_35
#17	Morelos	36	17036	Hueyapan	0.411	Morelos_Hueyapan	17_36
#2	Baja California	6	2006	San Quintín	0.346	Baja California_San Quintín	2_6
#20	Oaxaca	247	20247	Capulálpam de Méndez	0.323	Oaxaca_Capulálpam de Méndez	20_247
#4	Campeche	12	04012	Seybaplaya	n.d.
#20	Oaxaca	248	20248	San Mateo del Mar	0.326
#17	Morelos	36	17036	Hueyapan	0.411	Morelos_Hueyapan	17_36	FALSO
#17	Morelos	35	17035	Xoxocotla	0.406	Morelos_Xoxocotla	17_35
#17_34_MORELOS_COATETELCO,17_35_MORELOS_XOXOCOTLA,17_36_MORELOS_HUEYAPAN,4_12_CAMPECHE_DZITBALCHE,
#4_13_CAMPECHE_SEYBAPLAYA
#filter(!ID_EDO%in%c("17_34","17_35","17_36","4_12","4_13"))
#dp<-merge(dp21[,c(setdiff(names(dp21),names(GINI20)),"ID_EDO")],GINI20,by="ID_EDO",all.x =T,sort = F)
#anti= anti_join(VOLATILIDAD20,GINI20,by="ID_EDO")
#EDO_MUN_FAL= VOLATILIDAD20$ID_EDO[VOLATILIDAD20$ID_EDO%in%GINI20$ID_EDO==F]
#EDO_MUN_FAL2= VOLATILIDAD20$ID_EDO[GINI20$ID_EDO%in%VOLATILIDAD20$ID_EDO==F]

#GINI20 = GINI20 %>% 
# arrange(ID_EDO)


dp21 = dp21 %>% mutate(
  EDO_MUN = paste(NOMBRE_ESTADO, MUNICIPIO,ID_MUNICIPIO, sep = "_"),
  ID_EDO =paste(ID_ESTADO, ID_MUNICIPIO, sep = "_") ,
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = "."))%>% 
  arrange(ID_MUN2) %>% 
  #17_34_MORELOS_COATETELCO,17_35_MORELOS_XOXOCOTLA,17_36_MORELOS_HUEYAPAN,4_12_CAMPECHE_DZITBALCHE,
  #4_13_CAMPECHE_SEYBAPLAYA
  filter(!ID_EDO%in%c("17_34","17_35","17_36","4_12","4_13"))

dp18 = dp18 %>% mutate(
  EDO_MUN= paste(NOMBRE_ESTADO, MUNICIPIO,ID_MUNICIPIO,sep = "_"),
  ID_EDO =paste(ID_ESTADO, ID_MUNICIPIO, sep = "_"),
  ID_MUN2 = paste(ID_ESTADO,NOMBRE_ESTADO,MUNICIPIO, sep = ".")) %>% 
  # !20_247_OAXACA_SAN MATEO DEL MAR,7_106_CHIAPAS_VENUSTIANO CARRANZA,7_64_CHIAPAS_OXCHUC,7_81,CHIAPAS_SILTEPEC
  arrange(ID_MUN2) %>% 
  filter(!ID_EDO%in%c("20_247","7_106","7_64","7_81"))

#dp<-merge(dp21[,c(setdiff(names(dp21),names(dp18)),"ID_EDO")],dp18,by="ID_EDO",all.x =T,sort = F)
#EDO_MUN_FAL= dp21$ID_EDO[dp21$ID_EDO%in%dp18$ID_EDO==F]
#EDO_MUN_FAL= dp18$ID_EDO[dp18$ID_EDO%in%dp21$ID_EDO==F]
#df=tibble(dp18$ID_EDO,dp21$ID_EDO,dp18$EDO_MUN,dp21$EDO_MUN)


dp18= dp18%>% 
  mutate(
    
    PAN = PAN +PAN_PRD_MC + PAN_PRD + PAN_MC+ PRD_MC,
    PRI = PRI +PRI_PVEM_NA + PRI_NA + PRI_PVEM + PVEM_NA,
    MORENA = MORENA + ES +MORENA_ES + PT_MORENA_ES+ PT_MORENA +PT_ES,
    CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3+CAND_IND4+CAND_IND5+CAND_IND6+CAND_IND7+CAND_IND8+
      CAND_IND9+CAND_IND10+ CAND_IND11+CAND_IND12+CAND_IND13+CAND_IND14+CAND_IND15+CAND_IND16+
      CAND_IND17+CAND_IND18+CAND_IND19+CAND_IND20+ CAND_IND21+CAND_IND22+CAND_IND23+CAND_IND24+
      CAND_IND25+CAND_IND26+CAND_IND27+CAND_IND28+CAND_IND29+CAND_IND30+ 
      CAND_IND31+CAND_IND32+CAND_IND33+CAND_IND34+CAND_IND35+CAND_IND36+CAND_IND37+ CAND_IND38,
    OTROS = NA. # SE AGREGA VARIABLE CON OTROS
    
  ) %>%  
  select(ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,EDO_MUN,ID_EDO,MUNICIPIO,PAN, PRI, PRD, PVEM, PT, MC, MORENA,CAND_IND1, OTROS )





dp21 = dp21 %>% 
  mutate(
    PAN = PAN + PAN_PRI_PRD + PAN_PRI + PAN_PRD + PRI_PRD,
    MORENA = MORENA+ PVEM_PT_MORENA + PVEM_PT + PVEM_MORENA + PT_MORENA,
    CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3,
    OTROS = PES+ RSP+FXM
  ) %>%  
  select(ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,EDO_MUN,ID_EDO,MUNICIPIO,PAN, PRI, PRD, PVEM, PT, MC,MORENA,CAND_IND1, OTROS )



# proporciones 

dp18_p =   prop.table(as.matrix(dp18[,-c(1,2,3,4,5,6)] ), margin = 1)*100
dp21[is.na(dp21)]=0
dp21_p =   prop.table(as.matrix(dp21[,-c(1,2,3,4,5,6)] ), margin = 1 )*100

VOLATILIDAD =  (rowSums(abs(dp18_p-dp21_p), na.rm = T)/2)

VOLATILIDAD20 = tibble("ID_ESTADO"= dp21$ID_ESTADO,"ID_MUNICIPIO"= dp21$ID_MUNICIPIO,"NOMBRE_ESTADO"=dp21$NOMBRE_ESTADO, "MUNICIPIO"= dp21$MUNICIPIO,
                       
                       "EDO_MUN"= dp21$EDO_MUN,"ID_EDO" = dp21$ID_EDO, "VOL"= VOLATILIDAD )



# juntamos bases de volatilidad y gini

VOLATIL18_21 = tibble("ID_ESTADO"= VOLATILIDAD20$ID_ESTADO,"ID_MUNICIPIO"= VOLATILIDAD20$ID_MUNICIPIO,"NOMBRE_ESTADO"=VOLATILIDAD20$NOMBRE_ESTADO,
                      "MUNICIPIO"= VOLATILIDAD20$MUNICIPIO,"EDO_MUN"= VOLATILIDAD20$EDO_MUN,
                      "ID_EDO" = VOLATILIDAD20$ID_EDO, "VOL"= VOLATILIDAD , "GINI" = GINI20$COEF)



VOLATIL18_21= VOLATIL18_21 %>% 
  filter(!EDO_MUN%in%c("OAXACA_REFORMA DE PINEDA_72",
                       "OAXACA_SANTA MARIA MIXTEQUILLA_422",
                       "OAXACA_SANTA MARIA XADANI_442",
                       "YUCATAN_SANAHCAT_64",
                       #no tienen coeficiente de GINI
                       "TLAXCALA_LA MAGDALENA TLALTELULCO_52",
                       "OAXACA_SANTA CATARINA JUQUILA_364"
                       
  )
  ) 
  
RESUMEN18_21 = summary(VOLATIL18_21[,c("VOL","GINI")])


VOLATIL18_21= VOLATIL18_21 %>% 
  mutate(G_VOL = c("VOL"),
         G_GINI =c("GINI"))

df1 =VOLATIL18_21$VOL
df2 =VOLATIL18_21$GINI
df3 =VOLATIL18_21$G_VOL
df4 =VOLATIL18_21$G_GINI

df=data.frame(
  rbind(
    cbind(df1,df3),
    cbind(df2,df4)
  )
)

colnames(df) =c("x","grupo")

df=df %>% mutate(x= as.numeric(as.character(x)))

library(ggplot2)

k_colors = c("#0000FF","#4682B4")

#layout_matrix <- matrix(1:3, ncol = 3)
#par(mfrow = c(1, 1))
# Especificar el diseño
#layout(layout_matrix)

ggplot(df, aes(x = x, fill = grupo)) + 
  geom_density (alpha = 0.5, position = "identity")+
  scale_fill_manual(values = k_colors, labels = c( "Coeficiente de Gini", "Volatilidad electoral ") )+
  guides(fill = guide_legend(title = " "))+
  labs(x = "Coeficientes" ,y = "Densidad" )+
  theme(legend.position = "bottom")


df %>%
  ggboxplot(x       = "grupo", 
            y       = "x",
            fill    = "grupo",
            palette = c("#2e00fa", "#4682B4"))+
  theme_bw()+
  labs(
    x = " ", y = "Coeficientes" )+
  theme(legend.position = "bottom")+ 
  scale_fill_manual(values = k_colors, labels = c("Volatilidad electoral ", "Coeficiente de Gini") )+
  guides(fill = guide_legend(title = " "))



# generar mapas

                 
shp_municipios <- st_read("DATA/shp/889463776079_s/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00mun.shp" )

mapa = read.xlsx("base_consolidada_para _claves_mapas_18_21.xlsx")

shp_municipios= inner_join(shp_municipios,mapa, by = "CVEGEO")

shp_municipios1 = shp_municipios %>%  
  select(CVEGEO,MUNICIPIO, GINI, geometry)

#gini
  tm_shape(shp_municipios1)+
  tm_fill("GINI", palette = "Blues", id = "MUNICIPIO", title = "Gini") +
  tm_borders("grey25", alpha = .20)+
  tm_layout(".", main.title.position = "") +
  tm_view(view.legend.position = c("left", "bottom"))

#VOLATILIDAD

mu = VOLATIL18_21 %>% 
  filter(VOL<=25 )


data.frame(table(VOLATIL18_21$NOMBRE_ESTADO))

data.frame(table(mu$NOMBRE_ESTADO))

mu = VOLATIL18_21 %>% 
  filter(VOL>=26 )
data.frame(table(mu$NOMBRE_ESTADO))



#GINI
mu = VOLATIL18_21 %>% 
  filter(GINI<=34 )


data.frame(table(VOLATIL18_21$NOMBRE_ESTADO))

data.frame(table(mu$NOMBRE_ESTADO))

mu = VOLATIL18_21 %>% 
  filter(GINI>=35 )
data.frame(table(mu$NOMBRE_ESTADO))


# volatilidad 

shp_municipios2 = shp_municipios %>%  
  select(CVEGEO,MUNICIPIO, VOL, geometry)


tm_shape(shp_municipios2)+
  tm_fill("VOL", palette = "Blues", id = "MUNICIPIO", title = "Volatiliad") +
  tm_borders("grey25", alpha = .20)+
  tm_layout(".",main.title.position = "") +
  tm_view(view.legend.position = c("left", "bottom"))


mu = VOLATIL15_18 %>% 
  filter(VOL>=60 )



RESUMEN15_18 = summary(VOLATIL15_18[,6:7])

mu = VOLATIL15_18 %>% 
  filter(VOL<=20 )

data.frame(table(mu$NOMBRE_ESTADO))
data.frame(table(VOLATIL15_18$NOMBRE_ESTADO))



mu = VOLATIL15_18 %>% 
  filter(VOL>=80 )

data.frame(table(mu$NOMBRE_ESTADO))

#
mu = VOLATIL15_18 %>% 
  filter(GINI<=30 )

data.frame(table(mu$NOMBRE_ESTADO))
data.frame(table(VOLATIL15_18$NOMBRE_ESTADO))



mu = VOLATIL15_18 %>% 
  filter(GINI>=50 )

data.frame(table(mu$NOMBRE_ESTADO))


#Analisando los cuatro periodos

pert = read.xlsx("graficos_descriptivos_todos_los_periodos.xlsx")

k_colors = c("#0000FF","#496cb7","#2c5bd0","#e2edff","#b1b1ff","#74a3ff")


ggplot(pert, aes(x = X, fill = Grupo)) + 
  geom_density (alpha = 0.5, position = "identity")+
  scale_fill_manual(values = k_colors, labels = c( "Coeficiente de Gini 2012 - 2015 ", 
                                                   "Coeficiente de Gini 2015 - 2018 ",
                                                   "Coeficiente de Gini 2018 - 2021 ",
                                                   "Volatilidad electoral 2012 - 2015 ",
                                                   "Volatilidad electoral 2015 - 2018",
                                                   "Volatilidad electoral 2018 - 2021"
                                                   ) )+
  guides(fill = guide_legend(title = " "))+
  labs(x = "Coeficientes" ,y = "Densidad" )+
  theme(legend.position = "bottom")




pert %>%
  ggboxplot(x       = "Grupo", 
            y       = "X",
            fill    = "Grupo",
            palette = c("#e2edff","#b1b1ff","#74a3ff","#0000FF","#496cb7","#2c5bd0" ))+
  theme_bw()+
  labs(
    x = " ", y = "Coeficientes" )+
  theme(legend.position = "bottom")+ 
  scale_fill_manual(values = k_colors, labels = c( "Volatilidad electoral 2012 - 2015 ",
                                                   "Volatilidad electoral 2015 - 2018",
                                                   "Volatilidad electoral 2018 - 2021" ,
    
                                                  "Coeficiente de Gini 2012 - 2015 ", 
                                                  "Coeficiente de Gini 2015 - 2018 ",
                                                  "Coeficiente de Gini 2018 - 2021 "
                                                  
                                                 ) )+
  guides(fill = guide_legend(title = " "))




ggplot(pert, aes(x = Grupo, y = X, fill = Grupo)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.04) +
  theme_bw()+
  scale_fill_manual(values = c("#2e00fa", "#4682B4","#0000FF","#4682B4","#0000FF","#4682B4" ))




#___________________________________________________modelo regresión líneal__________________________________________________####
#modelo Regresión 12_15 
modelo <- lm (GINI ~ VOL, data = VOLATIL12_15)
summary(modelo)

ggplot(data= VOLATIL12_15, aes(x= VOL, y= GINI))+
  geom_smooth(metodo = "lm")+
  geom_point()+
  ggtitle("Regresión lineal simple volatilidad electoral y coeficiente de GINI 2015")+
  stat_regline_equation(eitiqueta.x= 60, etiqueta.y= 15)


#####modelo Regresión 15_18 ####

modelo <- lm (GINI ~ VOL, data = VOLATIL15_18)
summary(modelo)

ggplot(data= VOLATIL15_18, aes(x= VOL, y= GINI))+
  geom_smooth(metodo = "lm")+
  geom_point()+
  ggtitle("Regresión lineal simple volatilidad electoral y coeficiente de GINI 2015")+
  stat_regline_equation(eitiqueta.x= 60, etiqueta.y= 15)


#####modelo Regresión 18_21 ####

modelo <- lm (GINI ~ VOL, data = VOLATIL18_21)
summary(modelo)

ggplot(data= VOLATIL18_21, aes(x= VOL, y= GINI))+
  geom_smooth(metodo = "lm")+
  geom_point()+
  ggtitle("Regresión lineal simple volatilidad electoral y coeficiente de GINI 2015")+
  stat_regline_equation(eitiqueta.x= 60, etiqueta.y= 15)

#____________________________________________ Analisis cluster___________________________________________________________####

#La distancia euclidiana en una sola dimensión se refiere a la separación entre dos puntos en un espacio unidimensional. 
#Aunque normalmente pensamos en espacios tridimensionales, en este caso consideraremos una línea recta como nuestro espacio.
#Aquí está cómo calcular la distancia euclidiana entre dos puntos A y B en una sola dimensión:
# Fórmula: La distancia euclidiana d(A, B) entre los puntos A y B se define como la raíz cuadrada del cuadrado de la 
#diferencia de sus coordenadas X: [ d(A, B) = \sqrt{(XB - XA)^2} ] Esta fórmula garantiza que:
#La distancia entre dos puntos siempre sea una cantidad positiva.

#La distancia entre A y B sea igual a la distancia entre B y A.
#Ejemplo: Supongamos que tenemos tres puntos en la recta unidimensional:
# Punto A con coordenada (XA = 2.5)
#Punto B con coordenada (XB = 4)
#Punto C con coordenada (XC = -2.5)
#Calculamos las distancias:
#(d(A, B) = \sqrt{(4 - 2.5)^2} = 1.5)
#(d(B, A) = \sqrt{(2.5 - 4)^2} = 1.5)
#(d(A, C) = \sqrt{(-2.5 - 2.5)^2} = 5.0)
#En resumen, la distancia entre dos puntos en una sola dimensión se calcula utilizando la fórmula mencionada y 
#representa la longitud del segmento de línea que conecta esos puntos

#UNA DISTANCIA EUCLIDEANA EN UN ESPACIO UNIDIMENSIONAL EN REALIDAD ES UNA DIFERENCIA ABSOLUTA.

#La distancia Euclídea se basa en el teorema de Pitágoras, según el cual,
#la hipotenusa al cuadrado es igual a la suma de catetos al cuadrado.


#install.packages("ggplot2", dependencies = T)
#install.packages("factoextra", dependencies = T)

library(ggplot2)
library(factoextra)
library(cluster)

##### distancias Euclidean entre a y b####

# esta función calcula la distancia enetre el un plano unidimencional

X = c(1,   2,  4,  5,  4)
Y = c(2,   3,  1,  4,  5)

euclidean_distance = function(a, b){
  a1 = as.matrix(a)
  b1 = as.matrix(b)
  euclidean <- numeric(nrow(a1))
  # Comprobamos que tienen la misma cantidad de observaciones
  if(length(a) == length(b)){
    for (i in 1:nrow(a1)) {
      euclidean[i] <- sqrt(sum((a1[i,1]- b1[i,1] )^2))
    } 
    return(euclidean)
  }
}


df1=euclidean_distance(X,Y)



# Calcular las distancias entre cada par de puntos

dist_eu <- function(bd,ID) {
  
  #bd_co <- bd[, 2:3]
  
  bd_co <- bd[,c("VOL","GINI")]
  dista <- matrix(NA, nrow = nrow(bd), ncol = nrow(bd))
  
  rownames(dista) <- ID
  colnames(dista) <- ID
  
  for (i in 1:nrow(bd_co)) {
    for (j in 1:nrow(bd_co)) {
      dista[i, j] <-  sqrt((bd_co[i,1]-bd_co[j,1])^2 + (bd_co[i,2]-bd_co[j,2])^2)
    }
  }
  return(dista)
}

df_eu=dist_eu(VOLATIL18_21,VOLATIL18_21$EDO_MUN)

sqrt(
(VOLATIL18_21$VOL[1]-VOLATIL18_21$VOL[2])^2+
(VOLATIL18_21$GINI[1]-VOLATIL18_21$GINI[2])^2
)


#guardar distancias euclidianas
#write.csv(df_eu,"distancias_euclidianas_18_21.csv")


# distancias euclideanas con matrices 
# al multimpolicar la transpuesta por la matriz se obitiene una matriz identidad
# propiedades de matricez
x= as.matrix(VOLATIL18_21[1:10,c("VOL","GINI")])
z <- x %*% t(x)

df_euq2=sqrt(diag(z)+ t(diag(z)-2*z ) )

# distancias euclideas con función dist
df_euq = dist(VOLATIL18_21[,c("VOL","GINI")], method = "euclidean")

write.csv(df_euq,"distancias_euclidianas_18_21.csv")

#calcular la matriz de distacias con get_dist


# CARGAMOS LIBRERIAS 
library(ggplot2)
#install.packages("factoextra")
library(factoextra)


VOLATIL18_21 = as.data.frame(VOLATIL18_21)

rownames(VOLATIL18_21)=VOLATIL18_21$EDO_MUN

m.distancia <- get_dist(VOLATIL18_21[,c("VOL","GINI")], method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"

fviz_dist(m.distancia, gradient = list(low = "#87CEEB", mid = "white", high = "#0000FF"), lab_size = 5)



#________________________________________________Cluster jerarquico_________________________________________________________####
#obtenemos la matriz de distancias escaladas

df_euq = dist(scale(VOLATIL18_21[,c("VOL","GINI")] ), method = "euclidean")
# creamos el objeto hclust con el metodo ward
#jera1= hclust(df_euq, method = "ward.D2")

jera2= hcut(df_euq,k = 3,stand = T,hc_method = "ward.D2")
# graficamos 1
fviz_dend(jera2, rect = TRUE, cex = 0.1, 
          main = "Dendrograma - ward", xlab = "Municipios", ylab = "Distancias",
          k_colors = c("#0000FF","#87CEEB" ,"#4682B4") )


VOLATIL18_21 = cbind(VOLATIL18_21, data.frame(jera2$cluster))

write.csv(VOLATIL18_21,"base_con_numero_de_clusters_18_21.csv")
#distancias euclidianas por gurpo o cluster

dist_clus3 = VOLATIL18_21 %>% 
  filter(jera2.cluster==3)

df_eu=dist_eu(dist_clus3[,c("VOL","GINI")],dist_clus3$EDO_MUN)

write.csv(df_eu,"distancias_cluster3_18_21.csv")



#______________________________________coeficiente de siluetas____________________________________________________________________####d
#para iniciar un kecnter con un numero de cluster fijo, tomaresmos el número de clusters 
#datos por el dendograma
kk = kmeans(VOLATIL18_21[,c("VOL","GINI" )],3)

VOLATIL18_21 = cbind(VOLATIL18_21,kk$cluster )

VOLATIL18_21=VOLATIL18_21 %>% 
  mutate(clust = ifelse(kk$cluster%in%"1","Alta",
                        ifelse(kk$cluster%in%"2","Baja",
                               ifelse(kk$cluster%in%"3","Media",NA))))
                      
  

write.csv(VOLATIL18_21,"cluster3_kmedias_18_21.csv")

#resumen cluster1
dist_clus1 = VOLATIL18_21 %>% 
  filter(kk$cluster==1)
res_k1 = summary(dist_clus1[,c("VOL","GINI" )] )

#resumen cluster 2
dist_clus2 = VOLATIL18_21 %>% 
  filter(kk$cluster==2)
res_k2 = summary(dist_clus2[,c("VOL","GINI" )] )
#resumen cluster 3
dist_clus3 = VOLATIL18_21 %>% 
  filter(kk$cluster==3)
res_k3 = summary(dist_clus3[,c("VOL","GINI" )] )

df=data.frame( dist(dist_clus[,c("VOL","GINI")], method = "euclidean"))
mean(df$dist.dist_clus...c..VOL....GINI.....method....euclidean..,na.rm = T)
write.csv(df,"cluster3_kmedias_18_21.csv")

df_eu=dist_eu(dist_clus[,c("VOL","GINI")],dist_clus$EDO_MUN)
mean(df)

write.csv(df_eu,"distancias_cluster3_18_21.csv")



s= silhouette(kk$cluster,dist(VOLATIL18_21[,c("VOL","GINI" )]) )

plot(s)

fviz_nbclust(VOLATIL18_21[,c("VOL","GINI" )], kmeans , method = "silhouette")
df=as.data.frame(s)

# Define tus colores personalizados
k_colors = c("#0000FF","#87CEEB" ,"#4682B4")

# Crear un gráfico de barras con colores personalizados
ggplot(df, aes(x = grupo, y = valor, fill = grupo)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values =k_colors )


# Create a grouped horizontal bar plot
nombres_personalizados <- c("2: 983, 0.40","3: 297, 0.46","1: 1179,0.50")
ggp =ggplot(df,aes(x=sil_width,y= reorder(sil_width, cluster),fill = as.factor(cluster)) )  +
  geom_bar(stat = "identity")+
  scale_fill_manual(values =c("#4682B4","#87CEEB","#0000FF"),
                    labels = nombres_personalizados
                    )+
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x = "Silhouette coefficient", y = "Tres clúster",fill = 'Silhouette coefficient por clúster') 



#_________________________________________________analisís cluster________________________________________________________####


# Número óptimo de cluster por diferesntes métodos

#introducción clustering
#crear grupos relaciones multivaraintes
#buscar patrones entre ellos
# similaridad entre las filas, es decir, cuanto se parecen pares de filas, llamadas distancias.

### Distancia Euclideana: Variables numéricas
#install.packages("NbClust", dependencies = T)
#library(NbClust)

NbClust(m.distancia, min.nc=2, max.nc=15, method="kmeans")

#estimar el número de clústers
#Elbow, silhouette o gap_stat  methhttp://127.0.0.1:22251/graphics/plot_zoom_png?width=1200&height=322od
fviz_nbclust(VOLATIL18_21[,c("VOL","GINI" )], kmeans, method = "wss")
fviz_nbclust(VOLATIL18_21[,c("VOL","GINI" )], kmeans, method = "silhouette")
fviz_nbclust(VOLATIL18_21[,c("VOL","GINI" )], kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
#resnumclust<-NbClust(VOLATIL18_21[,c("VOL","GINI")], distance = "euclidean", min.nc=3, max.nc=10, method = "kmeans", index = "alllong")

#fviz_nbclust(resnumclust)

#VOLATIL18_21= as.data.frame(VOLATIL18_21)

rownames(VOLATIL12_15) <- VOLATIL12_15$ID_MUN2
rownames(VOLATIL15_18) <- VOLATIL15_18$EDO_MUN
rownames(VOLATIL18_21) <- VOLATIL18_21$EDO_MUN

k0 <- kmeans(VOLATIL12_15[,c("VOL","GINI")], centers = 3,nstart = 25)
k1 <- kmeans(VOLATIL15_18[,c("VOL","GINI")], centers = 3,nstart = 25)


k2 <- kmeans(VOLATIL18_21[,c("VOL","GINI")], centers = 3,nstart = 25)
k2

VOLATIL12_15 = cbind(VOLATIL12_15,k0$cluster )
VOLATIL15_18 = cbind(VOLATIL15_18,k1$cluster )


VOLATIL18_21 = cbind(VOLATIL18_21,k2$cluster )

VOLATIL18_21_df = tibble(VOLATIL18_21,as.data.frame(k2$cluster ))

VOL18_21=VOLATIL18_21[,c("ID_ESTADO","ID_MUNICIPIO","NOMBRE_ESTADO","MUNICIPIO",
                "VOL","GINI","PER","k2$cluster" )]

VOL15_18= VOLATIL15_18[,c("ID_ESTADO","ID_MUNICIPIO","NOMBRE_ESTADO","MUNICIPIO",
                "VOL","GINI","PER","k1$cluster" )]

VOL12_15= VOLATIL12_15[,c("ID_ESTADO","ID_MUNICIPIO","NOMBRE_ESTADO","MUNICIPIO",
                "VOL","GINI","PER","k0$cluster" )]

  
names(VOL18_21)=names(VOL15_18) = names(VOL12_15)

cf = rbind(VOL18_21,VOL15_18,VOL12_15
           
           
           )

write.csv(cf,"volatilidad-final_clusters_tres_periodos.csv")




#resumen cluster1
dist_clus1 = VOLATIL18_21 %>% 
  filter(k2$cluster==1)
res_k1 = summary(dist_clus1[,c("VOL","GINI" )] )
summary(VOLATIL18_21[,c("VOL","GINI" )] )


write.csv(VOLATIL18_21,"volatilidad-final_clusters.csv")

df1=read.xlsx("cluster1_grande_box.xlsx")

df1 %>%
  ggboxplot(x       = "Grupo", 
            y       = "X",
            fill    = "Grupo",
            palette = c("#2e00fa", "#4682B4"))+
            labs(x = "Variables", y = "Coeficientes" )



#resumen cluster 2
dist_clus2 = VOLATIL18_21 %>% 
  filter(k2$cluster==2)
res_k2 = summary(dist_clus2[,c("VOL","GINI" )] )
write.csv(dist_clus2,"cluster2_18_21_v2.csv")

df=data.frame(table(dist_clus2$NOMBRE_ESTADO))



#resumen cluster 3
dist_clus3 = VOLATIL18_21 %>% 
  filter(k2$cluster==3)
  res_k3 = summary(dist_clus3[,c("VOL","GINI" )] )
  
  df=data.frame(table(dist_clus3$NOMBRE_ESTADO))

write.csv(dist_clus3,"cluster3_18_21_v2.csv")

df1=read.xlsx("box_plot3.xlsx")

df1 %>%
  ggboxplot(x       = "Grupo", 
            y       = "X",
            fill    = "Grupo",
            palette = c("#2e00fa", "#4682B4"))+
  labs(x = "Variables", y = "Coeficientes" )

df1=read.xlsx("tres_box_plot.xlsx")

#tres clusters
df1 %>%
  ggboxplot(x       = "Grupo", 
            y       = "X",
            fill    = "Grupo",
            palette = c("#2e00fa", "#87CEEB","#2e00fa", "#87CEEB","#2e00fa", "#87CEEB"))+
  labs(x = "Variables", y = "Coeficientes" )


#plotear los cluster
fviz_cluster(k2, data = VOLATIL18_21[,c("VOL","GINI")] )+
  scale_color_manual(values = c("#4682B4","#87CEEB","#0000FF","#4682B4","#87CEEB") )+ 
  theme_minimal()
  

VOLATIL18_21_rown = as.data.frame(VOLATIL18_21)
rownames(VOLATIL18_21_rown)=VOLATIL18_21$EDO_MUN

fviz_cluster(k2, data = VOLATIL18_21[,c("VOL","GINI")] , ellipse.type = "euclid",repel = TRUE,star.plot = TRUE)+ #ellipse.type= "t", "norm", "euclid"
  scale_color_manual(values = c("#4682B4","#87CEEB","#0000FF") )+ 
  theme_minimal()


fviz_cluster(k2, data = VOLATIL18_21[,c("VOL","GINI")], ellipse.type = "norm")+
  scale_color_manual(values = c("#4682B4","#87CEEB","#0000FF") )+ 
  theme_minimal()

fviz_cluster(k2, data = VOLATIL18_21[,c("VOL","GINI")], ellipse.type = "norm",
             palette = "Set2", ggtheme = theme_minimal())+
  scale_color_manual(values = c("#4682B4","#87CEEB","#0000FF") )+ 
  theme_minimal()

res2 <- hcut(VOLATIL18_21[,c("VOL","GINI")], k = 3, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green" ))
#pasar los cluster a mi df inicial para trabajar con ellos

cls = df6 %>%
  mutate(Cluster = k2$cluster)%>% 
  group_by(Cluster) %>%
  summarise_all(list("min" = min, "max" = max, media = "mean"))

num_clus_grup2015 = df6 %>% 
  mutate(Cluster = k2$cluster) %>% 
  group_by(Cluster) %>% 
  count()


write.csv(cls,"clousters.csv")



n()

################# analisis clust para volatilidad
library(cluster)


clusters = data.frame(column_to_rownames(VOLATIL18_21, "X"))

#calcular la matriz de distacias

m.distancia <- get_dist(VOLATIL18_21[,c("")], method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"

library(NbClust)
NbClust(df6, min.nc=2, max.nc=15, method="kmeans")


k2 <- kmeans(df6, centers = 5, nstart = 25)
k2
str(k2)

#clusplot(clara(df6, 5))

#pasar los cluster a mi df inicial para trabajar con ellos

cls = df6 %>%
  mutate(Cluster = k2$cluster)%>% 
  group_by(Cluster) %>%
  summarise_all(list("min" = min, "max" = max, media = "mean"))

num_clus_grup = df6 %>% 
  mutate(Cluster = k2$cluster) %>% 
  group_by(Cluster) %>% 
  count()


write.csv(cls,"clousters.csv")






#semilla para iniciar
set.seed(12345)
bd = data.frame(matrix(rnorm(1000),100,10))

kcenter = function(bd,k=2,distancia= "euclidean", center = "median"){
  k=6;distancia="euclidean";centro="media";semilla=12345
  nf<-nrow(bd)
  nc<-ncol(bd)
  #bd$k<-rep(seq(1:3),ceiling(nf/k))[1:nf]
  set.seed(semilla)
  bd$k<-c(1:k,sample(1:k,nf-k,replace = T))
  
  
  if(centro=="media"){
    centros<-bd %>% group_by(k) %>% summarise_at(vars(1:nc),mean)
  } else if(centro=="mediana"){
    centros<-bd %>% group_by(k) %>% summarise_at(vars(1:nc),median)
  }
  
  
  