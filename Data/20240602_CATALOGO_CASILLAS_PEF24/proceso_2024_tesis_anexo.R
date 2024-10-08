# Cargar librerias

library(tmap) #Dibujar el mapa
library(sf) #Para leer el shapefile y reducir el tamaño del archivo
library(pryr) #Calcular el tamaño del archivo
library(readr) #para cargar csv
library(dplyr) # para inner join
library(openxlsx) # Permite crear, leer y escribir archivos Excel (.xlsx)
library(foreign) # Permite importar y exportar datos de y hacia otros formatos estadísticos como SPSS, Stata y SAS4
library(survey) # Proporciona herramientas para el análisis de datos de encuestas complejas.
library(dplyr) # Parte del tidyverse, se usa para manipulación de datos, ofreciendo una gramática coherente y eficiente.
library(tidyverse) #Un conjunto de paquetes que comparten una filosofía de diseño y gramática para manipulación y visualización de datos.
library(dplyr) # manipulación eficiente de datos.
library(ggpubr) # Extiende ggplot2 para facilitar la creación de gráficos de publicación
library(ggplot2) # Parte del tidyverse, es una poderosa herramienta para la creación de gráficos basados en la gramática de gráficos.
library(cluster) #  Proporciona métodos para el análisis de agrupamiento (clustering).
library(factoextra) # Herramientas para la visualización y la interpretación de análisis de datos multivariados, incluyendo clustering.
library(NbClust) # Determina el número óptimo de clusters en un conjunto de datos.
library(tidyr) #

#____________________________________________Proceso 2021 a 2024 _____________________________________________________####

#Cargamos bases de datos
#Nota:
#Las bases de datos disponibles para las elecciones federales 2024 PREP,
#están disponibles pero sin información desagregada por municipios,
#por lo que fue necesario un preprocesamiento y poder obtener los resultados a nivel municipal.

dp24 = read.csv("DATA/20240603_2005_PREP/20240603_2005_PREP_DIP_FED/DIP_FED_2024.csv") #Cargar bases de datos PREP 2024 por distito
dp24_catalogo = read.xlsx("DATA/20240602_CATALOGO_CASILLAS_PEF24/20240503_CORTE_CASILLAS_PEF24.xlsx") # Catalogo de casillas para poder identificar los municipios.
dp21 = read.csv("DATA/DatosAbiertos-2021.csv") # Base de datos elección federal 2021.
GINI20 = read.xlsx("DATA/GINI_2020.xlsx") # Coeficiente de Gini 2020.

#limpieza y preparación de base de datos a nivel distrito
dp24_seccion = dp24 %>%
  # Seleccionamos variables de interés
  select(ID_ENTIDAD,ENTIDAD,ID_DISTRITO_FEDERAL,DISTRITO_FEDERAL,
                              SECCION,TOTAL_PERSONAS_VOTARON,
                              TOTAL_VOTOS_SACADOS,          PAN,                          PRI,
                              PRD,                         PVEM,                         PT,
                              MC,                           MORENA,                       CI_01,
                              CI_02,                       PAN.PRI.PRD,                  PAN.PRI,
                              PAN.PRD,                      PRI.PRD,                      PVEM_PT_MORENA,
                              PVEM_PT,                      PVEM_MORENA,                  PT_MORENA,
                              TOTAL_VOTOS_ASENTADO, NO_REGISTRADAS) %>%
  #Preprocesamiento de los datos
  mutate(
        ID_MUN = paste(ID_ENTIDAD,as.numeric(as.character(SECCION)),sep = "_"),
         ID_ENTIDAD = as.numeric(as.character(ID_ENTIDAD)),
         ID_DISTRITO_FEDERAL = as.numeric(as.character(ID_DISTRITO_FEDERAL)),
         TOTAL_VOTOS_SACADOS = as.numeric(ifelse(TOTAL_VOTOS_SACADOS%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(TOTAL_VOTOS_SACADOS),0,TOTAL_VOTOS_SACADOS)),
         TOTAL_PERSONAS_VOTARON = as.numeric(ifelse(TOTAL_PERSONAS_VOTARON%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(TOTAL_PERSONAS_VOTARON),0,TOTAL_PERSONAS_VOTARON)),
         PAN = as.numeric(ifelse(PAN%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PAN),0,PAN)),
         PRI = as.numeric(ifelse(PRI%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PRI),0,PRI)),
         PRD = as.numeric(ifelse(PRD%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PRD),0,PRD)),
         PVEM = as.numeric(ifelse(PVEM%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PVEM),0,PVEM)),
         PT = as.numeric(ifelse(PT%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PT),0,PT)),
         MC = as.numeric(ifelse(MC%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(MC),0,MC)),
         MORENA = as.numeric(ifelse(MORENA%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(MORENA),0,MORENA)),
         CI_01 = as.numeric(ifelse(CI_01%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(CI_01),0,CI_01)),
         CI_02 = as.numeric(ifelse(CI_02%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(CI_02),0,CI_02)),
         PAN.PRI.PRD = as.numeric(ifelse(PAN.PRI.PRD%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PAN.PRI.PRD),0,PAN.PRI.PRD)),
         PAN.PRI = as.numeric(ifelse(PAN.PRI%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PAN.PRI),0,PAN.PRI)),
         PAN.PRD = as.numeric(ifelse(PAN.PRD%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PAN.PRD),0,PAN.PRD)),
         PRI.PRD = as.numeric(ifelse(PRI.PRD%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na( PRI.PRD),0, PRI.PRD)),
         PVEM_PT_MORENA = as.numeric(ifelse(PVEM_PT_MORENA%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na( PVEM_PT_MORENA),0, PVEM_PT_MORENA)),
         PVEM_PT = as.numeric(ifelse(PVEM_PT%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na( PVEM_PT),0, PVEM_PT)),
         PVEM_MORENA = as.numeric(ifelse(PVEM_MORENA%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na( PVEM_MORENA),0, PVEM_MORENA)),
         PT_MORENA = as.numeric(ifelse(PT_MORENA%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(PT_MORENA),0,PT_MORENA)),
         NO_REGISTRADAS = as.numeric(ifelse(NO_REGISTRADAS%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(NO_REGISTRADAS),0,NO_REGISTRADAS)),
         TOTAL_VOTOS_ASENTADO = as.numeric(ifelse(TOTAL_VOTOS_ASENTADO%in%c("Ilegible","Sin acta ","Sin dato","N/A") |is.na(TOTAL_VOTOS_ASENTADO),0,TOTAL_VOTOS_ASENTADO))
  ) %>%
  # Agrupamos los datos por ID_MUN
  group_by(ID_MUN) %>%
  summarise(
    "TOTAL_VOTOS_SACADOS_SM" = sum(TOTAL_VOTOS_SACADOS, na.rm = T),
    "PAN_SM" = sum(PAN, na.rm = T),
    "PRI_SM" = sum(PRI, na.rm = T),
    "PRD_SM" = sum(PRD, na.rm = T),
    "PVEM_SM" = sum(PVEM, na.rm = T),
    "PT_SM" = sum(PT, na.rm = T),
    "MC_SM" = sum(MC, na.rm = T),
    "MORENA_SM" = sum(MORENA, na.rm = T),
    "CI_01_SM" = sum(CI_01, na.rm = T),
    "CI_02_SM" = sum(CI_02, na.rm = T),
    "PAN.PRI.PRD_SM" = sum(PAN.PRI.PRD, na.rm = T),
    "PAN.PRI_SM" = sum(PAN.PRI, na.rm = T),
    "PAN.PRD_SM" = sum(PAN.PRD, na.rm = T),
    "PRI.PRD_SM" = sum(PRI.PRD, na.rm = T),
    "PVEM_PT_MORENA_SM" = sum(PVEM_PT_MORENA, na.rm = T),
    "PVEM_PT_SM" = sum(PVEM_PT, na.rm = T),
    "PVEM_MORENA_SM" = sum(PVEM_MORENA, na.rm = T),
    "PT_MORENA_SM" = sum(PT_MORENA, na.rm = T),
    "PVEM_PT_SM" = sum(PVEM_PT, na.rm = T),
    "NO_REGISTRADAS_SM" = sum(NO_REGISTRADAS, na.rm = T),
    "TOTAL_VOTOS_ASENTADO_SM" = sum(TOTAL_VOTOS_ASENTADO, na.rm = T),
    "ENTIDAD_SM" = first(ENTIDAD),
    "SECCION_SM" = first(SECCION),
    "ID_ENTIDAD_SM" = first(ID_ENTIDAD)
  ) %>%
  mutate(TOTAL_VOTOS_SACADOS = TOTAL_VOTOS_SACADOS_SM,
         PAN = PAN_SM,
         PRI = PRI_SM,
         PRD = PRD_SM,
         PVEM = PVEM_SM,
         PT = PT_SM,
         MC = MC_SM,
         MORENA = MORENA_SM,
         CI_01 = CI_01_SM,
         CI_02 = CI_02_SM,
         PAN.PRI.PRD = PAN.PRI.PRD_SM,
         PAN.PRI = PAN.PRI_SM,
         PAN.PRD = PAN.PRD_SM,
         PRI.PRD = PRI.PRD_SM,
         PVEM_PT_MORENA = PVEM_PT_MORENA_SM,
         PVEM_PT = PVEM_PT_SM,
         PVEM_MORENA = PVEM_MORENA_SM,
         PT_MORENA = PT_MORENA_SM,
         PVEM_PT = PVEM_PT_SM,
         NO_REGISTRADAS = NO_REGISTRADAS_SM,
         TOTAL_VOTOS_ASENTADO = TOTAL_VOTOS_ASENTADO_SM,
         ENTIDAD =ENTIDAD_SM,
         SECCION = SECCION_SM,
         ID_ENTIDAD = ID_ENTIDAD_SM
  ) %>%
  select(ID_ENTIDAD ,ID_MUN,ENTIDAD,SECCION,
         TOTAL_VOTOS_SACADOS ,
         PAN,
         PRI,
         PRD,
         PVEM,
         PT,
         MC,
         MORENA,
         CI_01 ,
         CI_02 ,
         PAN.PRI.PRD ,
         PAN.PRI ,
         PAN.PRD ,
         PRI.PRD ,
         PVEM_PT_MORENA ,
         PVEM_PT,
         PVEM_MORENA ,
         PT_MORENA ,
         PVEM_PT ,
         NO_REGISTRADAS ,
         TOTAL_VOTOS_ASENTADO
  )

# limpieza y acomodo de directorio de secciones para pegar municipios
dp24_direc = dp24_catalogo %>%select(
  ESTADO, ESTADO_DESCRIPCION,MUNICIPIO,NOMBRE_MUNICIPIO,SECCION
) %>%
  mutate(ID_MUN = paste(ESTADO,SECCION,sep = "_")
  ) %>%
  group_by(ID_MUN) %>%
  summarise(
    "ESTADO_SM" = mean(ESTADO, na.rm = T),
    "MUNICIPIO_SM" = mean(MUNICIPIO, na.rm = T) ,
    "ESTADO_NOM" = first(ESTADO_DESCRIPCION),
    "MUNICIPIO_NOM" = first(NOMBRE_MUNICIPIO),
    "SECCION_SE" = first(SECCION)
  ) %>%
  mutate(
    ESTADO = ESTADO_SM,
    MUNICIPIO = MUNICIPIO_SM,
    SECCION=SECCION_SE) %>%
  select(ID_MUN,SECCION,ESTADO,ESTADO_NOM,MUNICIPIO,MUNICIPIO_NOM)

# ordenar base de datos de directorio por “ID_MUN”
dp24_direc = dp24_direc[order(dp24_direc$ID_MUN),]

# ordenar base de datos de secciones por “ID_MUN”
dp24_seccion = dp24_seccion[order(dp24_seccion$ID_MUN),]

# pegando los directorios a la base de secciones 
dp24_df=merge(dp24_direc,dp24_seccion, by ="ID_MUN",all.y=T,sort = F)

# Agrupamos por municipio 
dp24= dp24_df %>% mutate(ID_MUN_ENT = paste(ESTADO,MUNICIPIO,MUNICIPIO_NOM,sep = "_")) %>%
  group_by(ID_MUN_ENT) %>%
  summarise(
    "TOTAL_VOTOS_SACADOS_SM" = sum(TOTAL_VOTOS_SACADOS, na.rm = T),
    "PAN_SM" = sum(PAN, na.rm = T),
    "PRI_SM" = sum(PRI, na.rm = T),
    "PRD_SM" = sum(PRD, na.rm = T),
    "PVEM_SM" = sum(PVEM, na.rm = T),
    "PT_SM" = sum(PT, na.rm = T),
    "MC_SM" = sum(MC, na.rm = T),
    "MORENA_SM" = sum(MORENA, na.rm = T),
    "CI_01_SM" = sum(CI_01, na.rm = T),
    "CI_02_SM" = sum(CI_02, na.rm = T),
    "PAN.PRI.PRD_SM" = sum(PAN.PRI.PRD, na.rm = T),
    "PAN.PRI_SM" = sum(PAN.PRI, na.rm = T),
    "PAN.PRD_SM" = sum(PAN.PRD, na.rm = T),
    "PRI.PRD_SM" = sum(PRI.PRD, na.rm = T),
    "PVEM_PT_MORENA_SM" = sum(PVEM_PT_MORENA, na.rm = T),
    "PVEM_PT_SM" = sum(PVEM_PT, na.rm = T),
    "PVEM_MORENA_SM" = sum(PVEM_MORENA, na.rm = T),
    "PT_MORENA_SM" = sum(PT_MORENA, na.rm = T),
    "PVEM_PT_SM" = sum(PVEM_PT, na.rm = T),
    "NO_REGISTRADAS_SM" = sum(NO_REGISTRADAS, na.rm = T),
    "TOTAL_VOTOS_ASENTADO_SM" = sum(TOTAL_VOTOS_ASENTADO, na.rm = T),
    "ENTIDAD_SM" = first(ENTIDAD),
    "ESTADO_NOM_SM" = first(ESTADO_NOM),
    "MUNICIPIO_SM" = first(MUNICIPIO),
    "MUNICIPIO_NOM_SM" = first(MUNICIPIO_NOM),
    "ID_MUN_SM" = first(ID_MUN),
    "SECCION_SM" = first(SECCION.x),
    "ID_ENTIDAD_SM" = first(ID_ENTIDAD)
  ) %>%
  mutate(TOTAL_VOTOS_SACADOS = TOTAL_VOTOS_SACADOS_SM,
         PAN = PAN_SM,
         PRI = PRI_SM,
         PRD = PRD_SM,
         PVEM = PVEM_SM,
         PT = PT_SM,
         MC = MC_SM,
         MORENA = MORENA_SM,
         CI_01 = CI_01_SM,
         CI_02 = CI_02_SM,
         PAN.PRI.PRD = PAN.PRI.PRD_SM,
         PAN.PRI = PAN.PRI_SM,
         PAN.PRD = PAN.PRD_SM,
         PRI.PRD = PRI.PRD_SM,
         PVEM_PT_MORENA = PVEM_PT_MORENA_SM,
         PVEM_PT = PVEM_PT_SM,
         PVEM_MORENA = PVEM_MORENA_SM,
         PT_MORENA = PT_MORENA_SM,
         PVEM_PT = PVEM_PT_SM,
         NO_REGISTRADAS = NO_REGISTRADAS_SM,
         TOTAL_VOTOS_ASENTADO = TOTAL_VOTOS_ASENTADO_SM,
         ENTIDAD = ENTIDAD_SM,
         ENTIDAD_2 = ENTIDAD,
         ESTADO_NOM = ESTADO_NOM_SM,
         MUNICIPIO = MUNICIPIO_SM,
         MUNICIPIO_NOM = MUNICIPIO_NOM_SM,
         ID_MUN = ID_MUN_SM,
         SECCION = SECCION_SM,
         ID_ENTIDAD = ID_ENTIDAD_SM
  ) %>%
  select(ID_ENTIDAD,ID_MUN,ID_MUN_ENT,
         ENTIDAD_2,
         ESTADO_NOM,
         ENTIDAD,
         MUNICIPIO,
         MUNICIPIO_NOM,
         SECCION,
         TOTAL_VOTOS_SACADOS ,
         PAN,
         PRI,
         PRD,
         PVEM,
         PT,
         MC,
         MORENA,
         CI_01 ,
         CI_02 ,
         PAN.PRI.PRD ,
         PAN.PRI ,
         PAN.PRD ,
         PRI.PRD ,
         PVEM_PT_MORENA ,
         PVEM_PT,
         PVEM_MORENA ,
         PT_MORENA ,
         PVEM_PT ,
         NO_REGISTRADAS ,
         TOTAL_VOTOS_ASENTADO
  )%>%
  mutate(ID_ORDER = paste(ENTIDAD,MUNICIPIO_NOM,sep = "_"))


# Preparamos variables para análisis volatilidad 
# Filtramos municipios que no están en el ejercicio electoral 2021.

dp24 = dp24 %>%
  mutate(PAN = PAN+ PAN.PRI.PRD+ PAN.PRI+ PAN.PRD,
         PRI = + PRI.PRD,
         MORENA = MORENA+ PVEM_PT_MORENA+ PVEM_MORENA+ PT_MORENA,
         PVEM= PVEM+PVEM_PT,
         CAND_IND = CI_01+ CI_02,
         OTROS = NO_REGISTRADAS
  ) %>%
  select(ID_ENTIDAD,ID_ORDER,ID_MUN,ID_MUN_ENT,ENTIDAD,MUNICIPIO,MUNICIPIO_NOM,
         PAN,PRI,PRD,PVEM,PT,MC,MORENA,CAND_IND,OTROS,
         -c(PAN.PRI.PRD,PAN.PRI,PAN.PRD,PRI.PRD,PVEM_PT_MORENA,PVEM_MORENA,
            PT_MORENA,PVEM_PT,ESTADO_NOM,ENTIDAD_2,CI_01,CI_02,SECCION,TOTAL_VOTOS_SACADOS,
            TOTAL_VOTOS_ASENTADO)) %>%
  arrange(ID_ORDER) %>%
  filter(!ID_MUN%in%c("2_193" ,"2_149","4_359","7_1194","7_2348","7_1189",
                      "7_1786","12_2069","12_1014","12_1716","12_695",
                      "16_2002","19_2212","20_1297","20_2417","25_1618",
                      "25_2085","28_311","32_NA"),
         !is.na(MUNICIPIO))


#Proceso electoral 2021 

#Creamos y filtros por id 

dp21 = dp21 %>% mutate(
  EDO_MUN = paste(NOMBRE_ESTADO, MUNICIPIO,ID_MUNICIPIO, sep = "_"),
  ID_EDO =paste(ID_ESTADO, ID_MUNICIPIO, sep = "_") ,
  ID_ORDER = paste(NOMBRE_ESTADO,MUNICIPIO, sep = "_")) %>%
  arrange(ID_ORDER)

# Preparamos variables para análisis volatilidad 
# Filtramos municipios que no están en el ejercicio electoral 2024.

dp21 = dp21 %>%
  mutate(
    PAN = PAN + PAN_PRI_PRD + PAN_PRI + PAN_PRD + PRI_PRD,
    MORENA = MORENA+ PVEM_PT_MORENA + PVEM_PT + PVEM_MORENA + PT_MORENA,
    CAND_IND = CAND_IND1+CAND_IND2+CAND_IND3,
    OTROS = PES+ RSP+FXM
  ) %>%
  select(ID_ORDER,ID_EDO,ID_ESTADO,ID_MUNICIPIO,NOMBRE_ESTADO,MUNICIPIO,EDO_MUN,ID_EDO,MUNICIPIO,PAN,
         PRI, PRD, PVEM, PT, MC,MORENA,CAND_IND1, OTROS ) %>%
  filter(!ID_EDO%in%c("7_66","7_30","20_562","16_92","4_11","28_13"))

# proceso para calcular coeficiente de volatilidad
# Proporciones  2021
dp21[is.na(dp21)]=0
dp21_p =   prop.table(as.matrix(dp21[,8:16] ), margin = 1 )*100

# Proporciones  2024
dp24[is.na(dp24)]=0
dp24_p =   prop.table(as.matrix(dp24[,8:16 ]), margin = 1 )*100

# Coeficiente de Pedersen 
VOLATILIDAD = data.frame("VOL"= rowSums(abs(dp21_p-dp24_p), na.rm = T)/2)

# Armamos data frame 
VOLATILIDAD24 = tibble("CVEGEO" = dp24$ID_ENTIDAD,
                       "ID_MUNICIPIO" = dp21$ID_MUNICIPIO,
                       "EDO_MUN"= dp24$ID_ORDER,
                       "ID_EDO" = dp24$ID_MUN,
                       "NOMBRE_ESTADO"=dp24$ENTIDAD,
                       "MUNICIPIO"= dp24$MUNICIPIO_NOM,
                       "VOL"=  VOLATILIDAD$VOL )


# Filtramos municipios que tengan un coeficiente mayor a 0 

VOLATILIDAD24=VOLATILIDAD24 %>%
  filter(!VOL==0,
         !ID_EDO%in%c("4_173","4_320","17_564", "17_699","17_594","20_2465") )

# Filtramos base de datos coeficientes de Gini para que coincidan los municipios-
#con elecciones federales en 2024. 

GINI20 = GINI20[order(GINI20$EDO_MUN),]

GINI20 = GINI20 %>% filter(!ID_EDO%in%c(
  "4_11", "7_119" ,"7_30" ,"7_33" ,"7_66","7_120",
  "16_5" ,"16_62" ,"16_92","20_559","20_73","20_72",
  "20_90","20_121","20_257","20_354","20_355","20_412",
  "20_422","20_442","20_562","26_6","26_38","28_13")
)

# Armamos data frame que tiene coeficientes de volatilidad y coeficiente de Gini.

VOLATIL21_24 = tibble(VOLATILIDAD24, GINI20$EDO_MUN ,"GINI" = GINI20$COEF)

VOLATIL21_24= VOLATIL21_24 %>%
  filter(!ID_EDO%in%c("29_151") )

# Resumen estadístico 

summary(VOLATIL21_24[,c("VOL","GINI")])

# gráficos descriptivos 

# Preparamos datos 
df=data.frame(
  rbind(
    cbind(VOLATIL21_24$VOL,"VOL"),
    cbind(VOLATIL21_24$GINI,"GINI")
  )
)

# Ponemos nombre a las columnas
colnames(df) =c("x","grupo")

#hacemos numericos los valores de "x"
df=df %>% mutate(x= as.numeric(as.character(x)))

# fijamos colores para graficos 
k_colors = c("#0000FF","#4682B4")

# generamos grafico de densidad o histogramas
ggplot(df, aes(x = x, fill = grupo)) + 
  geom_density (alpha = 0.5, position = "identity")+
  scale_fill_manual(values = k_colors, labels = c( "Coeficiente de Gini", "Volatilidad electoral ") )+
  guides(fill = guide_legend(title = " "))+
  labs(x = "Coeficientes" ,y = "Densidad" )+
  theme(legend.position = "bottom")

# generamos grafico box-plot
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



# mapas de calor 

# cargamos archivo shp
shp_municipios <- st_read("DATA/shp/889463776079_s/mg_sep2019_integrado/mg_sep2019_integrado/conjunto_de_datos/00mun.shp" )

#Proceso para generar mapas de calor
VOLATIL21_24 = VOLATIL21_24 %>%
  mutate(
    CVEGEO = ifelse(
      as.numeric(as.character(CVEGEO)) < 10,
      paste(0, CVEGEO, sep = ""),
      as.character(CVEGEO)
    ),
    ID_MUNICIPIO = ifelse(
      as.numeric(as.character(ID_MUNICIPIO)) < 10,
      paste("00", ID_MUNICIPIO, sep = ""),
      ifelse(
        as.numeric(as.character(ID_MUNICIPIO)) < 100,
        paste("0", ID_MUNICIPIO, sep = ""),
        as.character(ID_MUNICIPIO)
      )
    ),
    CVEGEO = paste(as.character(CVEGEO), as.character(ID_MUNICIPIO), sep = ""),
    G_VOL = c("VOL"),
    G_GINI = c("GINI")
  )

# pegamos bases de datos volatilidad 2021-2024 a la base shp
shp_municipios= inner_join(shp_municipios,VOLATIL21_24, by = "CVEGEO")

# seleccionamos base de datos coeficiente de volatilidad
shp_municipios1 = shp_municipios %>%
  select(CVEGEO, MUNICIPIO, VOL, geometry)

#mapa de coeficientes de Gini
tm_shape(shp_municipios1) +
  tm_fill("VOL",
          palette = "Blues",
          id = "MUNICIPIO",
          title = "Volatilidad") +
  tm_borders("grey25", alpha = .20) +
  tm_layout(".", main.title.position = "") +
  tm_view(view.legend.position = c("left", "bottom"))


# seleccionamos base de datos coeficiente de Gini
shp_municipios1 = shp_municipios %>%
  select(CVEGEO, MUNICIPIO, GINI, geometry)

#mapa de coeficientes de Gini
tm_shape(shp_municipios1) +
  tm_fill("GINI",
          palette = "Blues",
          id = "MUNICIPIO",
          title = "Gini") +
  tm_borders("grey25", alpha = .20) +
  tm_layout(".", main.title.position = "") +
  tm_view(view.legend.position = c("left", "bottom"))

#Analisis cluster

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

df_eu=dist_eu(VOLATIL21_24,VOLATIL21_24$EDO_MUN)

sqrt(
  (VOLATIL21_24$VOL[1]-VOLATIL21_24$VOL[2])^2+
    (VOLATIL21_24$GINI[1]-VOLATIL21_24$GINI[2])^2
)


#guardar distancias euclidianas
#write.csv(df_eu,"distancias_euclidianas_18_21.csv")


# distancias euclideanas con matrices 
# al multimpolicar la transpuesta por la matriz se obitiene una matriz identidad
# propiedades de matricez
x= as.matrix(VOLATIL21_24[1:10,c("VOL","GINI")])
z <- x %*% t(x)

df_euq2=sqrt(diag(z)+ t(diag(z)-2*z ) )

# distancias euclideas con función dist
df_euq = dist(VOLATIL21_24[,c("VOL","GINI")], method = "euclidean")

#calcular la matriz de distacias con get_dist

m.distancia <- get_dist(VOLATIL21_24[,c("VOL","GINI")], method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"

fviz_dist(m.distancia, gradient = list(low = "#87CEEB", mid = "white", high = "#0000FF"), lab_size = 5)



# Cluster jerarquico
#obtenemos la matriz de distancias escaladas

df_euq = dist(scale(VOLATIL21_24[,c("VOL","GINI")] ), method = "euclidean")
# creamos el objeto hclust con el metodo ward
#jera1= hclust(df_euq, method = "ward.D2")

jera2= hcut(df_euq,k = 3,stand = T,hc_method = "ward.D2")

# dendograma 
fviz_dend(jera2, rect = TRUE, cex = 0.1, 
          main = "Dendrograma - ward", xlab = "Municipios", ylab = "Distancias",
          k_colors = c("#0000FF","#87CEEB" ,"#4682B4") )


# coeficiente de siluetas
#para iniciar un kecnter con un numero de cluster fijo, tomaresmos el número de clusters 
#datos por el dendograma
set.seed(12345)
kk = kmeans(VOLATIL21_24[,c("VOL","GINI" )],3)

s= silhouette(kk$cluster,dist(VOLATIL21_24[,c("VOL","GINI" )]) )

plot(s)

df=as.data.frame(s)

fviz_nbclust(VOLATIL21_24[,c("VOL","GINI" )], kmeans , method = "silhouette")

# Define tus colores personalizados
k_colors = c("#0000FF","#87CEEB" ,"#4682B4")


# Create a grouped horizontal bar plot
nombres_personalizados <- c("1: 932, 0.49","2: 420, 0.41","3: 1082,0.45")

ggplot(df,aes(x=sil_width,y= reorder(sil_width, cluster),fill = as.factor(cluster)) )  +
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
m.distancia <- get_dist(VOLATIL21_24[,c("VOL","GINI")], method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"

NbClust(m.distancia, min.nc=2, max.nc=15, method="kmeans")

#estimar el número de clústers
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

set.seed(12345)
k2 <- kmeans(VOLATIL21_24[,c("VOL","GINI")], centers = 3,nstart = 23)
k2$centers # Centroides
table(k2$cluster) # número de clusters
VOLATIL21_24 = cbind(VOLATIL21_24,kk$cluster ) # base con clusters 

fviz_cluster(
  k2,
  data = VOLATIL21_24[, c("VOL", "GINI")],
  ellipse.type = "euclid",
  repel = TRUE,
  star.plot = TRUE
) +
  scale_color_manual(values = c("#0000FF", "#4682B4", "#87CEEB")) +
  theme_minimal() +
  labs(x = "Coeficiente de Volatilidad", # Título del eje X
       y = "Coeficiente de Gini", # Título del eje Y
       title = "null") +
  theme(
    plot.title = element_text(hjust = 0.5),
    # Centrar el título
    plot.subtitle = element_text(hjust = 0.5),
    # Centrar el subtítulo
    plot.caption = element_text(hjust = 0.5)
  )  # Centrar el pie de página

#resumen cluster1

dist_clus1 = VOLATIL21_24 %>%
  filter(k2$cluster==1)
dist_clus2 = VOLATIL21_24 %>%
  filter(k2$cluster==2)
dist_clus3 = VOLATIL21_24 %>%
  filter(k2$cluster==3)

summary(dist_clus1[,c("VOL","GINI" )] )
summary(dist_clus2[,c("VOL","GINI" )] )
summary(dist_clus3[,c("VOL","GINI" )] )

df= as.data.frame( rbind(
  cbind(dist_clus2$VOL,"Clúster 2 Volatilidad"),
  cbind(dist_clus3$VOL,"Clúster 3 Volatilidad"),
  cbind(dist_clus1$VOL,"Clúster 1 Volatilidad"),
  
  cbind(dist_clus1$GINI,"Clúster 1 Gini"),
  cbind(dist_clus2$GINI,"Clúster 2 Gini"),
  cbind(dist_clus3$GINI,"Clúster 3 Gini")))




names(df) = c("X", "Grupo")

df = df %>% mutate(X = as.numeric(df$X))

df %>%
  ggboxplot(
    x       = "Grupo",
    y       = "X",
    fill    = "Grupo",
    palette = c(
      "#0000FF",
      "#0000FF",
      "#0000FF",
      "#74a3ff",
      "#74a3ff",
      "#74a3ff"
    )
  ) +
  theme_bw() +
  labs(x = " ", y = "Coeficientes") +
  theme(legend.position = "none", )




df_Ags= VOLATIL21_24 %>%  filter(NOMBRE_ESTADO%in%"AGUASCALIENTES")
