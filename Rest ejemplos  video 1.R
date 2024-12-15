#Ejemplo 1

nombres<-c("Medición I","Medición II","Medición III","Medición IV","Medición V","Medición VI")
volúmenes<-c(5,5.5,5.7,6.0,6.8,7.0)
temperaturas<-c(20,21,22,23,24,25)

# creamos el dataframe 

medidas<-data.frame(nombre=nombres,volumen=volúmenes,temperatura=temperaturas)

head(medidas)

#estrura del dataframe

str(medidas)

medidas$nombre

medidas$volumen

#Resumen estadísticos del dataframe

summary(medidas)

dim(medidas)

medidas$temperatura

#Escoger múltiples columnas
medidas[,c("nombre","temperatura")]
medidas[medidas$temperatura>23,]

#EJEMPLO 2

medidas<-c("I","II","III","IV","V","VI","VII","VIII","IX","X")
volúmenes<-c(49.8,50,50.2,50.4,50.7,51,51.3,51.8,52.2,52.6)
temperaturas<-c(5,15,20,25,30,40,50,60,70,80)

# creamos el dataframe 

agua<-data.frame(medida=medidas,volumen=volúmenes,temperatura=temperaturas)

head(agua)

#estrura del dataframe

str(agua)

agua$medida

agua$volumen

#Resumen estadísticos del dataframe

summary(agua)

dim(agua)

agua$temperatura

#Escoger múltiples columnas
agua[,c("volumen","temperatura")]
agua[agua$temperatura>40,]

#VIDEO 2. DPLYR APLICACIÓN
#EJEMPLO 1

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

#Conjunto de datos
solubilidad <- data.frame(
  Compuesto = c("NaBr", "ZnCl", "KCl2", "Na2SO4", "Cr2SO4"),
  Solvente = c("Agua", "Etanol", "Agua", "Metanol", "Etanol"),
  Temperatura_C = c(25, 25, 50, 25, 30),
  Solubilidad_g_L = c(360, 25, 745, 310, 203)
  )
#compuestos solubles en agua, filtrar
solubilidad_agua <- solubilidad %>% 
  filter(Solvente == "Agua")

#columna que indique si la solubilidad supera 300 g/L
solubilidad <- solubilidad %>% 
  mutate(Alta_Solubilidad = Solubilidad_g_L > 300)

resumen_solubilidad <- solubilidad %>% 
  group_by(Solvente) %>% 
  summarise(
    Promedio_Solubilidad = mean(Solubilidad_g_L),
    Maxima_Solubilidad = max(Solubilidad_g_L)
  )
#Resultados
print("Compuestos solubles en agua:")
print(solubilidad_agua)

print("Solubilidad con clasificación de alta solubilidad:")
print(solubilidad)

print("Resumen por solvente:")
print(resumen_solubilidad)

#EJEMPLO 2: DPLYR

#Reacciones de ácidos con bases

reacciones <- data.frame(
  Acido = c("HCl", "H2SO4", "HNO3", "CH3COOH", "H2CO3"),
  Base = c("NaOH", "KOH", "Ca(OH)2", "NH3", "Mg(OH)2"),
  Temperatura_C = c(25, 30, 40, 25, 50),
  pH_Inicial = c(1, 2, 1.5, 3.5, 4),
  pH_Final = c(7, 7, 7, 8, 8.5)
)

#filtrar reacciones a temperatura mayor a 25 °C
reacciones_temp_alta <- reacciones %>% 
  filter(Temperatura_C > 25)

#creación de columna del cambio de pH
reacciones <- reacciones %>% 
  mutate(Cambio_pH = pH_Final - pH_Inicial)

#Resumen de los datos del cambio de pH
resumen_reacciones <- reacciones %>% 
  group_by(Base) %>% 
  summarise(
    Promedio_Cambio_pH = mean(Cambio_pH),
    Maximo_Cambio_pH = max(Cambio_pH)
  )

alcular_suma_y_porcentaje <- function(df, col1, col2) {
  df %>% 
    mutate(Suma = {{col1}} + {{col2}},
           Porcentaje_Col1 = ({{col1}} / Suma) * 100,
           Porcentaje_Col2 = ({{col2}} / Suma) * 100)
}

#Resultados del análisis
print("Reacciones a temperatura mayor a 25 °C:")
print(reacciones_temp_alta)

print("Reacciones con cambio de pH calculado:")
print(reacciones)

print("Resumen del cambio de pH por base:")
print(resumen_reacciones)

# Resumen estadístico del cambio de pH agrupado por base
resumen_reacciones <- reacciones %>% 
  group_by(Base) %>% 
  summarise(
    Promedio_Cambio_pH = mean(Cambio_pH),
    Maximo_Cambio_pH = max(Cambio_pH)
  )

#las bases por el promedio del cambio de pH en orden descendente
reacciones_ordenadas <- resumen_reacciones %>% 
  arrange(desc(Promedio_Cambio_pH))

calcular_suma_y_porcentaje <- function(df, col1, col2) {
  df %>% 
    mutate(
      Suma = !!sym(col1) + !!sym(col2),
      Porcentaje_Col1 = (!!sym(col1) / Suma) * 100,
      Porcentaje_Col2 = (!!sym(col2) / Suma) * 100
    )
}
reacciones_con_sumas <- calcular_suma_y_porcentaje(reacciones, "pH_Inicial", "pH_Final")


print("Reacciones con suma y porcentajes calculados:")
print(reacciones_con_sumas)
