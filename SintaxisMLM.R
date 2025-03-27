
# Cargar datos

library(haven)

options(scipen = 999) # Elimina notación científica
datosMLM <- read_sav("datosMLM.sav")

library(lme4)
library(lmerTest)

# Gráfico: Relación entre intención y actividad física a través de los vecindarios

library(ggplot2)

ggplot(datosMLM, aes(x = Intencion_Conductual, 
                     y = Actividad_Fisica)) + 
  geom_point(size = 1, 
             alpha = 0.5) +
  geom_smooth(method = "lm",
              color = "grey20") +
  facet_wrap(~IDvecindario) +
  xlab("Intención conductual") + 
  ylab("Actividad física semanal") +
  ylim(0,7) +
  theme_bw()

# Modelos de regresión multinivel ----


## 1) Modelo nulo ----

M.nulo <- lmer(Actividad_Fisica ~ 1 + (1 | IDvecindario), 
               data = datosMLM, 
               REML = TRUE)

summary(M.nulo)

library(performance)

icc(M.nulo)

### Centrar intención conductual en el promedio grupal ----

library(misty)

datosMLM$Intencion_ConductualCPG <- center(datosMLM$Intencion_Conductual, 
                                           type = "CWC", 
                                           cluster = datosMLM$IDvecindario)

## 2) Modelo de intercepto aleatorio ----

M.interaleatorio <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area +
                           (1 | IDvecindario), 
                         data = datosMLM, 
                         REML = TRUE)

summary(M.interaleatorio, ddf = "Kenward-Roger") # valores p método de Kenward-Roger

## 3) Modelo de coeficientes aleatorios ----

datosMLM$Actividad_Fisica <- as.numeric(datosMLM$Actividad_Fisica)
datosMLM$Intencion_ConductualCPG <- as.numeric(datosMLM$Intencion_ConductualCPG)
datosMLM$Disp_Area <- as.numeric(datosMLM$Disp_Area)

M.coefaleatorios <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area + 
                           (1 + Intencion_ConductualCPG | IDvecindario), 
                         data = datosMLM, 
                         REML = TRUE)

ranova(M.coefaleatorios)

summary(M.coefaleatorios, ddf = "Kenward-Roger")

# Tamaño del efecto (R2) ----

library(r2mlm)

r2 <- r2mlm(M.coefaleatorios)

r2$Decompositions

## Varianza intra-grupos explicada por el efecto fijo y aleatorio de intención
0.2036128 + 0.1150949

## Varianza total explicada por el efecto fijo y aleatorio de intención
0.14987367 + 0.08471815

## Varianza entre-grupos explicada por el efecto fijo de areas
0.556718

## Varianza total explicada por el efecto fijo de areas
0.14693346

## 4) Modelo de interceptos y pendientes como desenlace ----

M.internivelcruzado <- lmer(Actividad_Fisica ~ Intencion_ConductualCPG + Disp_Area + 
                              Intencion_ConductualCPG:Disp_Area + 
                              (1 + Intencion_ConductualCPG | IDvecindario), 
                            data = datosMLM, 
                            REML = TRUE)

summary(M.internivelcruzado, ddf = "Kenward-Roger")




