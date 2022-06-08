install.packages("polycor")
install.packages("ggcorrplot")
install.packages("GPArotation")
library(psych)
library(polycor)
library(ggplot2)
library(ggcorrplot)

options(max.print=1000000)

#Cargue datos tipificados

library(readxl)
Grupos_TD <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_GRUPOS.xlsx", 
                           sheet = "TD.TIP", range = "A1:K31", col_types = c("numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric"))
View(Grupos_TD)


GRUPOS_TEC <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_GRUPOS.xlsx", 
                           sheet = "TEC.TIP", range = "A1:P31", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
View(GRUPOS_TEC)

GRUPOS_FACT <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_GRUPOS.xlsx", 
                           sheet = "FACT.TIP", range = "A1:S31", 
                           col_types = c("numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
View(GRUPOS_FACT)

#Correlación de datos
grupocorrel_TD <- cor(Grupos_TD) 
grupocorrel_TEC <- cor(GRUPOS_TEC) 
grupocorrel_FACT <- cor(GRUPOS_FACT)

#Verificar que datos sean adecuados
#Prueba de bartlett
cortest.bartlett(grupocorrel_TD, n = 100)->p_esf_grupo_TD
p_esf_grupo_TD$p

cortest.bartlett(grupocorrel_TEC, n = 100)->p_esf_grupo_TEC
p_esf_grupo_TEC$p

cortest.bartlett(grupocorrel_FACT, n = 100)->p_esf_grupo_FACT
p_esf_grupo_FACT$p

#Prueba de KMO
KMO(grupocorrel_TD)
KMO(grupocorrel_TEC)
KMO(grupocorrel_FACT)

#Identificar numero de factores
paral_TD_grupo<-fa.parallel(grupocorrel_TD,n.obs=121,fa="fa",fm="ml", main = '', ylabel = '')
title('Análisis Paralelo - Transformación Digital grupos', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')

paral_TEC_grupo<-fa.parallel(grupocorrel_TEC,n.obs=256,fa="fa",fm="ml", main = '', ylabel = '')
title('Análisis Paralelo - Tecnologías 4.0 grupos', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')

paral_FACT_grupo<-fa.parallel(grupocorrel_FACT,n.obs=361,fa="fa",fm="ml", main = '', ylabel = '')
title('Análisis Paralelo - Factores influyentes grupos', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')


#Modelo factorial
library(GPArotation)
modelo_TD_grupo<-fa(grupocorrel_TD, nfactors = 2, rotate = "varimax", fm="ml")
modelo_TEC_grupo<-fa(grupocorrel_TEC, nfactors = 4, rotate = "varimax", fm="ml")
modelo_FACT_grupo<-fa(grupocorrel_FACT, nfactors = 3, rotate = "varimax", fm="ml")

colnames(modelo_TD_grupo$loadings) <- c("Factor_1", "Factor_2")
colnames(modelo_TEC_grupo$loadings) <- c("Factor_1", "Factor_2", "Factor_3", "Factor_4")
colnames(modelo_FACT_grupo$loadings) <- c("Factor_1", "Factor_2", "Factor_3")

load_TD_grupo <- modelo_TD_grupo$loadings[,0:2]
load_TEC_grupo <- modelo_TEC_grupo$loadings[,0:4]
load_FACT_grupo <- modelo_FACT_grupo$loadings[,0:3]

comunalidades_TD_grupo <- modelo_TD_grupo$communality
especificidades_TD_grupo <- modelo_TD_grupo$uniquenesses

comunalidades_TEC_grupo <- modelo_TEC_grupo$communality
especificidades_TEC_grupo <- modelo_TEC_grupo$uniquenesses

comunalidades_FACT_grupo <- modelo_FACT_grupo$communality
especificidades_FACT_grupo <- modelo_FACT_grupo$uniquenesses


#puntajes
scores_TD_grupo <- fa(Grupos_TD, nfactors = 2, rotate = "varimax", fm="ml", scores = "regression")$scores
scores_TEC_grupo <- fa(GRUPOS_TEC, nfactors = 4, rotate = "varimax", fm="ml", scores = "regression")$scores
scores_FACT_grupo <- fa(GRUPOS_FACT, nfactors = 3, rotate = "varimax", fm="minres", scores = "regression")$scores
colnames(scores_TD_grupo) <- c("Factor_1", "Factor_2")
colnames(scores_TEC_grupo) <- c("Factor_1", "Factor_2", "Factor_3", "Factor_4")
colnames(scores_FACT_grupo) <- c("Factor_1", "Factor_2", "Factor_3")


#Exportar datos excel
install.packages("xlsx")
library(xlsx)
write.xlsx(load_TD_grupo, "AFE_TD_grupo.xlsx")
write.xlsx(comunalidades_TD_grupo, "comunalidades_TD.xlsx")
write.xlsx(especificidades_TD_grupo, "especificidades_TD.xlsx")
write.xlsx(scores_TD_grupo, "scores_TD_grupo.xlsx")

write.xlsx(load_TEC_grupo, "AFE_TEC_grupo.xlsx")
write.xlsx(comunalidades_TEC_grupo, "comunalidades_TEC.xlsx")
write.xlsx(especificidades_TEC_grupo, "especificidades_TEC.xlsx")
write.xlsx(scores_TEC_grupo, "scores_TEC_grupo.xlsx")

write.xlsx(load_FACT_grupo, "AFE_FACT_grupo.xlsx")
write.xlsx(comunalidades_FACT_grupo, "comunalidades_FACT.xlsx")
write.xlsx(especificidades_FACT_grupo, "especificidades_FACT.xlsx")
write.xlsx(scores_FACT_grupo, "scores_FACT_grupo.xlsx")
