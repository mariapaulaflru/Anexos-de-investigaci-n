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
Empresas_TD <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_EMPRESAS.xlsx", 
                             sheet = "TD.TIP", range = "A1:K19", col_types = c("numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric", "numeric", "numeric", 
                                                                               "numeric"))
View(Empresas_TD)

Empresas_TEC <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_EMPRESAS.xlsx", 
                             sheet = "TEC.TIP", range = "A1:P19", 
                             col_types = c("numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))
View(Empresas_TEC)

Empresas_FACT <- read_excel("C:\\Users\\MARIAFLOREZ\\OneDrive - UNIVERSIDAD INDUSTRIAL DE SANTANDER\\AFE_empresas\\AFE EMPRESAS\\AFE ajustado\\DATOS_EMPRESAS.xlsx", 
                             sheet = "FACT.TIP", range = "A1:S19", 
                             col_types = c("numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))
View(Empresas_FACT)


#Correlación de datos
empcorrel_TD <- cor(Empresas_TD) 
empcorrel_TEC <- cor(Empresas_TEC) 
empcorrel_FACT <- cor(Empresas_FACT)


#Verificar que datos sean adecuados
  #Prueba de bartlett
cortest.bartlett(empcorrel_TD, n = 100)->p_esf_emp_TD
p_esf_emp_TD$p

cortest.bartlett(empcorrel_TEC, n = 100)->p_esf_emp_TEC
p_esf_emp_TEC$p

cortest.bartlett(empcorrel_FACT, n = 100)->p_esf_emp_FACT
p_esf_emp_FACT$p

#Prueba de KMO
KMO(empcorrel_TD)
KMO(empcorrel_TEC)
KMO(empcorrel_FACT)

#Identificar numero de factores
paral_TD_emp<-fa.parallel(empcorrel_TD,n.obs=121,fa="fa",fm="ml", main = '', ylabel = '')
title('Análisis Paralelo - Transformación Digital empresas', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')

paral_TEC_emp<-fa.parallel(empcorrel_TEC,n.obs=256,fa="fa",fm="ml", main = '', ylabel = '')
title('Análisis Paralelo - Tecnologías 4.0 empresas', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')

paral_FACT_emp<-fa.parallel(empcorrel_FACT,n.obs=361,fa="fa",fm="minres", main = '', ylabel = '')
title('Análisis Paralelo - Factores influyentes empresas', line = 2, cex.main=0.9,
      xlab = 'Número de factores', ylab = 'Valores propios')



#Modelo factorial
library(GPArotation)
modelo_TD_emp<-fa(empcorrel_TD, nfactors = 2, rotate = "varimax", fm="ml")
modelo_TEC_emp<-fa(empcorrel_TEC, nfactors = 5, rotate = "varimax", fm="ml")
modelo_FACT_emp<-fa(empcorrel_FACT, nfactors = 3, rotate = "varimax", fm="minres")
colnames(modelo_TD_emp$loadings) <- c("Factor_1", "Factor_2")
colnames(modelo_TEC_emp$loadings) <- c("Factor_1", "Factor_2", "Factor_3", "Factor_4", "Factor_5")
colnames(modelo_FACT_emp$loadings) <- c("Factor_1", "Factor_2", "Factor_3")

load_TD_emp <- modelo_TD_emp$loadings[,0:2]
load_TEC_emp <- modelo_TEC_emp$loadings[,0:5]
load_FACT_emp <- modelo_FACT_emp$loadings[,0:3]

comunalidades_TD_emp <- modelo_TD_emp$communality
especificidades_TD_emp <- modelo_TD_emp$uniquenesses

comunalidades_TEC_emp <- modelo_TEC_emp$communality
especificidades_TEC_emp <- modelo_TEC_emp$uniquenesses

comunalidades_FACT_emp <- modelo_FACT_emp$communality
especificidades_FACT_emp <- modelo_FACT_emp$uniquenesses


#puntajes
scores_TD_emp <- fa(Empresas_TD, nfactors = 2, rotate = "varimax", fm="ml", scores = "regression")$scores
scores_TEC_emp <- fa(Empresas_TEC, nfactors = 5, rotate = "varimax", fm="ml", scores = "regression")$scores
scores_FACT_emp <- fa(Empresas_FACT, nfactors = 3, rotate = "varimax", fm="minres", scores = "regression")$scores
colnames(scores_TD_emp) <- c("Factor_1", "Factor_2")
colnames(scores_TEC_emp) <- c("Factor_1", "Factor_2", "Factor_3", "Factor_4", "Factor_5")
colnames(scores_FACT_emp) <- c("Factor_1", "Factor_2", "Factor_3")


#Exportar datos excel
install.packages("xlsx")
library(xlsx)
write.xlsx(load_TD_emp, "AFE_TD_emp.xlsx")
write.xlsx(comunalidades_TD_emp, "comunalidades_TD_emp.xlsx")
write.xlsx(especificidades_TD_emp, "especificidades_TD_emp.xlsx")
write.xlsx(scores_TD_emp, "scores_TD_emp.xlsx")

write.xlsx(load_TEC_emp, "AFE_TEC_emp.xlsx")
write.xlsx(comunalidades_TEC_emp, "comunalidades_TEC_emp.xlsx")
write.xlsx(especificidades_TEC_emp, "especificidades_TEC_emp.xlsx")
write.xlsx(scores_TEC_emp, "scores_TEC_emp.xlsx")

write.xlsx(load_FACT_emp, "AFE_FACT_emp_1.xlsx")
write.xlsx(comunalidades_FACT_emp, "comunalidades_FACT_emp.xlsx")
write.xlsx(especificidades_FACT_emp, "especificidades_FACT_emp.xlsx")
write.xlsx(scores_FACT_emp, "scores_FACT_emp.xlsx")
