install.packages("msm")
library(msm)

library(readxl)
bdsarm <- read_excel("D:/bdsarm.xlsx")
View(bdsarm)

data(bdsarm)
dim(bdsarm)
names(bdsarm)
bdsarm[1:21,]

attach(bdsarm)
class(Tiempo_meses)
class(Estadocolosaureu)
Estadocolosaureu=factor(Estadocolosaureu)
class(edad)
class(Sexo)
Sexo=factor(Sexo)
class(Sexo)
class(Raza)
Raza=factor(Raza)
class(Estrato)
Estrato=factor(Estrato)
class(edad_agrupada)
edad_agrupada=factor(edad_agrupada)
class(ÍndicedeCharlsonEstudio)
class(ÍndicedeKarnofsky)



statetable.msm(Estadocolosaureu, Id, data=bdsarm)

Q <- rbind ( c(1, 1), c(1, 1))
Q

Q.crude <- crudeinits.msm(Estadocolosaureu ~ Tiempo_meses, Id, data=bdsarm, qmatrix=Q)
Q.crude

sarm.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                 deathexact = FALSE)
sarm.msm


sarmsex.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                    deathexact = FALSE, gen.inits = TRUE, covariates = ~ Sexo)
sarmsex.msm

qmatrix.msm(sarmsex.msm, covariates=list(Sexo=0)) # Female
qmatrix.msm(sarmsex.msm, covariates=list(Sexo=1)) # Male


sarmedadagrup.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                    deathexact = FALSE, gen.inits = TRUE, covariates = ~ edad_agrupada)
sarmedadagrup.msm

qmatrix.msm(sarmedadagrup.msm, covariates=list(edad_agrupada=0)) # 18-30
qmatrix.msm(sarmedadagrup.msm, covariates=list(edad_agrupada=1)) # 31-50
qmatrix.msm(sarmedadagrup.msm, covariates=list(edad_agrupada=2)) # 51-70
qmatrix.msm(sarmedadagrup.msm, covariates=list(edad_agrupada=3)) # Mayor a 71

coef.msm(sarmedadagrup.msm)
hazard.msm(sarmedadagrup.msm)

sarmedad.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                         deathexact = FALSE, gen.inits = TRUE, covariates = ~ edad)
sarmedad.msm

summary.msm(sarmedadagrup.msm)

sarminfprevia.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                         deathexact = FALSE, gen.inits = TRUE, covariates = ~ Infecciónocolonizaciónpre)
sarminfprevia.msm

qmatrix.msm(sarminfprevia.msm, covariates=list(Infecciónocolonizaciónpre=0)) #No
qmatrix.msm(sarminfprevia.msm, covariates=list(Infecciónocolonizaciónpre=1)) #Si


sarmindcharlson.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                         deathexact = FALSE, gen.inits = TRUE, covariates = ~ ÍndicedeCharlsonEstudio)
sarmindcharlson.msm


sarmindkarnof.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                         deathexact = FALSE, gen.inits = TRUE, covariates = ~ ÍndicedeKarnofsky)
sarmindkarnof.msm



coef.msm(sarmsex.msm)
msm.summary(sarmsex.msm)
hazard.msm(sarmsex.msm)


############################################################################
#Modelo 2a: covariables específicas de transición#
sarmsex2.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                    deathexact = FALSE, gen.inits = TRUE, 
                   covariates = list("1-2" = ~ Sexo, "2-1" = ~Sexo))
sarmsex2.msm

sarmsex2.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                    deathexact = FALSE, gen.inits = TRUE, 
                    covariates = list("1-1" = ~ Sexo, "1-2" = ~Sexo))
sarmsex2.msm

sarmsex2.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                    deathexact = FALSE, gen.inits = TRUE, 
                    covariates = list("2-1" = ~ Sexo, "2-2" = ~Sexo))
sarmsex2.msm
#############################################################################


##############################################################################
#Modelo 3: efectos covariables restringidos
sarm3.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                 deathexact = FALSE, gen.inits = TRUE, covariates = ~ Sexo, 
                constraint = list(Sexo=c(1,1)))
sarm3.msm

###############################################################################


###############################################################################
#Modelo 4: parámetros fijos

sarm4.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                 deathexact = FALSE, 
                 control = list(trace=2, REPORT=1), 
                 fixedpars = c(1, 2))
sarm4.msm
###############################################################################


#####################################
#Funciones extractoras
#####################################

#Matrices de intensidad
qmatrix.msm(sarm.msm)
qmatrix.msm(sarm.msm, sojourn = TRUE)[c("estimates","SE")]

#Matrices de probabilidad de transición 
pmatrix.msm(sarm.msm, t=18)
pmatrix.msm(sarm.msm, t=2)
pmatrix.msm(sarm.msm, t=6)
pmatrix.msm(sarm.msm, t=12)

#Tiempos medios de estadía en el mismo estado
sojourn.msm(sarm.msm)
sojourn.msm(sarmsex.msm, covariates=list(Sexo=0)) #Mujer
sojourn.msm(sarmsex.msm, covariates=list(Sexo=1)) #Hombre
sojourn.msm(sarminfprevia.msm, covariates=list(Infecciónocolonizaciónpre=1))
sojourn.msm(sarmantibioticos.msm, covariates=list(Usodeantibióticos=1))
sojourn.msm(sarmdiabetes.msm, covariates=list(Diabetesmellitus=1))
sojourn.msm(sarmfallacard.msm, covariates=list(Fallacardíaca=1))
sojourn.msm(sarmenfermcoro.msm, covariates=list(Enfermedadcoronariainfartor=1))
sojourn.msm(sarmhipert.msm, covariates=list(Hipertensiónarterial=1))




#Probabilidad de que cada estado sea el siguiente
pnext.msm(sarm.msm)

#Duración total de la estadía (REQUIERE ESTADO ABSORBENTE)
totlos.msm(sarm.msm) 

#Tiempos de primer paso esperados 
efpt.msm(sarm.msm)

#Número esperado de visitas (REQUIERE ESTADO ABSORBENTE)
envisits.msm(sarm.msm)

#Relación de intensidades de transición 
qratio.msm(sarm.msm, ind1=c(2,1), ind2=c(1,2))

#Razones de peligro para la transición 
hazard.msm(sarmsex.msm)

#Establecer valores de covariables
qmatrix.msm(sarmsex.msm, covariates = 0)

qmatrix.msm(sarmsex.msm, covariates = list(Sexo = 1))


###################################################
#Gráficos de supervivencia
###################################################

plot(sarmsex.msm, legend.pos=c(8, 1))

prevalence.msm(sarm.msm, times=seq(0,12,2))

plot.prevalence.msm(sarm.msm,mintime = 0, maxtime = 12)

plot.prevalence.msm(sarm.msm, mintime=0, maxtime=12, legend.pos=c(4, 95))





sarmedad.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                   deathexact = FALSE, gen.inits = TRUE, covariates = ~ edad)
sarmedad.msm

sarmraza.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                     deathexact = FALSE, gen.inits = TRUE, covariates = ~ Raza)
sarmraza.msm

qmatrix.msm(sarmraza.msm, covariates=list(Raza=0)) # Negra
qmatrix.msm(sarmraza.msm, covariates=list(Raza=1)) # Blanca

sarmestrato.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                        deathexact = FALSE, gen.inits = TRUE, covariates = ~ Estrato)
sarmestrato.msm

qmatrix.msm(sarmestrato.msm, covariates=list(estrato=1)) # Bajo
qmatrix.msm(sarmestrato.msm, covariates=list(estrato=2)) # Medio
qmatrix.msm(sarmestrato.msm, covariates=list(estrato=3)) # Alto

sarmtabaquismo.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                       deathexact = FALSE, gen.inits = TRUE, covariates = ~ Tabaquismo)
sarmtabaquismo.msm

qmatrix.msm(sarmtabaquismo.msm, covariates=list(Tabaquismo=0)) #No
qmatrix.msm(sarmtabaquismo.msm, covariates=list(Tabaquismo=1)) #Si


sarmhospital.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                          deathexact = FALSE, gen.inits = TRUE, covariates = ~ Hospitalización)
sarmhospital.msm

qmatrix.msm(sarmhospital.msm, covariates=list(Hospitalización=0)) #No
qmatrix.msm(sarmhospital.msm, covariates=list(Hospitalización=1)) #Si


sarmantibioticos.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                        deathexact = FALSE, gen.inits = TRUE, covariates = ~ Usodeantibióticos)
sarmantibioticos.msm

qmatrix.msm(sarmantibioticos.msm, covariates=list(Usodeantibióticos=0)) #No
qmatrix.msm(sarmantibioticos.msm, covariates=list(Usodeantibióticos=1)) #Si



sarmdiabetes.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                          deathexact = FALSE, gen.inits = TRUE, covariates = ~ Diabetesmellitus)
sarmdiabetes.msm

qmatrix.msm(sarmdiabetes.msm, covariates=list(Diabetesmellitus=0)) #No
qmatrix.msm(sarmdiabetes.msm, covariates=list(Diabetesmellitus=1)) #Si



sarmfallacard.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                        deathexact = FALSE, gen.inits = TRUE, covariates = ~ Fallacardíaca)
sarmfallacard.msm

qmatrix.msm(sarmfallacard.msm, covariates=list(Fallacardíaca=0)) #No
qmatrix.msm(sarmfallacard.msm, covariates=list(Fallacardíaca=1)) #Si



sarmenfermcoro.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                         deathexact = FALSE, gen.inits = TRUE, covariates = ~ Enfermedadcoronariainfartor)
sarmenfermcoro.msm

qmatrix.msm(sarmenfermcoro.msm, covariates=list(Enfermedadcoronariainfartor=0)) #No
qmatrix.msm(sarmenfermcoro.msm, covariates=list(Enfermedadcoronariainfartor=1)) #Si



sarmhipert.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                          deathexact = FALSE, gen.inits = TRUE, covariates = ~ Hipertensiónarterial)
sarmhipert.msm

qmatrix.msm(sarmhipert.msm, covariates=list(Hipertensiónarterial=0)) #No
qmatrix.msm(sarmhipert.msm, covariates=list(Hipertensiónarterial=1)) #Si



sarmtipocateter.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                      deathexact = FALSE, gen.inits = TRUE, covariates = ~ Tipocateter)
sarmtipocateter.msm

qmatrix.msm(sarmtipocateter.msm, covariates=list(Tipocateter=0)) #Femoral
qmatrix.msm(sarmtipocateter.msm, covariates=list(Tipocateter=1)) #Yugular
qmatrix.msm(sarmtipocateter.msm, covariates=list(Tipocateter=2)) #Ninguno



sarmfemoral.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                           deathexact = FALSE, gen.inits = TRUE, covariates = ~ Femoral)
sarmfemoral.msm



sarmniegafis.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                       deathexact = FALSE, gen.inits = TRUE, covariates = ~ Negaciónalusodefístula)
sarmniegafis.msm

qmatrix.msm(sarmniegafis.msm, covariates=list(Negaciónalusodefístula=0)) #No
qmatrix.msm(sarmniegafis.msm, covariates=list(Negaciónalusodefístula=1)) #Si




sarmtabaquismoactual.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject=Id, data = bdsarm, qmatrix = Q, 
                          deathexact = 3, gen.inits = TRUE, covariates = ~ Tabaquismo_actual)
sarmtabaquismoactual.msm

qmatrix.msm(sarmtabaquismoactual.msm, covariates=list(Tabaquismo_actual=0)) #No
qmatrix.msm(sarmtabaquismoactual.msm, covariates=list(Tabaquismo_actual=1)) #Si

plot(sarmtabaquismoactual.msm, legend.pos=c(8, 1))

#FUNCIONES EXTRACTORAS

qmatrix.msm(sarm.msm)
qmatrix.msm(sarm.msm)$SE
qmatrix.msm(sarm.msm)$estimates
qmatrix.msm(sarm.msm)$L
qmatrix.msm(sarm.msm)$U
qmatrix.msm(sarm.msm, ci=c("normal"),sojourn = TRUE)

pmatrix.msm(sarm.msm, t=12)

sojourn.msm(sarm.msm)

pnext.msm(sarm.msm)

totlos.msm(sarm.msm, t=18)

#Razón estimada de intensidades de transición
#Calcula la estimación y el error estándar aproximado de la relación de dos 
#intensidades de transición estimadas de un modelo multiestado ajustado en un 
#conjunto dado de valores de covariables.
qratio.msm(sarm.msm, ind1=c(1,2), ind2=c(2,1))

hazard.msm(sarmsex.msm)

plot(sarm.msm)


plot(sarm.msm, legend.pos=c(8, 1))
plot.msm(sarm.msm,1,2)
plot(sarm.msm, 1, FALSE, legend.pos=c(8, 1))

plot.msm(sarm.msm, from = NULL, to = NULL, range = NULL, legend.pos = NULL, 
         covariates = edad_cat)

prevalence.msm(sarm.msm, times=seq(0,18,4))
plot.prevalence.msm(sarm.msm, mintime=0, maxtime=18, legend.pos=c(4, 95))

plot.survfit.msm(sarm.msm)

plotprog.msm(state~months, data= sarm)

#Extrae coeficientes del modelo
#Extraiga las intensidades de transición logarítmicas estimadas y los efectos 
#lineales correspondientes de cada covariable.
coef.msm(sarmsex.msm)
coef.msm(sarminf.msm)
coef.msm(sarmage.msm)

#Prueba de razón de verosimilitud entre dos o más modelos multiestado ajustados
#
lrtest.msm(sarm.msm, sarmsex.msm)
lrtest.msm(sarm.msm, sarmage.msm)
lrtest.msm(sarm.msm, sarminf.msm)
lrtest.msm(sarm.msm, sarmraza.msm)
lrtest.msm(sarm.msm, sarmestrato.msm)

qratio.msm(sarmsex.msm, ind1=c(2,1), ind2=c(1,2))

#Extraer modelo logarítmico de probabilidad
#Extraiga la probabilidad logarítmica y el número de parámetros de un 
#modelo equipado con msm.
logLik.msm(sarm.msm)

model.frame(sarm.msm)


summary(bdsarm)

summary(Sexo)
prop.table(bdsarm)

mean(Tiempo_meses)
mean(Tiempo_dias)

qmatrix.msm(sarm.msm,sojourn=TRUE)[c("estimates","SE")]


summary(edad)

printnew.msm(sarm.msm)
printold.msm(sarm.msm)













#MODELOS DE CLASIFICACION ERRONEA#
#######################################

Qm <- rbind ( c(0.852, 0.147), c(0.442, 0.557))

ematrix <- rbind ( c(0.1, 0.1), c(0.1, 0.1))

sarmmisc.msm <- msm(Estadocolosaureu ~ Tiempo_meses, subject = Id, data = bdsarm,
                   qmatrix = Qm, ematrix = ematrix, deathexact = FALSE)
sarmmisc.msm
