################################################################################
##                                                                            ##
##                     Conjoint Analysis Replication Code                     ##
##                            RC University Paper                             ##
##                                                                            ##
##                     Milton M. Inostroza, Denise Laroze.                    ##
##                                                                            ##
##                              01 Octubre 2021                               ##
##                                                                            ##
################################################################################


library(plyr)
library(dplyr)
library(stargazer)
library(nnet)
library(ggplot2)
library(stringi)
library(clusterSEs)
library(xtable)
library(psych)
library(rms)
library(FindIt)
library(data.table)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(mfx)
library(margins)


rm(list = ls())

#setwd("/Users/miltoninos/Desktop/Modulo Proyecto Tesis/Codigos Conjoint/Experimento/Resultados/final")

#source("conjoint_recode.R")

load("conjoint1N.Rdata")

conjoint2 <- droplevels(conjoint1N)  

conjoint <- rbind(conjoint2)

###################################
### Data prep 
###################################
# Recoding base category

#unique(conjoint1$prensa)
conjoint2 <- within(conjoint2, prensa<- relevel(prensa, ref = "La prensa indica que su productividad cientifica esta en el promedio nacional"))

#unique(conjoint1$corporativa)
conjoint2 <- within(conjoint2, corporativa <- relevel(corporativa, ref = "Su pagina web indica que su nivel academico esta en el promedio nacional"))

#unique(conjoint1$twit_contacto)
conjoint2 <- within(conjoint2, twit_contacto <- relevel(twit_contacto, ref = "Comparte articulo sobre universidades acreditadas por 5 anos que incluye a esta"))

#unique(conjoint1$twit_influencer)
conjoint2 <- within(conjoint2, twit_influencer <- relevel(twit_influencer, ref = "Relata que la calidad academica de esta universidad es regular"))



####################################
#### Recode for Figures
####################################

conjoint <- rbind(conjoint2)
stargazer(conjoint)

#conjoint[conjoint==""]  <- NA 


hombre<-conjoint$hombre
levels1<-c("1", "0")

nativo<-conjoint$Nativo_D
levels2<-c("1", "0")

estudios<-conjoint$nivel_estudios
levels3<-c("Básica completa", "Educación media completa (rindiendo PSU o PAA)", "Educación media incompleta", "Estudios de postgrado completos", 
           "Estudios de postgrado incompleto", "Media completa", "Otro", "Técnico (o instituto profesional) completo", "Técnico (o instituto profesional) incompleto",
           "Universitario completos", "Universitario incompletos")

generacion<-conjoint$generacion
levels4<-c("BB", "G_X", "G_Y", "G_Z")

formacion<-conjoint$tipo_institución
levels5<-c("Instituto Profesional", "Universidad Estatal o Pública", "Universidad Privada", "Universidad Tradicional No Estatal")

actividad<-conjoint$actividad
levels6<-c("Auto empleado/ independiente", "Desempleado(a)  y buscando trabajo", "Desempleado(a) y sin buscar trabajo",
           "En situación de discapacidad sin posibilidad de trabajar", "Estudiante", "Otro",
           "Trabaja como dueño(a) de casa", "Trabajando a tiempo completo", "Trabajando jornada parcial")

estado_civil<-conjoint$estado_civil
levels7<-c("Casada (o)", "Divorciada (o) o Separada (o)", "Otro", "Soltera (o)", "Viuda (o)")

boomers<-conjoint$boomers
levels8<-c("0", "1")

genX<-conjoint$genX
levels8<-c("0", "1")

millennials<-conjoint$millennials
levels9<-c("0", "1")

centennials<-conjoint$centennials
levels10<-c("0", "1")

Educacion<-conjoint$Educacion
levels11<-c("Secundaria", "Tecnica", "Universitaria", "Postgrado")

Educacion2<-conjoint$Educacion2
levels12<-c("Secundaria", "Tecnica", "Universitaria", "Estudiante Universitario", "Postgrado")

Ocupacion<-conjoint$Ocupacion
levels13<-c("Estudiante", "Desempleado", "Empleado", "Estudiante", "Jubilado")

Ocupacion2<-conjoint$Ocupacion2
levels14<-c("Estudiante", "Desempleado", "Empleado", "Empleado Part-Time","Estudiante", "Jubilado")

Grupo_Gen<-conjoint$Grupo_Gen
levels15<-c("Nat_Dig1", "Nat_Dig2", "No_Nat")


####################
### Descriptives
####################
conjoint$universidad_select_reputacion<-as.numeric(conjoint$universidad_select_reputacion)
conjoint$universidad_select_confianza<-as.numeric(conjoint$universidad_select_confianza)
conjoint$universidad_select_prestigio<-as.numeric(conjoint$universidad_select_prestigio)


vars.corr<-c("universidad_select_reputacion"  ,  "universidad_select_confianza",  "universidad_select_prestigio" )
corr.test(conjoint[vars.corr])  ### These values are extreamly correlated, it is probably indifferent which measure is used as a dependent variable


tbl<-conjoint[, c(vars.corr)]


mcor<-round(cor(tbl, use = "complete.obs"),2)
mcor

xtable(mcor)



#############################################
### Main model estimations
##############################################

mylogit <- glm(universidad_select_prestigio ~ prensa + corporativa + twit_contacto + twit_influencer, data = conjoint, family = "binomial")
summary(mylogit)


########### Model estimations brand selection
model1 <- lrm(universidad_select_reputacion~ prensa+corporativa ,data=conjoint,  x=T, y=T)
model2 <- lrm(universidad_select_confianza ~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint,  x=T, y=T)
model3 <- lrm(universidad_select_prestigio ~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint,  x=T, y=T)


model1.cl<-bootcov(model1,cluster=conjoint$id)
model2.cl<-bootcov(model2,cluster=conjoint$id)
model3.cl<-bootcov(model3,cluster=conjoint$id)



stargazer(model1.cl, model2.cl, model3.cl)

, type="html", out="models_cl_C2.html")


modeli <- lm(universidad_select_reputacion~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint)


m <- margins(modeli)
plot(m)
cplot(modeli, "prensa", what = "prediction", main = "Predicted Fuel Economy, Given Weight")

###########################
#### Robustness tests
###########################

########## Model estimations University selection + controls

model11 <- glm(universidad_select_reputacion ~ prensa + corporativa + twit_contacto + twit_influencer + hombre + Educacion + Ocupacion + generacion, family=binomial(link='logit'),data=conjoint2)
summary(model11)
model12 <- glm(universidad_select_confianza ~ prensa + corporativa + twit_contacto + twit_influencer + hombre + Educacion + Ocupacion + generacion, family=binomial(link='logit'),data=conjoint2)
summary(model12)
model13 <- glm(universidad_select_prestigio ~ prensa + corporativa + twit_contacto + twit_influencer + hombre + Educacion + Ocupacion + generacion, family=binomial(link='logit'),data=conjoint2)
summary(model13)


stargazer(model11, model12, model13, type="html", out="modelos_controles.html")


########### Creación subset (_ND: Nativo Digial, _no: No Nativo Digital, _h: Hombres, _m: Mujeres)

conjoint2_ND<-subset(conjoint2, Nativo_D==1)
conjoint2_no<-subset(conjoint2, Nativo_D==0)
conjoint2_h<-subset(conjoint2, hombre=="1")
conjoint2_m<-subset(conjoint2, hombre=="0")


########### Model estimations brand selection Genero (Hombre)

model1h <- lrm(universidad_select_reputacion~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint2_h,  x=T, y=T)

model1h.cl<-bootcov(model1h,cluster=conjoint2_h$id)

stargazer(model1h.cl, type="html", out="model1h_cl_C2.html")


########### Model estimations brand selection Genero (Mujer)

model1m <- lrm(universidad_select_reputacion~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint2_m,  x=T, y=T)

model1m.cl<-bootcov(model1m,cluster=conjoint2_m$id)

stargazer(model1m.cl, type="html", out="model1m_cl_C2.html")


########### Model estimations brand selection Generacion (Nativo)

model1ND <- lrm(universidad_select_reputacion~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint2_ND,  x=T, y=T)

model1ND.cl<-bootcov(model1ND,cluster=conjoint2_ND$id)

stargazer(model1ND.cl, type="html", out="model1ND_cl_C2.html")


########### Model estimations brand selection Genero (NoNativo)

model1no <- lrm(universidad_select_reputacion~ prensa+corporativa+ twit_contacto + twit_influencer,data=conjoint2_no,  x=T, y=T)

model1no.cl<-bootcov(model1no,cluster=conjoint2_no$id)

stargazer(model1no.cl, type="html", out="model1no_cl_C2.html")


########### Model estimations brand selection todos
stargazer(model1h.cl, model1m.cl, model1ND.cl, model1no.cl, type="text", out="modeloss_cl_C2.txt")




#######################
### FindIt estimations          
#######################



### FindIt estimations Base Total

conjoint2$prensa<- factor(conjoint2$prensa,ordered=T,levels=c("La prensa destaca su alta productividad cientifica", 
                                                              "La prensa no ofrece noticias sobre esta universidad", 
                                                              "La prensa indica que su productividad cientifica esta en el promedio nacional"))

conjoint2$corporativa <-factor(conjoint2$corporativa, ordered = T, levels = c("Su pagina web indica que su nivel academico es alto", 
                                                                              "Su pagina web no presenta informacion corporativa", 
                                                                              "Su pagina web indica que su nivel academico esta en el promedio nacional"))

conjoint2$twit_contacto <-factor(conjoint2$twit_contacto, ordered = T, levels = c( "Felicita la acreditacion de 7 anos obtenida por la universidad", 
                                                                                   "Comenta que esta universidad tiene acreditacion de 3 anos", 
                                                                                   "Comparte articulo sobre universidades acreditadas por 5 anos que incluye a esta"))

conjoint2$twit_influencer<-factor(conjoint2$twit_influencer, ordered = T, levels = c("Relata que la calidad academica de esta Universidad es buena", 
                                                                                     "Relata una mala experiencia academica con esta universidad",  
                                                                                     "Relata que la calidad academica de esta universidad es regular"))



fit1 <- CausalANOVA(formula=universidad_select_reputacion ~ prensa+corporativa+ twit_contacto + twit_influencer,
                    data=conjoint2, cluster=conjoint2$id, nway=1)
summary(fit1)
plot(fit1)

plot(fit1,
     cex.lab=0.2,
     axes=FALSE)


png(filename = "loan_age.png", width = 900, height = 480)
plot(fit1)
dev.off()




# Include code to replace standard errors with robust S.Es
robustse.f <- function(model, cluster, df_correction) {
  ## Huber-White heteroskedasticity-robust standard error calculation and table generation code for lm and glm models in R.
  ##Written by Joshua Gubler ~  http://joshuagubler.com.  Note that the second half of this function is just a wrapper for the excellent "multiwaycov" package here: https://cran.r-project.org/web/packages/multiwayvcov/multiwayvcov.pdf .  Love the authors of that package...
  ##Last updated: 3 July 2017  
  
  #model = the model you estimated, now calculated with robust standard errors
  #cluster = the name of the variable on which you will cluster. Put a tilda in front of it (e.g. ~ state).  If you don't put in a cluster, you will simply get huber-white robust errors.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  if(missing(cluster)) {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    model$se <- coeftest(model, vcov=vcovHC(model,"HC1"))[,2]
    model$vcovHC <- vcovHC(model,"HC1")
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcov=vcovHC(model,"HC1"))
  } else {
    name <- deparse(substitute(model))
    vcovCL <- cluster.vcov(model, cluster, df_correction = df_correction)
    model$vcovCL <- vcovCL
    modelname <- paste(name,"clustrob",sep=".")
    model$se <- coeftest(model, vcovCL)[,2]
    assign(modelname,model,envir = .GlobalEnv)
    #coeftest(model, vcovCL)
  }
}


# Logit models
model1 <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1)
model1_controls <- glm(destination ~ social + econ + service + immigration + education + age + gender + ideology + likely,family=binomial(link='logit'),data=conjoint1)


model61 <- glm(universidad_select_reputacion ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2)
model61_controls <- glm(universidad_select_reputacion ~ prensa+corporativa+ twit_contacto + twit_influencer + 
                          Nativo_D + generacion + hombre + nivel_estudios + tipo_institución + actividad,family=binomial(link='logit'),data=conjoint2)

model62 <- glm(universidad_select_confianza ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2)
model62_controls <- glm(universidad_select_confianza ~ prensa+corporativa+ twit_contacto + twit_influencer + 
                          Nativo_D + generacion + hombre + nivel_estudios + tipo_institución + actividad,family=binomial(link='logit'),data=conjoint2)

model63 <- glm(universidad_select_prestigio ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2)
model63_controls <- glm(universidad_select_prestigio ~ prensa+corporativa+ twit_contacto + twit_influencer + 
                          Nativo_D + generacion + hombre + nivel_estudios + tipo_institución + actividad,family=binomial(link='logit'),data=conjoint2)




model2 <- glm(destination ~ social + econ + service + immigration + education+ likely,family=binomial(link='logit'),data=conjoint2)
model2_controls <- glm(destination ~ social + econ + service + immigration + education + age + gender + ideology + likely,family=binomial(link='logit'),data=conjoint2)

model3 <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3)
model3_controls <- glm(destination ~ social + econ + service + country + education + age + gender + ideology + likely,family=binomial(link='logit'),data=conjoint3)
model3_controls_same1 <- glm(destination ~ social + econ + service + country + education + age + gender + ideology + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$c.same == 1,])
model3_controls_same0 <- glm(destination ~ social + econ + service + country + education + age + gender + ideology + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$c.same == 0,])


# Cluster-robust standard errors
robustse.f(model61, ~id, F)
robustse.f(model61_controls, ~id, F)
robustse.f(model62, ~id, F)
robustse.f(model62_controls, ~id, F)
robustse.f(model63, ~id, F)
robustse.f(model63_controls, ~id, F)
robustse.f(model3_controls_same0, ~id, F)
robustse.f(model3_controls_same1, ~id, F)


#Model 61
plotdf61 <- data.frame(estimate = coeftest(model61.clustrob, model61.clustrob$vcovCL)[,1],SE = coeftest(model61.clustrob, model61.clustrob$vcovCL)[,2])
plotdf61 <- plotdf61[-1,]
plotdf61 <- rbind(plotdf61[1,],
                  c(0,0),
                  plotdf61[1:8,])
plotdf61 <- rbind(plotdf61[1:3,],
                  c(0,0),
                  plotdf61[4:9,])
plotdf61 <- rbind(plotdf61[1:6,],
                  c(0,0),
                  plotdf61[7:10,])
plotdf61 <- rbind(plotdf61[1:9,],
                  c(0,0),
                  plotdf61[10:11,])

plotdf61$coef <- c("prensaLa prensa destaca su alta productividad cientifica", 
                   "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                   "prensaLa prensa no ofrece noticias sobre esta universidad",
                   "corporativaSu pagina web indica que su nivel academico es alto",
                   "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                   "corporativaSu pagina web no presenta informacion corporativa",
                   "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                   "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                   "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                   "twit_influencerRelata que la calidad academica de esta Universidad es buena", 
                   "twit_influencerRelata que la calidad academica de esta universidad es regular",
                   "twit_influencerRelata una mala experiencia academica con esta universidad"
)

plotdf61$coef <- factor(plotdf61$coef, levels = c("twit_influencerRelata una mala experiencia academica con esta universidad",
                                                  "twit_influencerRelata que la calidad academica de esta universidad es regular",
                                                  "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                                                  "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                                                  "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                                                  "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                                                  "corporativaSu pagina web no presenta informacion corporativa",
                                                  "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                                                  "corporativaSu pagina web indica que su nivel academico es alto",
                                                  "prensaLa prensa no ofrece noticias sobre esta universidad",
                                                  "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                                                  "prensaLa prensa destaca su alta productividad cientifica",
                                                  "Intercept"))


plotdf61$LCI <- plotdf61$estimate-1.96*plotdf61$SE
plotdf61$UCI <- plotdf61$estimate+1.96*plotdf61$SE



ggplot(plotdf61, aes(x=coef)) +
  geom_point(y=plotdf61$estimate) +
  geom_linerange(max=plotdf61$UCI, min=plotdf61$LCI) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylim(-1.5,1.5) +
  labs(x="",y="") +
  coord_flip()

ggsave(paste0("conjoint2_61.png" ,""), width = 35, height = 20, units = c("cm"), dpi = 300)




#Model 62
plotdf62 <- data.frame(estimate = coeftest(model62.clustrob, model62.clustrob$vcovCL)[,1],SE = coeftest(model62.clustrob, model62.clustrob$vcovCL)[,2])
plotdf62 <- plotdf62[-1,]
plotdf62 <- rbind(plotdf62[1,],
                  c(0,0),
                  plotdf62[1:8,])
plotdf62 <- rbind(plotdf62[1:3,],
                  c(0,0),
                  plotdf62[4:9,])
plotdf62 <- rbind(plotdf62[1:6,],
                  c(0,0),
                  plotdf62[7:10,])
plotdf62 <- rbind(plotdf62[1:9,],
                  c(0,0),
                  plotdf62[10:11,])

plotdf62$coef <- c("prensaLa prensa destaca su alta productividad cientifica", 
                   "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                   "prensaLa prensa no ofrece noticias sobre esta universidad",
                   "corporativaSu pagina web indica que su nivel academico es alto",
                   "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                   "corporativaSu pagina web no presenta informacion corporativa",
                   "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                   "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                   "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                   "twit_influencerRelata que la calidad academica de esta Universidad es buena", 
                   "twit_influencerRelata que la calidad academica de esta universidad es regular",
                   "twit_influencerRelata una mala experiencia academica con esta universidad"
)

plotdf62$coef <- factor(plotdf62$coef, levels = c("twit_influencerRelata una mala experiencia academica con esta universidad",
                                                  "twit_influencerRelata que la calidad academica de esta universidad es regular",
                                                  "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                                                  "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                                                  "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                                                  "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                                                  "corporativaSu pagina web no presenta informacion corporativa",
                                                  "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                                                  "corporativaSu pagina web indica que su nivel academico es alto",
                                                  "prensaLa prensa no ofrece noticias sobre esta universidad",
                                                  "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                                                  "prensaLa prensa destaca su alta productividad cientifica",
                                                  "Intercept"))


plotdf62$LCI <- plotdf62$estimate-1.96*plotdf62$SE
plotdf62$UCI <- plotdf62$estimate+1.96*plotdf62$SE



ggplot(plotdf62, aes(x=coef)) +
  geom_point(y=plotdf62$estimate) +
  geom_linerange(max=plotdf62$UCI, min=plotdf62$LCI) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylim(-1.5,1.5) +
  labs(x="",y="") +
  coord_flip()

ggsave(paste0("conjoint2_62.png" ,""), width = 30, height = 20, units = c("cm"), dpi = 300)


#Model 63
plotdf63 <- data.frame(estimate = coeftest(model63.clustrob, model63.clustrob$vcovCL)[,1],SE = coeftest(model63.clustrob, model63.clustrob$vcovCL)[,2])
plotdf63 <- plotdf63[-1,]
plotdf63 <- rbind(plotdf63[1,],
                  c(0,0),
                  plotdf63[1:8,])
plotdf63 <- rbind(plotdf63[1:3,],
                  c(0,0),
                  plotdf63[4:9,])
plotdf63 <- rbind(plotdf63[1:6,],
                  c(0,0),
                  plotdf63[7:10,])
plotdf63 <- rbind(plotdf63[1:9,],
                  c(0,0),
                  plotdf63[10:11,])

plotdf63$coef <- c("prensaLa prensa destaca su alta productividad cientifica", 
                   "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                   "prensaLa prensa no ofrece noticias sobre esta universidad",
                   "corporativaSu pagina web indica que su nivel academico es alto",
                   "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                   "corporativaSu pagina web no presenta informacion corporativa",
                   "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                   "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                   "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                   "twit_influencerRelata que la calidad academica de esta Universidad es buena", 
                   "twit_influencerRelata que la calidad academica de esta universidad es regular",
                   "twit_influencerRelata una mala experiencia academica con esta universidad"
)

plotdf63$coef <- factor(plotdf63$coef, levels = c("twit_influencerRelata una mala experiencia academica con esta universidad",
                                                  "twit_influencerRelata que la calidad academica de esta universidad es regular",
                                                  "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                                                  "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                                                  "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                                                  "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                                                  "corporativaSu pagina web no presenta informacion corporativa",
                                                  "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                                                  "corporativaSu pagina web indica que su nivel academico es alto",
                                                  "prensaLa prensa no ofrece noticias sobre esta universidad",
                                                  "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                                                  "prensaLa prensa destaca su alta productividad cientifica",
                                                  "Intercept"))


plotdf63$LCI <- plotdf63$estimate-1.96*plotdf63$SE
plotdf63$UCI <- plotdf63$estimate+1.96*plotdf63$SE



ggplot(plotdf63, aes(x=coef)) +
  geom_point(y=plotdf63$estimate) +
  geom_linerange(max=plotdf63$UCI, min=plotdf63$LCI) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylim(-1.5,1.5) +
  labs(x="",y="") +
  coord_flip()

ggsave(paste0("conjoint2_63.png" ,""), width = 30, height = 20, units = c("cm"), dpi = 300)




# Combine into one plot

plotdf661 <- cbind(plotdf61,treatment = "Reputacion")
plotdf662 <- cbind(plotdf62,treatment = "Confianza")
plotdf663 <- cbind(plotdf63,treatment = "Prestigio")

plotdf <- rbind(plotdf661,plotdf662,plotdf663)
plotdf$coef <- factor(plotdf$coef, levels = c("twit_influencerRelata una mala experiencia academica con esta universidad",
                                              "twit_influencerRelata que la calidad academica de esta universidad es regular",
                                              "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                                              "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                                              "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                                              "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                                              "corporativaSu pagina web no presenta informacion corporativa",
                                              "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                                              "corporativaSu pagina web indica que su nivel academico es alto",
                                              "prensaLa prensa no ofrece noticias sobre esta universidad",
                                              "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                                              "prensaLa prensa destaca su alta productividad cientifica",
                                              "Intercept"))

ggplot(data = plotdf, aes(x=coef)) +
  facet_grid(.~treatment) +
  geom_point(aes(y=estimate)) +
  geom_linerange(aes(max=UCI, min=LCI)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="") +
  coord_flip()

ggsave(paste0("conjoint_combined.png"), width = 30, height = 20, units = c("cm"), dpi = 300)







## Generate models
# Set reference categories
# conjoint1 <- within(conjoint1, econ <- relevel(econ, ref = "Annual GDP Growth of 2%"))
# conjoint1 <- within(conjoint1, service <- relevel(service, ref = "Average international ranking of service salaries: 50th Percentile"))
# conjoint1 <- within(conjoint1, education <- relevel(education, ref = "Average international ranking of universities: 40th Percentile"))

# conjoint2 <- within(conjoint2, econ <- relevel(econ, ref = "Annual GDP Growth of 2%"))
# conjoint2 <- within(conjoint2, service <- relevel(service, ref = "Average international ranking of service salaries: 50th Percentile"))
# conjoint2 <- within(conjoint2, education <- relevel(education, ref = "Average international ranking of universities: 40th Percentile"))

# conjoint3 <- within(conjoint3, econ <- relevel(econ, ref = "Annual GDP Growth of 2%"))
# conjoint3 <- within(conjoint3, service <- relevel(service, ref = "Average international ranking of service salaries: 50th Percentile"))
# conjoint3 <- within(conjoint3, education <- relevel(education, ref = "Average international ranking of universities: 40th Percentile"))

# Logit models
model61_nat <- glm(universidad_select_reputacion ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 1,]) 
model61_non <- glm(universidad_select_reputacion ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 0,]) 

model62_nat <- glm(universidad_select_confianza ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 1,]) 
model62_non <- glm(universidad_select_confianza ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 0,]) 

model63_nat <- glm(universidad_select_prestigio ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 1,]) 
model63_non <- glm(universidad_select_prestigio ~ prensa+corporativa+ twit_contacto + twit_influencer,family=binomial(link='logit'),data=conjoint2[conjoint2$Nativo_D == 0,]) 



# Cluster-robust standard errors
robustse.f(model61_nat, ~id, F)
robustse.f(model61_non, ~id, F)
robustse.f(model62_nat, ~id, F)
robustse.f(model62_non, ~id, F)
robustse.f(model63_nat, ~id, F)
robustse.f(model63_non, ~id, F)



## Coef plot
# Nativo group
plotdf_nat <- data.frame(estimate = coeftest(model61_nat.clustrob, model61_nat.clustrob$vcovCL)[,1],SE = coeftest(model61_nat.clustrob, model61_nat.clustrob$vcovCL)[,2],group = "Nativo",treatment = "Reputacion")
plotdf_nat <- rbind(plotdf_nat, data.frame(estimate = coeftest(model62_nat.clustrob, model62_nat.clustrob$vcovCL)[,1],SE = coeftest(model62_nat.clustrob, model62_nat.clustrob$vcovCL)[,2],group = "Nativo",treatment = "Confianza"))
plotdf_nat <- rbind(plotdf_nat, data.frame(estimate = coeftest(model63_nat.clustrob, model63_nat.clustrob$vcovCL)[,1],SE = coeftest(model63_nat.clustrob, model63_nat.clustrob$vcovCL)[,2],group = "Nativo",treatment = "Prestigio"))

plotdf_nat <- rbind(plotdf_nat[1:2,],c(0,0,"Nativo","Reputacion"),plotdf_nat[3:27,])
plotdf_nat <- rbind(plotdf_nat[1:5,],c(0,0,"Nativo","Reputacion"),plotdf_nat[6:28,])
plotdf_nat <- rbind(plotdf_nat[1:8,],c(0,0,"Nativo","Reputacion"),plotdf_nat[9:29,])
plotdf_nat <- rbind(plotdf_nat[1:11,],c(0,0,"Nativo","Reputacion"),plotdf_nat[12:30,])
plotdf_nat <- rbind(plotdf_nat[1:15,],c(0,0,"Nativo","Confianza"),plotdf_nat[16:31,])
plotdf_nat <- rbind(plotdf_nat[1:18,],c(0,0,"Nativo","Confianza"),plotdf_nat[19:32,])
plotdf_nat <- rbind(plotdf_nat[1:21,],c(0,0,"Nativo","Confianza"),plotdf_nat[22:33,])
plotdf_nat <- rbind(plotdf_nat[1:24,],c(0,0,"Nativo","Confianza"),plotdf_nat[25:34,])
plotdf_nat <- rbind(plotdf_nat[1:28,],c(0,0,"Nativo","Prestigio"),plotdf_nat[29:35,])
plotdf_nat <- rbind(plotdf_nat[1:31,],c(0,0,"Nativo","Prestigio"),plotdf_nat[32:36,])
plotdf_nat <- rbind(plotdf_nat[1:34,],c(0,0,"Nativo","Prestigio"),plotdf_nat[35:37,])
plotdf_nat <- rbind(plotdf_nat[1:37,],c(0,0,"Nativo","Prestigio"),plotdf_nat[38:38,])


# No Nativo group
plotdf_non <- data.frame(estimate = coeftest(model61_non.clustrob, model61_non.clustrob$vcovCL)[,1],SE = coeftest(model61_non.clustrob, model61_non.clustrob$vcovCL)[,2],group = "No_Nativo",treatment = "Reputacion")
plotdf_non <- rbind(plotdf_non, data.frame(estimate = coeftest(model62_non.clustrob, model62_non.clustrob$vcovCL)[,1],SE = coeftest(model62_non.clustrob, model62_non.clustrob$vcovCL)[,2],group = "No_Nativo",treatment = "Confianza"))
plotdf_non <- rbind(plotdf_non, data.frame(estimate = coeftest(model63_non.clustrob, model63_non.clustrob$vcovCL)[,1],SE = coeftest(model63_non.clustrob, model63_non.clustrob$vcovCL)[,2],group = "No_Nativo",treatment = "Prestigio"))

plotdf_non <- rbind(plotdf_non[1:2,],c(0,0,"No_Nativo","Reputacion"),plotdf_non[3:27,])
plotdf_non <- rbind(plotdf_non[1:5,],c(0,0,"No_Nativo","Reputacion"),plotdf_non[6:28,])
plotdf_non <- rbind(plotdf_non[1:8,],c(0,0,"No_Nativo","Reputacion"),plotdf_non[9:29,])
plotdf_non <- rbind(plotdf_non[1:11,],c(0,0,"No_Nativo","Reputacion"),plotdf_non[12:30,])
plotdf_non <- rbind(plotdf_non[1:15,],c(0,0,"No_Nativo","Confianza"),plotdf_non[16:31,])
plotdf_non <- rbind(plotdf_non[1:18,],c(0,0,"No_Nativo","Confianza"),plotdf_non[19:32,])
plotdf_non <- rbind(plotdf_non[1:21,],c(0,0,"No_Nativo","Confianza"),plotdf_non[22:33,])
plotdf_non <- rbind(plotdf_non[1:24,],c(0,0,"No_Nativo","Confianza"),plotdf_non[25:34,])
plotdf_non <- rbind(plotdf_non[1:28,],c(0,0,"No_Nativo","Prestigio"),plotdf_non[29:35,])
plotdf_non <- rbind(plotdf_non[1:31,],c(0,0,"No_Nativo","Prestigio"),plotdf_non[32:36,])
plotdf_non <- rbind(plotdf_non[1:34,],c(0,0,"No_Nativo","Prestigio"),plotdf_non[35:37,])
plotdf_non <- rbind(plotdf_non[1:37,],c(0,0,"No_Nativo","Prestigio"),plotdf_non[38:38,])



plotdf <- rbind(plotdf_nat,plotdf_non)

plotdf$coef <- c("Intercept",
                 "prensaLa prensa no ofrece noticias sobre esta universidad", 
                 "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                 "prensaLa prensa destaca su alta productividad cientifica",
                 "corporativaSu pagina web no presenta informacion corporativa",
                 "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                 "corporativaSu pagina web indica que su nivel academico es alto",
                 "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                 "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                 "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                 "twit_influencerRelata una mala experiencia academica con esta universidad", 
                 "twit_influencerRelata que la calidad academica de esta universidad es regular",
                 "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                 "Intercept",
                 "prensaLa prensa no ofrece noticias sobre esta universidad", 
                 "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                 "prensaLa prensa destaca su alta productividad cientifica",
                 "corporativaSu pagina web no presenta informacion corporativa",
                 "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                 "corporativaSu pagina web indica que su nivel academico es alto",
                 "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                 "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                 "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                 "twit_influencerRelata una mala experiencia academica con esta universidad", 
                 "twit_influencerRelata que la calidad academica de esta universidad es regular",
                 "twit_influencerRelata que la calidad academica de esta Universidad es buena",
                 "Intercept",
                 "prensaLa prensa no ofrece noticias sobre esta universidad", 
                 "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                 "prensaLa prensa destaca su alta productividad cientifica",
                 "corporativaSu pagina web no presenta informacion corporativa",
                 "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                 "corporativaSu pagina web indica que su nivel academico es alto",
                 "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                 "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                 "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                 "twit_influencerRelata una mala experiencia academica con esta universidad", 
                 "twit_influencerRelata que la calidad academica de esta universidad es regular",
                 "twit_influencerRelata que la calidad academica de esta Universidad es buena")







plotdf$coef <- factor(plotdf$coef, levels = c("twit_influencerRelata que la calidad academica de esta Universidad es buena",
                                              "twit_influencerRelata que la calidad academica de esta universidad es regular",
                                              "twit_influencerRelata una mala experiencia academica con esta universidad",
                                              "twit_contactoFelicita la acreditacion de 7 anos obtenida por la universidad",
                                              "twit_contactoComparte articulo sobre universidades acreditadas por 5 anos que incluye a esta",
                                              "twit_contactoComenta que esta universidad tiene acreditacion de 3 anos",
                                              "corporativaSu pagina web indica que su nivel academico es alto",
                                              "corporativaSu pagina web indica que su nivel academico esta en el promedio nacional",
                                              "corporativaSu pagina web no presenta informacion corporativa",
                                              "prensaLa prensa destaca su alta productividad cientifica",
                                              "prensaLa prensa indica que su productividad cientifica esta en el promedio nacional",
                                              "prensaLa prensa no ofrece noticias sobre esta universidad",
                                              "Intercept"))

plotdf <- plotdf[plotdf$coef != "Intercept",]
plotdf$estimate <- as.numeric(plotdf$estimate)
plotdf$SE <- as.numeric(plotdf$SE)
plotdf$LCI <- plotdf$estimate-1.96*plotdf$SE
plotdf$UCI <- plotdf$estimate+1.96*plotdf$SE

plotdf$treatment <- factor(paste0("Treatment: ",plotdf$treatment))
plotdf$group <- factor(plotdf$group, levels=c("Nativo","No_Nativo"))

ggplot(data = plotdf, aes(x=coef, group=group)) +
  facet_grid(.~treatment) +
  geom_point(aes(y=estimate, colour = factor(group)),position=position_dodge(width = 0.7)) +
  geom_linerange(aes(max=UCI, min=LCI,colour = factor(group)),position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_colour_manual(values=c("red","blue")) +
  labs(x="",y="") +
  theme(legend.position="bottom") +
  guides(colour=guide_legend(title="")) +
  coord_flip()

ggsave(paste0("conjoint_Nat_NoNat.png"), width = 30, height = 20, units = c("cm"), dpi = 300)





######################################
#### Balance tests 
######################################


#### Balance tests #####
bal_prensa <- multinom(prensa ~ hombre + Educacion + Ocupacion + generacion, data = conjoint)
bal_corporativa <- multinom(corporativa ~ hombre + Educacion + Ocupacion + generacion, data = conjoint)
bal_twit_contacto <- multinom(twit_contacto ~ hombre + Educacion + Ocupacion + generacion, data = conjoint)
bal_twit_influencer <- multinom(twit_influencer ~ hombre + Educacion + Ocupacion + generacion, data = conjoint)

stargazer(bal_prensa, type="html", out="balance_prensa.html")
stargazer(bal_corporativa, type="html", out="balance_corporativa.html")
stargazer(bal_twit_contacto, type="html", out="balance_twit_contacto.html")
stargazer(bal_twit_influencer, type="html", out="balance_twit_influencer.html")















############
### Figures
############




