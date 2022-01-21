
##############################################
### Codigo de muestra de manejo de datos
### Autor Milton Inostroza
### 2021
##############################################


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


#setwd("/Users/miltoninos/Desktop/Modulo Proyecto Tesis/Codigos Conjoint/Experimento/Resultados")

df<-read.csv("base_C2MI_muestra.csv",sep=";",head=T)

#base_C2MI_muestra.csv

save(df, file ="base_CMI2_nativo_enbruto.Rdata")


#ej<-read.Rdata("conjoint1.Rdata")




###########################################
### reshape data
###########################################



vars<-c("X1_Reputacion", "X1_Confianza", "X1_Prestigio", "X1_Q_NivelPres_UX", "X1_Q_NivelPres_UZ",
        "X2_Reputacion", "X2_Confianza", "X2_Prestigio", "X2_Q_NivelPres_UX", "X2_Q_NivelPres_UZ",
        "X3_Reputacion", "X3_Confianza", "X3_Prestigio", "X3_Q_NivelPres_UX", "X3_Q_NivelPres_UZ",
        "X4_Reputacion", "X4_Confianza", "X4_Prestigio", "X4_Q_NivelPres_UX", "X4_Q_NivelPres_UZ",
        "X5_Reputacion", "X5_Confianza", "X5_Prestigio", "X5_Q_NivelPres_UX", "X5_Q_NivelPres_UZ")



dfl<-reshape(df, direction ="long", 
             varying = vars, 
             timevar = "variant",
             times = c("X1", "X2", "X3", "X4", "X5"),
             v.names = c("Reputacion", "Confianza", "Prestigio","Q_NivelPres_UX", "Q_NivelPres_UZ" ),
             idvar = "ResponseId"
)



dfl <- dfl[order(dfl$ResponseId, dfl$variant), ]

master<-df


#save(dfl, file= "/Users/miltoninos/Desktop/Modulo Proyecto Tesis/Codigos Conjoint/Piloto/Pilotoconjoint_MI.Rdata")

#################################
#### Conjoint 1 data structuring
#################################

##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.1.1 == attribute,as.character(master$I.1.1.1),
                         ifelse(master$I.1.2 == attribute,as.character(master$I.1.1.2),
                                ifelse(master$I.1.3 == attribute,as.character(master$I.1.1.3), as.character(master$I.1.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.1.1 == attribute,as.character(master$I.1.2.1),
                         ifelse(master$I.1.2 == attribute,as.character(master$I.1.2.2),
                                ifelse(master$I.1.3 == attribute,as.character(master$I.1.2.3),as.character(master$I.1.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

prensa <- df$Level[df$Attribute == "Información de prensa"]
corporativa <- df$Level[df$Attribute == "Información corporativa"]
twit_contacto <- df$Level[df$Attribute == "Twit de uno de tus contactos de redes sociales:"]
twit_influencer <- df$Level[df$Attribute == "Twit de reconocido influencer"]
universidad <- df$Candidate[df$Attribute == "Twit de reconocido influencer"]
universidad_select_reputacion <- ifelse((master$X1_Reputacion == "universidad X" & universidad == 1) | (master$X1_Reputacion == "universidad Z" & universidad == 2),1,0)
universidad_select_confianza <- ifelse((master$X1_Confianza == "universidad X" & universidad == 1) | (master$X1_Confianza == "universidad Z" & universidad == 2),1,0)
universidad_select_prestigio <- ifelse((master$X1_Prestigio == "universidad X" & universidad == 1) | (master$X1_Prestigio == "universidad Z" & universidad == 2),1,0)
universidad_Xpref<-master$X1_Q_NivelPres_UX
universidad_Zpref<-master$X1_Q_NivelPres_UZ

hombre<-master$Hombre
edad<-master$PD1
Nativo_D<-master$Nativo_D
nivel_estudios<-master$PD3
área_estudios<-master$PD3_1
tipo_institución<-master$PD3_2
id<-master$ResponseId

conjoint1a <- data.frame(id, prensa, corporativa, twit_contacto, twit_influencer, universidad, universidad_select_reputacion, universidad_select_confianza, universidad_select_prestigio, universidad_Xpref, universidad_Zpref, hombre, edad, Nativo_D, nivel_estudios, área_estudios, tipo_institución)



##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.2.1 == attribute,as.character(master$I.2.1.1),
                         ifelse(master$I.1.2 == attribute,as.character(master$I.2.1.2),
                                ifelse(master$I.2.3 == attribute,as.character(master$I.1.1.3), as.character(master$I.2.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.2.1 == attribute,as.character(master$I.2.2.1),
                         ifelse(master$I.2.2 == attribute,as.character(master$I.2.2.2),
                                ifelse(master$I.2.3 == attribute,as.character(master$I.2.2.3),as.character(master$I.2.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

prensa <- df$Level[df$Attribute == "Información de prensa"]
corporativa <- df$Level[df$Attribute == "Información corporativa"]
twit_contacto <- df$Level[df$Attribute == "Twit de uno de tus contactos de redes sociales:"]
twit_influencer <- df$Level[df$Attribute == "Twit de reconocido influencer"]
universidad <- df$Candidate[df$Attribute == "Twit de reconocido influencer"]
universidad_select_reputacion <- ifelse((master$X2_Reputacion == "universidad X" & universidad == 1) | (master$X2_Reputacion == "universidad Z" & universidad == 2),1,0)
universidad_select_confianza <- ifelse((master$X2_Confianza == "universidad X" & universidad == 1) | (master$X2_Confianza == "universidad Z" & universidad == 2),1,0)
universidad_select_prestigio <- ifelse((master$X2_Prestigio == "universidad X" & universidad == 1) | (master$X2_Prestigio == "universidad Z" & universidad == 2),1,0)
universidad_Xpref<-master$X2_Q_NivelPres_UX
universidad_Zpref<-master$X2_Q_NivelPres_UZ




hombre<-master$Hombre
edad<-master$PD1
Nativo_D<-master$Nativo_D
nivel_estudios<-master$PD3
área_estudios<-master$PD3_1
tipo_institución<-master$PD3_2
id<-master$ResponseId

conjoint1b <- data.frame(id, prensa, corporativa, twit_contacto, twit_influencer, universidad, universidad_select_reputacion, universidad_select_confianza, universidad_select_prestigio, universidad_Xpref, universidad_Zpref, hombre, edad, Nativo_D, nivel_estudios, área_estudios, tipo_institución)


##CJ1c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.3.1 == attribute,as.character(master$I.3.1.1),
                         ifelse(master$I.3.2 == attribute,as.character(master$I.3.1.2),
                                ifelse(master$I.3.3 == attribute,as.character(master$I.3.1.3), as.character(master$I.3.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.3.1 == attribute,as.character(master$I.3.2.1),
                         ifelse(master$I.3.2 == attribute,as.character(master$I.3.2.2),
                                ifelse(master$I.3.3 == attribute,as.character(master$I.3.2.3),as.character(master$I.3.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

prensa <- df$Level[df$Attribute == "Información de prensa"]
corporativa <- df$Level[df$Attribute == "Información corporativa"]
twit_contacto <- df$Level[df$Attribute == "Twit de uno de tus contactos de redes sociales:"]
twit_influencer <- df$Level[df$Attribute == "Twit de reconocido influencer"]
universidad <- df$Candidate[df$Attribute == "Twit de reconocido influencer"]
universidad_select_reputacion <- ifelse((master$X3_Reputacion == "universidad X" & universidad == 1) | (master$X3_Reputacion == "universidad Z" & universidad == 2),1,0)
universidad_select_confianza <- ifelse((master$X3_Confianza == "universidad X" & universidad == 1) | (master$X3_Confianza == "universidad Z" & universidad == 2),1,0)
universidad_select_prestigio <- ifelse((master$X3_Prestigio == "universidad X" & universidad == 1) | (master$X3_Prestigio == "universidad Z" & universidad == 2),1,0)
universidad_Xpref<-master$X3_Q_NivelPres_UX
universidad_Zpref<-master$X3_Q_NivelPres_UZ




hombre<-master$Hombre
edad<-master$PD1
Nativo_D<-master$Nativo_D
nivel_estudios<-master$PD3
área_estudios<-master$PD3_1
tipo_institución<-master$PD3_2
id<-master$ResponseId

conjoint1c <- data.frame(id, prensa, corporativa, twit_contacto, twit_influencer, universidad, universidad_select_reputacion, universidad_select_confianza, universidad_select_prestigio, universidad_Xpref, universidad_Zpref, hombre, edad, Nativo_D, nivel_estudios, área_estudios, tipo_institución)





##CJ1d
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.4.1 == attribute,as.character(master$I.4.1.1),
                         ifelse(master$I.4.2 == attribute,as.character(master$I.4.1.2),
                                ifelse(master$I.4.3 == attribute,as.character(master$I.4.1.3), as.character(master$I.4.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.4.1 == attribute,as.character(master$I.4.2.1),
                         ifelse(master$I.4.2 == attribute,as.character(master$I.4.2.2),
                                ifelse(master$I.4.3 == attribute,as.character(master$I.4.2.3),as.character(master$I.4.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

prensa <- df$Level[df$Attribute == "Información de prensa"]
corporativa <- df$Level[df$Attribute == "Información corporativa"]
twit_contacto <- df$Level[df$Attribute == "Twit de uno de tus contactos de redes sociales:"]
twit_influencer <- df$Level[df$Attribute == "Twit de reconocido influencer"]
universidad <- df$Candidate[df$Attribute == "Twit de reconocido influencer"]
universidad_select_reputacion <- ifelse((master$X4_Reputacion == "universidad X" & universidad == 1) | (master$X4_Reputacion == "universidad Z" & universidad == 2),1,0)
universidad_select_confianza <- ifelse((master$X4_Confianza == "universidad X" & universidad == 1) | (master$X4_Confianza == "universidad Z" & universidad == 2),1,0)
universidad_select_prestigio <- ifelse((master$X4_Prestigio == "universidad X" & universidad == 1) | (master$X4_Prestigio == "universidad Z" & universidad == 2),1,0)
universidad_Xpref<-master$X4_Q_NivelPres_UX
universidad_Zpref<-master$X4_Q_NivelPres_UZ




hombre<-master$Hombre
edad<-master$PD1
Nativo_D<-master$Nativo_D
nivel_estudios<-master$PD3
área_estudios<-master$PD3_1
tipo_institución<-master$PD3_2
id<-master$ResponseId

conjoint1d <- data.frame(id, prensa, corporativa, twit_contacto, twit_influencer, universidad, universidad_select_reputacion, universidad_select_confianza, universidad_select_prestigio, universidad_Xpref, universidad_Zpref, hombre, edad, Nativo_D, nivel_estudios, área_estudios, tipo_institución)




##CJ1e
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.5.1 == attribute,as.character(master$I.5.1.1),
                         ifelse(master$I.5.2 == attribute,as.character(master$I.5.1.2),
                                ifelse(master$I.5.3 == attribute,as.character(master$I.5.1.3), as.character(master$I.5.1.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Información de prensa", "Información corporativa", "Twit de uno de tus contactos de redes sociales:", "Twit de reconocido influencer" )) {
  level <- factor(ifelse(master$I.5.1 == attribute,as.character(master$I.5.2.1),
                         ifelse(master$I.5.2 == attribute,as.character(master$I.5.2.2),
                                ifelse(master$I.5.3 == attribute,as.character(master$I.5.2.3),as.character(master$I.5.2.4)))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

prensa <- df$Level[df$Attribute == "Información de prensa"]
corporativa <- df$Level[df$Attribute == "Información corporativa"]
twit_contacto <- df$Level[df$Attribute == "Twit de uno de tus contactos de redes sociales:"]
twit_influencer <- df$Level[df$Attribute == "Twit de reconocido influencer"]
universidad <- df$Candidate[df$Attribute == "Twit de reconocido influencer"]
universidad_select_reputacion <- ifelse((master$X5_Reputacion == "universidad X" & universidad == 1) | (master$X5_Reputacion == "universidad Z" & universidad == 2),1,0)
universidad_select_confianza <- ifelse((master$X5_Confianza == "universidad X" & universidad == 1) | (master$X5_Confianza == "universidad Z" & universidad == 2),1,0)
universidad_select_prestigio <- ifelse((master$X5_Prestigio == "universidad X" & universidad == 1) | (master$X5_Prestigio == "universidad Z" & universidad == 2),1,0)
universidad_Xpref<-master$X5_Q_NivelPres_UX
universidad_Zpref<-master$X5_Q_NivelPres_UZ




hombre<-master$Hombre
edad<-master$PD1
Nativo_D<-master$Nativo_D
nivel_estudios<-master$PD3
área_estudios<-master$PD3_1
tipo_institución<-master$PD3_2
id<-master$ResponseId

conjoint1e <- data.frame(id, prensa, corporativa, twit_contacto, twit_influencer, universidad, universidad_select_reputacion, universidad_select_confianza, universidad_select_prestigio, universidad_Xpref, universidad_Zpref, hombre, edad, Nativo_D, nivel_estudios, área_estudios, tipo_institución)


#### Gen one DF
conjoint1N <- rbind(conjoint1a,conjoint1b,conjoint1c,conjoint1d,conjoint1e)


rm(conjoint1a,conjoint1b,conjoint1c,conjoint1d,conjoint1e)

save(conjoint1N, file ="conjoint1N.Rdata")


