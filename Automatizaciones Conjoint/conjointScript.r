### creación de php

## variables explicativas
e<-c("Situación laboral", "Familia de origen", "Relación con contratante","Competencias")

## condiciones
f1 <- c("Se encuentra sin trabajo", "Tiene trabajo")  #necesidad 
f2<- c('Acomodada', 'Clase media', 'Modesta') #origen social
f3<- c('Lo conoce personalmente', 'Conocido de amigos','No lo conoce') # referencia
f4<- c('Pertinentes al cargo','Poco pertinentes al cargo') #competencias

niv<-list(f1, f2, f3, f4) #niveles de factores explicativas

## genera lista de factores entrecomillas (g1 a g4)
for (i in 1:length(niv)){
g<-paste0("g",i)    
h<-assign(g, paste0(niv[[i]], collapse='","', sep=""))
}

## genera nombres variables
for (j in 1:length(niv)){
    k<-paste0("k",j)
l<-assign(k, paste0('array("',e[j], '" => array("', sep=''))
}

##junta variables y niveles para crear php
paste('$featurearray = ', k1, g1,'), ', k2, g2, '), ', k3, g3, '), ', k4, g4, ');', sep='')


### creación tablas html
a<-rep(1, 4)
b<-rep(2, 4)
c<-(1:4)


## código html para primera iteración
t1<-cbind(paste('R', '1',c, sep='-'),
paste('R', '1',a,c, sep='-'),
paste('R', '1',b,c, sep='-'))

for (j in 1:4){
for (i in 1:3){
    kt<-paste0("kt",j,i)    
    ct<-assign(kt, paste0("<td style='text-align: center;'>${e://Field/", t1[j,i], "}</td>", sep=""))
}
l<-paste0("l", j)
r<-assign(l, paste0(kt11))
}

## tabla html para primera iteración 
paste0('<table class="UserTable"><tbody><tr><td>&nbsp;</td><td style="text-align: center;"><strong>Postulante A</strong></td><td style="text-align: center;"><strong>Postulante B</strong></td></tr><tr>', paste0(kt11,kt12,kt13, '</tr>'),
paste0(kt21,kt22,kt23, '</tr>'),
paste0(kt31,kt32,kt33, '</tr>'),
paste0(kt41,kt42,kt43), '</tr></tr></tbody></table>')

## código html para segunda iteración
t1<-cbind(paste('R', '2',c, sep='-'),
paste('R', '2',a,c, sep='-'),
paste('R', '2',b,c, sep='-'))

for (j in 1:4){
for (i in 1:3){
    kt<-paste0("kt",j,i)    
    ct<-assign(kt, paste0("<td style='text-align: center;'>${e://Field/", t1[j,i], "}</td>", sep=""))
}
l<-paste0("l", j)
r<-assign(l, paste0(kt11))
}

## tabla html para segunda iteración 
paste0('<table class="UserTable"><tbody><tr><td>&nbsp;</td><td style="text-align: center;"><strong>Postulante A</strong></td><td style="text-align: center;"><strong>Postulante B</strong></td></tr><tr>', paste0(kt11,kt12,kt13, '</tr>'),
paste0(kt21,kt22,kt23, '</tr>'),
paste0(kt31,kt32,kt33, '</tr>'),
paste0(kt41,kt42,kt43), '</tr></tr></tbody></table>')


## código html para tercera iteración
t1<-cbind(paste('R', '3',c, sep='-'),
paste('R', '3',a,c, sep='-'),
paste('R', '3',b,c, sep='-'))

for (j in 1:4){
for (i in 1:3){
    kt<-paste0("kt",j,i)    
    ct<-assign(kt, paste0("<td style='text-align: center;'>${e://Field/", t1[j,i], "}</td>", sep=""))
}
l<-paste0("l", j)
r<-assign(l, paste0(kt11))
}

## tabla html para tercera iteración 
paste0('<table class="UserTable"><tbody><tr><td>&nbsp;</td><td style="text-align: center;"><strong>Postulante A</strong></td><td style="text-align: center;"><strong>Postulante B</strong></td></tr><tr>', paste0(kt11,kt12,kt13, '</tr>'),
paste0(kt21,kt22,kt23, '</tr>'),
paste0(kt31,kt32,kt33, '</tr>'),
paste0(kt41,kt42,kt43), '</tr></tr></tbody></table>')

## código html para cuarta iteración
t1<-cbind(paste('R', '4',c, sep='-'),
paste('R', '4',a,c, sep='-'),
paste('R', '4',b,c, sep='-'))

for (j in 1:4){
for (i in 1:3){
    kt<-paste0("kt",j,i)    
    ct<-assign(kt, paste0("<td style='text-align: center;'>${e://Field/", t1[j,i], "}</td>", sep=""))
}
l<-paste0("l", j)
r<-assign(l, paste0(kt11))
}

## tabla html para cuarta iteración 
paste0('<table class="UserTable"><tbody><tr><td>&nbsp;</td><td style="text-align: center;"><strong>Postulante A</strong></td><td style="text-align: center;"><strong>Postulante B</strong></td></tr><tr>', paste0(kt11,kt12,kt13, '</tr>'),
paste0(kt21,kt22,kt23, '</tr>'),
paste0(kt31,kt32,kt33, '</tr>'),
paste0(kt41,kt42,kt43), '</tr></tr></tbody></table>')

