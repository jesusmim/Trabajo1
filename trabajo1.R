rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
setwd("C:/Users/jesus/Trabajo/")
Datososignermin2018<-read.table("201803_TABLA04_SICLI.txt",header = T,sep = "\t",col.names=c("CodEmpresa","Suministro","PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),colClasses = c("factor","factor","factor","character","numeric","numeric","character"))
Datososignermin2018$Fechadate<-ymd_hm(Datososignermin2018$Fecha)
Datososignermin2018<-select(Datososignermin2018,"CodEmpresa","Suministro","PuntoSuministro","RegistroActiva","RegistroPasiva","Periodo","Fechadate")
View(Datososignermin2018)

####Separacion de Fechas por dias, minutos y segundos####
Datososignermin2018$Año<-year(Datososignermin2018$Fechadate)
Datososignermin2018$Dia<-day(Datososignermin2018$Fechadate)
Datososignermin2018$Hora<-hour(Datososignermin2018$Fechadate)
Datososignermin2018$Minuto<-minute(Datososignermin2018$Fechadate)
Datososignermin2018$HorayMinuto<-format(Datososignermin2018$Fechadate,"%H:%M")
Datososignermin2018$HorayMinuto<-hm(Datososignermin2018$HorayMinuto)
str(Datososignermin2018$HorayMinuto)
Datososignermin2018<-na.omit(Datososignermin2018)
library(help="lubridate")

####Grafico evaluado en una hora determinada todos los dias para 3 Empresas ####

Data3<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data3<-filter(Data3,Hora=="8",CodEmpresa%in%c("CEEP","EDPE","EGAS"))
p1<-ggplot(Data3,aes(x=Minuto,y=RegistroActiva,colour=CodEmpresa))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00am a 9:00am de Empresas")
#Tomando estas 3 Empresas podemos verificar que es la Empresa CEEP la cual tiene el mayor registro de Energia Activa todos los dias#

####Grafico evaluado de 8:00am a 9:00am  en el dia 1 de la Empresa CEEP####
summary(Data3)
Data4<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data4<-filter(Data4,Dia=="1",Hora=="8",CodEmpresa=="CEEP")
ggplot(Data4,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point(shape=23,aes(fill=Suministro))+
  scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00am a 9:00am de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
ggsave("Registroenergiaactiva8a9.png")
#Vemos que el suministro CL0036 tiene el mayor registro de energia Activa, mientras que le sigue el suministro CL0597#

####Suministros que superan el promedio en la Empresa CEEP de 8:00am a 9:00am en el dia 1####
summary(Data4)
Data5<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data5<-filter(Data4,Dia=="1",Hora=="8",CodEmpresa=="CEEP",RegistroActiva>=1993.6)
ggplot(Data5,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00am a 9:00am de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#A pesar de que hay 9 suministros en la Empresa CEEP solo 3 superan el promedio de Registro Activa"

####Grafico evaluado 2:00pm a 3:00pm determinada en el dia 1 de la Empresa CEEP####
Data5<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data5<-filter(Data5,Dia=="1",Hora=="14",CodEmpresa=="CEEP")
ggplot(Data5,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 2:00pm a 3:00pm de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#Vemos que la empresa CL0036 sigue siendo la de mayor RegistroActiva#

####Suministros que superan el promedio en la Empresa CEEP de 2:00pm a 3:00pm en el dia 1####
summary(Data5)
Data6<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data6<-filter(Data6,Dia=="1",Hora=="8",CodEmpresa=="CEEP",RegistroActiva>=2255.70)
ggplot(Data6,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 2:00pm a 3:00pm de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#Los suministros CL0036,CL0254 y CLO597 son los que superan el promedio de RegistroActiva, son los mismos que los de las 8:00am, pero esta vez el promedio es mayor#

####Grafico evaluado de 8:00pm a 9:00pm en el dia 1 de la Empresa CEEP####
Data7<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data7<-filter(Data7,Dia=="1",Hora=="20",CodEmpresa=="CEEP")
ggplot(Data7,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00pm a 9:00pm de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#vemos que a las 8 pm el de mayor registro Activa es CL0597#

####Suministros que superan el promedio en la Empresa CEEP de 8:00pm a 9:00pm en el dia 1####
summary(Data7)
Data8<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data8<-filter(Data6,Dia=="1",Hora=="20",CodEmpresa=="CEEP",RegistroActiva>=1653.58)
ggplot(Data6,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00pm a 9:00pm de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#Visualizamos que en este caso el promedio es menor que el de la mañana y la tarde#

####Grafico evaluado de 8:00am a 9:00am en el dia 15 de la Empresa CEEP####
Data8<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data8<-filter(Data8,Dia=="15",Hora=="8",CodEmpresa=="CEEP")
ggplot(Data8,aes(x=Minuto,y=RegistroActiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00am a 9:00am de la Empresa CEEP")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#Vemos que en este dia mientras que CL0036 sigue siendo el de mayor registro, pero esta vez es mas alto)

####Grafico evaluado de 8:00am a 9:00am todos los dias del mes de Marzo####
Data9<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Data9<-filter(Data9,Dia<=15,Hora=="8",CodEmpresa=="CEEP",Suministro=="CL0036")
Data9$Dia<-as.character(Data9$Dia)
str(Data9$Dia)
ggplot(Data9,aes(x=Minuto,y=RegistroActiva,colour=Dia))+geom_point(shape=22,aes(fill=Dia))+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa de 8:00am a 9:00am de la Empresa CEEP del dia 1 al 15")+labs(y="Registro de Energía Activa en kW.h"	,x="Minutos")
#En este grafico evaluado en Dias vemos que el dia 10 fue el que mayor Registro Activa tuvo en los primeros minutos de las 8:00am#


####Evaluando los Registros Pasivos el dia 1 de 8:00am a 9:00am####
Data10<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroPasiva","Dia")
Data10<-filter(Data10,Hora=="8",Dia=="1",CodEmpresa=="CEEP")
ggplot(Data10,aes(x=Minuto,y=RegistroPasiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Pasiva de 8:00am a 9:00am de la Empresa CEEP")+labs(y="Registro de Energía Pasiva en kV.arh"	,x="Minutos")
#Podemos visualizar que en este caso que CL0597 es el que tiene mayor Registro Pasiva#

####Evaluando los Registros Pasivos el dia 1 de 8:00pm a 9:00pm####
Data10<-select(Datososignermin2018,"CodEmpresa","Suministro","Hora","Minuto","RegistroPasiva","Dia")
Data10<-filter(Data10,Hora=="20",Dia=="1",CodEmpresa=="CEEP")
ggplot(Data10,aes(x=Minuto,y=RegistroPasiva,colour=Suministro))+geom_point()+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Pasiva de 8:00pm a 9:00pm de la Empresa CEEP")+labs(y="Registro de Energía Pasiva en kV.arh"	,x="Minutos")
#De igual manera CL0597 es el de menor Registro Pasivo#





Datososignermin2019<-read.table("201903_TABLA4.txt",header = T,sep = "\t",col.names=c("CodEmpresa","Suministro","PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),colClasses = c("factor","factor","factor","POSIXct","character","character","character"))
Datososignermin2019$RegistroActiva<- as.numeric(sub(",", ".", Datososignermin2019$RegistroActiva, fixed = TRUE))
Datososignermin2019$RegistroPasiva<- as.numeric(sub(",", ".", Datososignermin2019$RegistroPasiva, fixed = TRUE))

Datososignermin2019$Dia<-year(Datososignermin2019$Fecha)
Datososignermin2019$Año<-day(Datososignermin2019$Fecha)
Datososignermin2019$Hora<-hour(Datososignermin2019$Fecha)
Datososignermin2019$Minuto<-minute(Datososignermin2019$Fecha)
Datososignermin2019<-na.omit(Datososignermin2019)

####Vamos a evaluar Registro Activo el dia 1 para La Empresa CEEP#### 
Datos1<-Datososignermin2019[0:47,]
Datos1<-select(Datos1,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Datos1$Hora<-as.character(Datos1$Hora)
ggplot(Datos1,aes(x=Minuto,y=RegistroActiva,colour=Hora))+geom_point(shape=22,aes(fill=Hora))+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa por horas en el dia 1")
#Podemos visualizar que la hora 00:00am y 1:00am es donde se tiene un mayor registro de Energia Activa#


####Vamos a evaluar Registro Activo el dia 15 para La Empresa CEEP#### 
Datos2<-select(Datososignermin2019,"CodEmpresa","Suministro","Hora","Minuto","RegistroActiva","Dia")
Datos2<-filter(Datos2,Dia=="15")
Datos2<-Datos2[1:48,]
Datos2$Hora<-as.character(Datos2$Hora)
ggplot(Datos2,aes(x=Minuto,y=RegistroActiva,colour=Hora))+geom_point(shape=24,aes(fill=Hora))+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Activa por horas en el dia 15")
#Vemos que en este caso el mayor Registro de Energia por horas es a las 11:00am siendo en este caso mas alto que los del dia 1#

####Vamos a evaluar Registro Activo el dia 15 para La Empresa CEEP#### 
Datos3<-select(Datososignermin2019,"CodEmpresa","Suministro","Hora","Minuto","RegistroPasiva","Dia")
Datos3<-filter(Datos3,Dia=="15")
Datos3<-Datos3[1:48,]
Datos3$Hora<-as.character(Datos2$Hora)
ggplot(Datos3,aes(x=Minuto,y=RegistroPasiva,colour=Hora))+geom_point(shape=25,aes(fill=Hora))+scale_x_continuous(breaks = c(0,15,30,45))+ggtitle("Registro de Energia Pasiva por horas en el dia 15")
#Aqui el mayor registro Pasivo tambien ocurre a las 11 am, sobretodo en los minutos 0 y 15#







