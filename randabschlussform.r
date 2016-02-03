#Dipl.-Prähist. Moritz Mennenga
#Niedersächsisches Institut für historische Küstenforschung
#Viktoriastr. 26/28
#263856 Wilhelmshaven
#
#   VERSION 2
#
#Das folgende Script für R-Cran ist getestet mit R-Studio Version 0.98.1006 und R-Cran R version 3.1.0 64-Bit auf Windows 7
#
#In diesem Script werden die Werte der Randabschlussformen pro Gefäßform ausgegeben
#
#Es kann das gesamte FB 1 eingeladen werden. Dann bitte auf ANPASSEN achten
#



#START Randabschlussformen


arbverz <- "D:\\"      					# Das angelegt Arbeitsverzeichnis waehlen
setwd(arbverz)									# Workspace setzen
.libPaths("D:\\")               # Falls die librarys nicht im Standartverzeichnes liegen, das hier angeben



library(RODBC)
library(ggplot2)


#Die Randabschlussformen bei denen Randform nicht 99 oder 00 ist.
#Tabelle ist wie folgt aufgebaut:
#Spalte 1: gefäÃform Name: gform
#Spalte 2: Randform Name: rfend


#Randabschlussform pro GefäÃform


arbverz <- "D:\\"        				# Das angelegt Arbeitsverzeichnis waehlen
setwd(arbverz)									# Workspace setzen
.libPaths("D:\\")               # Falls die librarys nicht im Standartverzeichnes liegen, das hier angeben


library(ggplot2)



#Falls die Bibilotheken nicht vorhanden sind mit folgendem installieren 
#install.packages("ggplot2")

# Hier bitte die gewünschte Datei einlesen oder das ganze über ODBC mit Access o.ä. verbinden
kr <- read.table("randformen.csv", sep=',', header=TRUE)

#ANPASSEN
kr$rfend <- kr$'58rfgrd' #'58rfgrd' ersetzten durch den Spaltennamen in dem die Randabschlussform (gerade) steht
kr$rfend <- kr$'57rfend' #'57rfend' ersetzten durch den Spaltennamen in dem die Randabschlussform steht
kr$gform <- kr$'56gform' #'56gform' ersetzten durch den Spaltennamen in dem die Gefäßform steht

#RANDABSCHLUSSFORM
# Für die Folgenden Schritte siehe Script Randformen
kr$gruppe <- NA

z=1
while (z <= nrow(kr))
{
  if(grepl("2[[:digit:]]{1}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 2)
  {kr$gruppe[z]<- c("Tonscheibe")}
  if(grepl("2[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Zylinderrand")}
  if(grepl("1[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Trichterrand")
print(kr$gform[z])}
  if(grepl("3[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Konusrand")}
  z=z+1
}

#Ergebnis zeichnen
palette_4<-c("#009EE2","#F2C559","#B24248","#1CA538")
ggplot(kr,aes(factor(rfend), fill= factor(gruppe)))+geom_bar(binwidth = 1.0)+  geom_text(stat='bin',aes(label = ..count..))+scale_fill_manual(values=palette_4, name="Randformen") +
  xlab("Randabschlussform nach NoNeK") + ylab("Anzahl")


#RANDABSCHLUSSFORM GERADE

z=1
while (z <= nrow(kr))
{
  if(grepl("2[[:digit:]]{1}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 2)
  {kr$gruppe[z]<- c("Tonscheibe")}
  if(grepl("2[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Zylinderrand")}
  if(grepl("1[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Trichterrand")
   print(kr$gform[z])}
  if(grepl("3[[:digit:]]{2}",as.character(kr$gform[z])) == TRUE && nchar(kr$gform[z]) == 3)
  {kr$gruppe[z]<- c("Konusrand")}
  z=z+1
}

palette_4<-c("#009EE2","#F2C559","#B24248","#1CA538")
ggplot(kr,aes(factor(rfgrd), fill= factor(gruppe)))+geom_bar(binwidth = 1.0)+  geom_text(stat='bin',aes(label = ..count..))+scale_fill_manual(values=palette_4, name="Randformen") +
  xlab("Randabschlussformen (gerade) nach NoNeK") + ylab("Anzahl")



#CM in INCHES fÃ¼r die Angabe beim Export
cm_hoehe <- 15 #Hier die gewünschte cm Zahl für die Höhe angeben
cm_breite <- 5 #Hier die gewünschte cm Zahl für die Breite angeben

#Ausführen
print("Höhe:")
print(cm_hoehe * 0.39370)
print("Breite:")
print(cm_breite * 0.39370)
