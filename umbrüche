#Dipl.-Prähist. Moritz Mennenga
#Niedersächsisches Institut für historische Küstenforschung
#Viktoriastr. 26/28
#263856 Wilhelmshaven
#
#   VERSION 1
#
#Das folgende Script für R-Cran ist getestet mit R-Studio Version 0.98.1006 und R-Cran R version 3.1.0 64-Bit auf Windows 7
#
#In diesem Script werden die Werte der Ausprägungen der Umbrüche ausgegeben
#
#AUF ANPASSEN ACHTEN
#



arbverz <- "D:\\"      					# Das angelegt Arbeitsverzeichnis waehlen
setwd(arbverz)									# Workspace setzen
.libPaths("D:\\")               # Falls die librarys nicht im Standartverzeichnes liegen, das hier angeben





library(ggplot2)

#Falls die Bibilotheken nicht vorhanden sind mit folgendem installieren 
#install.packages("ggplot2")

# Hier bitte die gewünschte Datei einlesen oder das ganze über ODBC mit Access o.ä. verbinden
kr <- read.table("techniken.csv", sep=',', header=TRUE)

#ANPASSEN
#Felder mit den Informationen der Umbrüche, wenn nötig in neue Spalten schreiben
kr$umbnn <- '037umbnn'  #'037umbnn' ersetzten durch den Spaltennamen in dem die Umbruch Lage unbekannt steht
kr$umbrs <- '038umbrs'  #s.o.
kr$umbrb <-'039umbrb' #s.o.
kr$umbsb <- '040umbsb' #s.o.
kr$umbbd <- '042umbbd' #s.o.



#Anlegen der Exportmatrix mit entsprechenden Namen der Spalten und Reihen
exp <- matrix(0,7,5)
rownames(exp) <- c("Gerundet","Scharfkantig","Abgesetzt","Gerundet,verdickt","Abgesetzt,innen eingezogen","leistenartig","Keine Aussage")
colnames(exp) <- c("Lage ubk.","Rand-Schulter","Rand-Bauch","Schulter-Bauch","Boden-Wand")


#Zählvariable
z<- 1
#Für jeden Eintrag in kr
while (z <= nrow(kr))
{ 
  #Wenn an der Stelle Lage unbekannt eine Zahl steht und diese < 9 ist (da 9 = Keine Aussage)
   if (kr$umbnn[z] < 9 && !is.na(kr$umbnn[z]))
  {
     #Soll in der Ergebnistabelle an der Stelle Eintrag aus Nonek (was der Reihenfolge der Werte entspricht) / Lage unbekannt der Wert um+
     #eins erhöht werden, dass wird dann für alle "Lagen" der Umbrüche durchgeführt
    exp[kr$umbnn[z],1] <-  exp[kr$umbnn[z],1]+1
  }
  #Wenn es größer 9 ist muss an der 7 Stelle etwas eingefügt werden
  if (kr$umbnn[z] >= 9 && !is.na(kr$umbnn[z]))
  {
    exp[7,1] <- exp[7,1]+1
  }
  
    
  if (kr$umbrs[z] < 9 && !is.na(kr$umbrs[z]))
  {
    exp[kr$umbrs[z],2] <-  exp[kr$umbrs[z],2]+1
  }
  if (kr$umbrs[z] >= 9 && !is.na(kr$umbrs[z]))
  {
    exp[7,2] <- exp[7,2]+1
  }
  
  
  if (kr$umbrb[z] < 9 && !is.na(kr$umbrb[z]))
  {
    exp[kr$umbrb[z],3] <-  exp[kr$umbrb[z],3]+1
  }
  if (kr$umbrb[z] >= 9 && !is.na(kr$umbrb[z]))
  {
    exp[7,3] <- exp[7,3]+1
  }
  
  if (kr$umbsb[z] < 9 && !is.na(kr$umbsb[z]))
  {
    exp[kr$umbsb[z],4] <-  exp[kr$umbsb[z],4]+1
  }
  if (kr$umbsb[z] >= 9 && !is.na(kr$umbsb[z]))
  {
    exp[7,4] <- exp[7,4]+1
  }
  
  
  if (kr$umbbd[z] < 9 && !is.na(kr$umbbd[z]))
  {
    exp[kr$umbbd[z],5] <-  exp[kr$umbbd[z],5]+1
  }
  if (kr$umbbd[z] >= 9 && !is.na(kr$umbbd[z]))
  {
    exp[7,5] <- exp[7,5]+1
  }
  

z<- z+1
}
exp
