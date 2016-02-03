#Dipl.-Prähist. Moritz Mennenga
#Niedersächsisches Institut für historische Küstenforschung
#Viktoriastr. 26/28
#263856 Wilhelmshaven
#
#   VERSION 2
#
#Das folgende Script für R-Cran ist getestet mit R-Studio Version 0.98.1006 und R-Cran R version 3.1.0 64-Bit auf Windows 7
#
#Im folgenden wird die Schwierigkeit behandelt bei NoNeK (http://www.uf.uni-erlangen.de/wp-content/uploads/nonek_v29.pdf)
#die Anzahl der Techniken pro Gefäßeinheiten/Muster darzustellen, da dazu alle Aufnahmen einer Gefäßeinheit untereinander und zusätzlich
#die Möglichkeit 4 Techniken pro Muster aufzunehmen verglichen werden müssen  
#
#
#Die Ausgangstabelle muss wie folgt aussehen:
#Spalte 1: Gefäßeinheit am Besten aus FB1 (Nummer o.ä.), ODER Muster aus FB2 (In diesem Falle die Plotfunktion am Ende Verwenden)
#Spalte 2: grabung (z.B. Fundstellennummer, dadurch können auch mehrere Fundplätze verarbeitet werden)  
#Spalte 3: technik1
#Spalte 4: technik2 
#Spalte 5: technik3 
#Spalte 6: technik4
#
#Optional können hier natürlich auch die Elemente stehen
#!!!!!!Zum sicheren Ablauf des Scripes sollten die Felder immer von links nach recht befüllt sein, also KEINE Technik 3 vorkommen, wenn es keine Technik 2 gibt
#!!!!!!Die Felder zur GefäßID und zur Grabung, sowie Technik 1 müssen Werte enthalten



#START TECHNIKEN


arbverz <- "D:\\"    						# Das angelegt Arbeitsverzeichnis waehlen
setwd(arbverz)									# Workspace setzen
.libPaths("D:\\")               # Falls die librarys nicht im Standartverzeichnes liegen, das hier angeben


#install.packages ('reshape', destdir = "D:\\Cloud\\dropbox\\Dropbox\\R_Pakete\\r", lib = "D:\\Cloud\\dropbox\\Dropbox\\R_Pakete\\r")
library(RODBC)
library(ggplot2)
library(reshape)
library(plyr)

#Falls die Bibilotheken nicht vorhanden sind mit folgendem installieren 
#install.packages("ggplot2","plyr")

# Hier bitte die gewünschte Datei einlesen oder das ganze über ODBC mit Access o.ä. verbinden
kr <- read.table("technik.csv", sep=';', header=FALSE)

#Die Tabelle sollte nun 6 Spalten haben!
#Die müssen erstmal vernunftig sortiert werden
kr <- arrange(kr,desc(kr[,2]), desc(kr[,1]), kr[,3],kr[,4],kr[,5],kr[,6])

#Und ein dataframe für das ergebnis muss erstellt werden
ergebnis = data.frame(gefid=(0), grabung = (0), technik = (0)) 

#jetzt geht es los mit dem Filtern und Vergleichen!
i = 1 #Zählvariable um durch den Eingangsdatensatz zu steuern
k = 1 #Zählvariable um durch den Ergebnisdatensatz zu steuern

# Schritte für alle Zeilen im Eingangsdokument durchführen
while (i <= nrow(kr)) 
  
{
  #Werte für die Technik 1 des ersten Eintrags aus Ausgangstabelle in die neue Übernehmen 
  if (!is.na(kr[i,3]))
  {
    ergebnis[k,1] <- kr[i,1]
    ergebnis[k,2] <- kr[i,2]
    ergebnis[k,3] <- kr[i,3]
    k <- k+1
  }
  
  if (!is.na(kr[i,4]) & kr[i,4] != kr[i,3])
    #Wenn es eine zweite Technik gibt, schauen ob die der ersten entspricht, ansonsten einen neuen Satz in der Ergebnistabelle anlegen
  {
    ergebnis[k,1] <- kr[i,1]
    ergebnis[k,2] <- kr[i,2]
    ergebnis[k,3] <- kr[i,4]
    k <- k+1

  }
  if (!is.na(kr[i,5]) & !is.na(kr[i,4]) & kr[i,5] != kr[i,3] & kr[i,5] != kr[i,4])
    #Wenn es eine dritte Technik gibt, schauen ob die der ersten bzw. zweiten entspricht, ansonsten einen neuen Satz in der Ergebnistabelle anlegen
  {
    ergebnis[k,1] <- kr[i,1]
    ergebnis[k,2] <- kr[i,2]
    ergebnis[k,3] <- kr[i,5]
    k <- k+1

  }
  if (!is.na(kr[i,6])& !is.na(kr[i,5]) & !is.na(kr[i,4]) & kr[i,6] != kr[i,3] & kr[i,6] != kr[i,4] & kr[i,6] != kr[i,5])
    #Wenn es eine vierte Technik gibt, schauen ob die der ersten, zweiten bzw. dritten entspricht, ansonsten einen neuen Satz in der Ergebnistabelle anlegen
  {
    ergebnis[k,1] <- kr[i,1]
    ergebnis[k,2] <- kr[i,2]
    ergebnis[k,3] <- kr[i,6]
    k <- k+1

  }
  
  i <- i+1
}


#Nur eindeutige Datensätze speichern, dadurch werden mehrere Einträge einer GE zusammengefasst
ergebnis <- unique(ergebnis)

#ergebnis als Tabelle
ergebnis

#PLOTFUNKTIONEN (ZUERST TECHNIK PRO GE dann TECHNIK PRO MUSTER)

#Und dann das ganze schön plotten
#nach Technik sortiert

reihenfolge <- count(ergebnis$technik)
reihenfolge <- arrange(reihenfolge,as.numeric(as.character(reihenfolge$x)))
ergebnis$technik <- factor(ergebnis$technik, levels = reihenfolge$x)
pl <- qplot(factor(technik), data = ergebnis, geom="bar",fill=factor(grabung))
pl + labs(x = "Technik nach NoNeK", y = "Gefäßeinheiten [n]", fill = "Grabung") 
print("Reihenfolge der Grabungen")
print(summary(kr$grabung))

#aufsteigend nach gesamtsumme
reihenfolge <- count(ergebnis$technik)
reihenfolge <- arrange(reihenfolge, desc(reihenfolge$freq))
ergebnis$technik <- factor(ergebnis$technik, levels = reihenfolge$x)
pl <- qplot(technik, data = ergebnis, geom="bar",fill=factor(grabung)) 
pl + labs(x = "Technik nach NoNeK", y = "Gefäßeinheiten [n]", fill = "Grabung")+ stat_bin(geom = "text", aes(label = paste(..count..)))
print("Reihenfolge der Grabungen")
print(summary(kr$grabung))

#ENDE TECHNIKEN

#PLOTTEN VON TECHNIK PRO MUSTER

#Plotten welches Muster hat welche Technik
#Eine neue Tabelle wird zum Plotten angelegt
#Falls nur eine Grabung ausgewertet werden soll Grabungsnummer eingeben
#ergebnis1 = subset(ergebnis, ergebnis$grabung == 3)
#ANSONSTEN hier weitermachen
ergebnis1 <- ergebnis
#die Grabungen istressieren uns hier nicht mehr
ergebnis1$grabung <- NULL
#Jedes Muster und jede Technik sollen nur einmal vorkommen
ergebnis1 = unique(ergebnis1)
#Die Folgenden Schritte sortieren und formen die Daten so um, dass sowohl die Muster, als auch die Techniken aufsteigend sortiert sind und im Plot angezeigt werden
ergebnis1$gefid <- as.integer(as.character(ergebnis1$gefid))
ergebnis1 <- ergebnis1[order(as.integer(ergebnis1$gefid)),]
ergebnis1$gefid <- as.factor(ergebnis1$gefid)
ergebnis1$technik <- as.factor(ergebnis1$technik)

#Und jetzt noch zeichnen lassen
qplot(data=ergebnis1,technik,gefid) + labs(x = "Technik nach NoNeK", y = "Muster nach NoNeK", fill = "Grabung")


#Das ganze jetzt noch als Tabelle
x <- length(unique(ergebnis1$gefid))
y <- length(unique(ergebnis1$technik))
muster_technik <- matrix(0, nrow = x, ncol = y)
muster_technik <- data.frame(muster_technik)
row.names(muster_technik) <- unique(ergebnis1$gefid)
colnames(muster_technik) <- unique(ergebnis1$technik)
a <- 0
b <- 0
p <- 0
counter <- 1
while (p <= nrow(ergebnis1))
{
  a <- ergebnis1$gefid[counter]
  b <- ergebnis1$technik[counter]
muster_technik[toString(a),toString(b)] <- 1
counter <- counter + 1
p <- p+1
}
muster_technik[,'NA'] <- NULL
muster_technik <- na.omit(muster_technik)

print(muster_technik)





