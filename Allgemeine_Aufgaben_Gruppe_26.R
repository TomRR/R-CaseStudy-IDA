#libraries.R
#Sichern, dass StringsAsFactors gleich FALSE
options(stringsAsFactors = F)

# Überprüfen, ob die Packages bereits installiert sind
if(!require(readr)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("readr", dependencies = TRUE)
}
library(readr)

#Und so weiter
if(!require(lubridate)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("lubridate", dependencies = TRUE)
}
library(lubridate)
if(!require(dplyr)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("dplyr", dependencies = TRUE)
}
library(dplyr)
if(!require(ggplot2)){
  # wenn es nicht installiert ist, wird es nun installiert
  install.packages("ggplot2", dependencies = TRUE)
}
library(ggplot2)

#Aufgabe1
#Importieren und reinigen die relevante Datensätze
K7 <- read_csv2("Data/Logistikverzug/Komponente_K7.csv") %>% select(2:6)
K7$Fehlerhaft <- as.logical(K7$Fehlerhaft)
names(K7)[5] <- "Vor-Fehlerhaft"

LK7 <- read_csv2("Data/Logistikverzug/Logistikverzug_K7.csv") %>% select(2:6)
LK7$Fehlerhaft <- as.logical(LK7$Fehlerhaft)
LK7$Wareneingang <- dmy(LK7$Wareneingang)
names(LK7)[5] <- "Nach-Fehlerhaft"

#Kombinieren um die Analyse zu bereiten
komp7 <- merge(K7, LK7, by=c('IDNummer', 'Herstellernummer', 'Werksnummer'))
rm(K7, LK7)

#Berechnung der vergangenen Zeit zwischen Warenausgang und Wareneingang
Tage_inzwischen <- komp7$Wareneingang - komp7$Produktionsdatum
Tage_inzwischen <- as.integer(Tage_inzwischen)

#Fügen diese Berechnung dem Datensatz hinzu
komp7 <- cbind(komp7, Tage_inzwischen)
rm(Tage_inzwischen)

#Berechnung der wichtigen Werte
min <- min(komp7$Tage_inzwischen)
max <- max(komp7$Tage_inzwischen)
mean <- mean(komp7$Tage_inzwischen)

ggplot(komp7, aes(x=komp7$Tage_inzwischen)) + geom_histogram(binwidth=.5)

#Aufgabe3
#Komponente K7 gilt nur in Fahrzeuge des Typs 22. Hier wird den Datensatz davon bereitet
BOT22 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv") %>% select(2:6)
#Bereitung der Zulassungendaten
zulassungen<-read_csv2("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv") %>% select(2:4)
names(zulassungen)[1]<-"ID_Fahrzeug"
BOT22 <- left_join(BOT22, zulassungen, by = "ID_Fahrzeug")
#Die Nummer der Fahrzeuge, die in Dortmund zugelassen wurden
nrow(filter(BOT22, Gemeinden == "DORTMUND"))

#Aufgabe4
str(zulassungen)

#Aufgabe6
#Sammlung der Motoren-, Fahrzeuge-, und Gemeindendaten
b11 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv") %>% select(5:6)
b12 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv") %>% select(5:6)
b21 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv") %>% select(5:6)
b22 <- read_csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv") %>% select(5:6)
PolizeiDatenSatz <- rbind(b11, b12, b21, b22) %>% left_join(zulassungen, by = 'ID_Fahrzeug')
rm(b11, b12, b21, b22)
#Identifizieren des Ungefallenen Fahrzeugs
UnfallWagen <- filter(PolizeiDatenSatz, ID_Motor == "K1BE2-104-1041-32050")
UnfallWagen$Gemeinden

