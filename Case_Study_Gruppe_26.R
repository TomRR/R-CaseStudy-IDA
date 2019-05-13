#CSR

#######Die Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(prettydoc, plotly,DT, lubridate,stats,data.table,readr,dplyr,tidyr,readtext,kableExtra,shinydashboard,shinyjs,shiny)


#   --------------------------------------------------


# helpers.R ----------------------
#set stringsAsFactors to FALSE globaly
options(stringsAsFactors = F)

#Funktion zum importieren der .txt Dateien
import_txt<- function(filename,sepcol,sepline) { #Header von der Funktion importxt 
  
  con   <- file(filename, open = "r")   # .txt Datei wird in einer con gespeichert mit "read"
  lines <- readLines(con)             # mit readLines(con) wird der Inhalt der .txt Datei in einem großen Character gespeichert
  l     <- strsplit(lines[[1]],split = sepline)  # Die Zeilen werden mit dem Zeilenseparator "sepline" gelistet
  l     <- unlist(l) #Die Zeilen von dem vorherigen Schritt werden in einem großen Character gespeichert
  l     <- as.data.frame(l) #Dadurch dass das Character l viele zeilen enthÃ¤lt, wird das als ein einspaltiges Dataframe konvertiert
  c     <- l[1,1] #erste Zelle enthÃ¤lt die Spaltennamen
  c     <- as.character(c) 
  s     <- strsplit(c,split=sepcol) #Spaltennamen werden separat in einem Vektor gespeichert
  s     <- unlist(s) 
  s     <- gsub("\"","",s) #überflüssige Anführungszeichen werden gelöscht
  s     <- c("A",s) #zusÃ¤tzlicher erster Name muss für die namenlose laufende Nummerierung hinzugefügt werden 
  df    <- l %>% separate(l,into=s,sep = sepcol) #gemaß der einzelenen Spaltennamen (Inhalt des Vektors s) wird das Dataframe in Spalten gesplittert
  df    <- df[-1,] #somit wird die erste Zeile Überflüssig, muss daher gelöscht werden.
  return(df)
}

#importing werk with extra symbols
import_werk <- function(filename){             #Funktion zum Importieren .txt Dateien mit komischen Symbolen
  con   <- file(filename, open = "r")      #Analog zur importxt()
  lines <- readLines(con)
  lines <- gsub("\x84","ae",lines)         #Falsche Buchstaben/Symbole werden korrigiert
  lines <- unlist(lines)
  l     <- as.data.frame(lines)
  write.csv(l,"Data/Geodaten/Werk.csv",row.names = F) #neues Dataframe in .csv speichern
  werk  <- fread("Data/Geodaten/Werk.csv",quote="") #Korrigierte datei wird neu einlesen
  werk  <-werk[,1:5]  #relevante Spalten auswÃ¤hlen
  names(werk) = c("PLZ",names(werk)[2:4],"Laengengrad")  #Spaltennamen korrigieren
  werk$PLZ <- gsub("\"","",werk$PLZ) #Aufrufezeichen beseitigen
  werk$Laengengrad <- gsub("\"","",werk$Laengengrad) #Aufrufezeichen beseitigen (Spalten seperat betrachten um das Consuming zu reduzieren)
  werk$PLZ <- as.integer(werk$PLZ) #PLZ wird als natÃ¼rliche Zahl betrachtet
  werk$Laengengrad <- as.numeric(werk$Laengengrad) #Koordinaten werden als numerics konvertiert
  werk$Breitengrad <- as.numeric(werk$Breitengrad)
  werk$Werk <- gsub("O","",werk$Werk)  
  werk$Werk <- as.integer(werk$Werk)
  return(werk)
}

#cleaning function
clean<-function(df){
  
  names(df) <- gsub("Produktionsdatum_Origin_01011970","Produktionsdatum",names(df))   #Spaltennamen einheitlich behalten
  #Relevante Spalten auswÃ¤hlen
  df <- select(df,ID,`Fehlerhaft_Datum`,`Fehlerhaft_Fahrleistung`,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft)
  #ID bleibt ein Character
  df$ID <- as.character(df$ID)
  df$ID <- gsub("\"","",df$ID) #Aufrufezeichen beseitigen
  df$Herstellernummer <- gsub("\"","",df$Herstellernummer) #Aufrufezeichen beseitigen
  df$Werksnummer <- gsub("\"","",df$Werksnummer)  #Aufrufezeichen beseitigen
  df$Herstellernummer<-as.integer(df$Herstellernummer) #Herstellernummer wird als Zahl betrachtet
  df$Werksnummer<-as.integer(df$Werksnummer) #Analog   
  df$Fehlerhaft_Fahrleistung<-as.numeric(df$Fehlerhaft_Fahrleistung) #Die fehlerhafte Fahrleistung ist eine natÃ¼rliche Zahl
  df$Fehlerhaft_Fahrleistung<-abs(df$Fehlerhaft_Fahrleistung)
  if(any(grepl("-",df$Produktionsdatum))==TRUE){
    df$Produktionsdatum<-ymd(df$Produktionsdatum)  # Datum mit standard Darstellung (Bsp 01-01-2010) konvertieren 
  } else{
    df$Produktionsdatum<-as.numeric(df$Produktionsdatum) #Datum mit laufender Datumsdarstellung konvertieren 
    df$Produktionsdatum<-as.Date.numeric(df$Produktionsdatum,origin = "1970-01-01")
  }
  df$Fehlerhaft<-as.logical(as.integer(df$Fehlerhaft)) #Fehlerhaft ist entweder 1 oder 0 (Logical)
  df$Fehlerhaft_Datum<-ymd(df$Fehlerhaft_Datum) #Fehlerhaft_Datum wird standard dargestellt. 
  return(df)
  
}



prepare<-function(df1,datum,leistung){    #Funktion zur Vorbereitung der Daten zur Auswertung
  df<- df1 %>% filter(Fehlerhaft==T) %>%    #Fehlerhafte EintrÃ¤ge werden gefiltert
    mutate(Lebensdauer=Fehlerhaft_Datum, Produktionsdatum) %>%   # Lebensdauer der Bauteile werden berechnet
    select(ID,Fehlerhaft_Datum,Werksnummer,Fehlerhaft_Fahrleistung,Lebensdauer) %>%  # Relevante Spalten auswÃ¤hlen
    filter(Fehlerhaft_Datum >= datum,Fehlerhaft_Fahrleistung<=leistung) %>% #Randbedingungen werden hinzugefÃ¼gt
    arrange(Fehlerhaft_Datum) #Daten werden durch Fehlerhaft_Datum aufsteigend sortiert
  return(df) 
}






#ic_Einzelteile.R--------------
#Import Einzelteile K2L1
#T14  
Einzelteil_T14 <- read.csv2("Data/Einzelteil/Einzelteil_T14.csv")   #Datei fÃ¼r die Informationen zum Bauteil T14 einlesen (siehe helpers.R)
Einzelteil_T14$Fehlerhaft_Fahrleistung <- gsub(",",".",Einzelteil_T14$Fehlerhaft_Fahrleistung) #sub , to . to be able to convert as.numeric later
names(Einzelteil_T14)[3] <- "ID" #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T14 <- clean(Einzelteil_T14) #Dataframe T14 mit der Funktion clean() (siehe helpers.R) saubermachen
Einzelteil_T14_tidy <- filter(Einzelteil_T14,Fehlerhaft==T) #T14f enthält nur fehlerhafte EintrÃ¤ge


#T15
Einzelteil_T15 <- read.csv2("Data/Einzelteil/Einzelteil_T15.csv")  #Datei fÃ¼r die Informationen zum Bauteil T15 einlesen
Einzelteil_T15_1 <- Einzelteil_T15[,3:9]  #Die Werte für T15 sind in 2 Teil-Dataframe dargestellt, diese mÃ¼ssen vereinigt werden.
Einzelteil_T15_2 <- Einzelteil_T15[,10:16] #2-Teil-Dataframe
names(Einzelteil_T15_1) <- gsub(".x","",names(Einzelteil_T15_1)) #Namen zum spÃ¤teren rbind einheitlich machen 
names(Einzelteil_T15_2) <- gsub(".y","",names(Einzelteil_T15_2)) #Namen zum spÃ¤teren rbind einheitlich machen
Einzelteil_T15 <- rbind(Einzelteil_T15_1,Einzelteil_T15_2)   #2 Teil-Dataframe zusammenfÃ¼gen
Einzelteil_T15 <- Einzelteil_T15[rowSums( is.na(Einzelteil_T15) ) <=1, ] #Zeilen nur mit NA EintrÃ¤ge beseitigen
names(Einzelteil_T15)[1] <- "ID" #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T15$Fehlerhaft_Fahrleistung<-gsub(",",".",Einzelteil_T15$Fehlerhaft_Fahrleistung) #sub , to . to be able to convert as.numeric later
Einzelteil_T15 <- clean(Einzelteil_T15) #Dataframe T15 mit der Funktion clean() (siehe helpers.R) saubermachen
rm(Einzelteil_T15_1, Einzelteil_T15_2) #diese werden nicht mehr benötigt: löschen
Einzelteil_T15_tidy<-filter(Einzelteil_T15,Fehlerhaft==T) #T15f enthÃ¤lt nur fehlerhafte EintrÃ¤ge

#T11
Einzelteil_T11<-import_txt(filename = "Data/Einzelteil/Einzelteil_T11.txt",sepcol= "\t" ,sepline="\f")  #Datei für die Informationen zum Bauteil T11 einlesen
names(Einzelteil_T11)[3]<-"ID" #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T11<-clean(Einzelteil_T11) #Dataframe T11 mit der Funktion clean() (siehe helpers.R) saubermachen
Einzelteil_T11_tidy<-filter(Einzelteil_T11, Fehlerhaft==T) #T11f enthÃ¤lt nur fehlerhafte EintrÃ¤ge

#Import Einzelteile K2L2
#T19
Einzelteil_T19<-read.csv("Data/Einzelteil/Einzelteil_T19.csv") #Datei fÃ¼r die Informationen zum Bauteil T19 einlesen (siehe helpers.R)
names(Einzelteil_T19)[3]<-"ID"  #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T19<-clean(Einzelteil_T19) #Dataframe T19 mit der Funktion clean() (siehe helpers.R) saubermachen
Einzelteil_T19_tidy<-filter(Einzelteil_T19, Fehlerhaft==T) #T19f enthÃ¤lt nur fehlerhafte EintrÃ¤ge

#T16
Einzelteil_T16<-import_txt(filename = "Data/Einzelteil/Einzelteil_T16.txt",sepcol = " \\| \\| ",sepline = "\t") #Datei fÃ¼r die Informationen zum Bauteil T16 einlesen (siehe helpers.R)
Einzelteil_T16.x<-Einzelteil_T16[,3:9]    #Die Werte für T16 sind in 3 Teil-Dataframe dargestellt, diese müssen vereinigt werden.
Einzelteil_T16.y<-Einzelteil_T16[,10:16]  #2-Teil-Dataframe
Einzelteil_T16.z<-Einzelteil_T16[,17:23]  #3-Teil-Dataframe
names(Einzelteil_T16.x)<-names(Einzelteil_T16.z)   #Namen zum spÃ¤teren rbind einheitlich machen 
names(Einzelteil_T16.y)<-names(Einzelteil_T16.z)   #Namen zum spÃ¤teren rbind einheitlich machen 
Einzelteil_T16<-rbind(Einzelteil_T16.x, Einzelteil_T16.y, Einzelteil_T16.z) #3 Teil-Dataframes zusammenfögen
Einzelteil_T16$Fehlerhaft_Fahrleistung<-as.numeric(Einzelteil_T16$Fehlerhaft_Fahrleistung) #Wir brauchen mindestens ein numeric oder integer um den weiteren Schreitt durchzufÃ¼hren
Einzelteil_T16<-Einzelteil_T16[rowSums( is.na(Einzelteil_T16) ) <=1, ] #Zeilen nur mit NA Einträge beseitigen 
Einzelteil_T16$Herstellernummer<-gsub("\"","",Einzelteil_T16$Herstellernummer)  #AnfÃ¼hrungszeichen beseitigen
names(Einzelteil_T16)[1]<-"ID" #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T16<-clean(Einzelteil_T16) #Dataframe T16 mit der Funktion clean() (siehe helpers.R) saubermachen
Einzelteil_T16_tidy<-filter(Einzelteil_T16,Fehlerhaft==T) #T16f enthÃ¤lt nur fehlerhafte EintrÃ¤ge
rm(Einzelteil_T16.x,Einzelteil_T16.y,Einzelteil_T16.z) #diese werden nicht mehr benötigt: löschen

#T20
Einzelteil_T20<-import_txt(filename = "Data/Einzelteil/Einzelteil_T20.txt",sepcol = " \\| \\| ",sepline = "\" \"")  #Datei für die Informationen zum Bauteil T20 einlesen (siehe helpers.R)
names(Einzelteil_T20)[3]<-"ID" #ID fÃ¼r weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Einzelteil_T20<-clean(Einzelteil_T20) #Dataframe T20 mit der Funktion clean() (siehe helpers.R) saubermachen
Einzelteil_T20_tidy<-filter(Einzelteil_T20,Fehlerhaft==T) #T20f enthÃ¤lt nur fehlerhafte EintrÃ¤ge

#ic_Sitze_Fahrzeuge.R---------
#Import und Clean von den Komponenten
#Bestandteile
Bestandteile_Komponente_K2LE1<-read.csv2("Data/Komponente/Bestandteile_Komponente_K2LE1.csv") #Importieren der Bestandteile aller Sitze K2LE1
Bestandteile_Komponente_K2LE1<-Bestandteile_Komponente_K2LE1[,-1] #unnötige laufende Nummerierung löschen
Bestandteile_Komponente_K2LE2<-read.csv2("Data/Komponente/Bestandteile_Komponente_K2LE2.csv") #Analog für K2LE2
Bestandteile_Komponente_K2LE2<-Bestandteile_Komponente_K2LE2[,-1] #Analog
#K2LE1
Komponente_K2LE1<-import_txt("Data/Komponente/Komponente_K2LE1.txt",sepcol = "II",sepline ="\v") # #Datei für die Informationen zum Sitz K2LE1 einlesen (siehe helpers.R)
Komponente_K2LE1.x<-Komponente_K2LE1[,3:9]   #Die Werte fÃ¼r K2LE1 sind in 2 Teil-Dataframe dargestellt, diese müssen vereinigt werden.
Komponente_K2LE1.y<-Komponente_K2LE1[,10:16]  #2te Teil-Dataframe für K2LE1
names(Komponente_K2LE1.y)<-names(Komponente_K2LE1.x)  #Namen zum späteren rbind einheitlich machen 
Komponente_K2LE1<-rbind(Komponente_K2LE1.x,Komponente_K2LE1.y) #2 TeilDataframe zusammenfügen
names(Komponente_K2LE1)<-gsub(".x","",names(Komponente_K2LE1))  #Spaltennamen ohne .x darstellen
names(Komponente_K2LE1)[1]<-"ID" #ID für weitere Zwecke (später rbind) einheitlich behalten 
Komponente_K2LE1<-clean(Komponente_K2LE1) #Dataframe KL1 mit der Funktion clean() (siehe helpers.R) saubermachen
#K2LE2
Komponente_K2LE2<-read.delim("Data/Komponente/Komponente_K2LE2.txt",sep="\\")  #Datei für die Informationen zum Sitz K2LE2 einlesen (siehe helpers.R)
names(Komponente_K2LE2)[2]<-"ID" #ID für weitere Zwecke (spÃ¤ter rbind) einheitlich behalten 
Komponente_K2LE2<-clean(Komponente_K2LE2) #Dataframe KL2 mit der Funktion clean() (siehe helpers.R) saubermachen

KL<-rbind(Komponente_K2LE1, Komponente_K2LE2)

#Import und Clean von Fahrzeuge
#OEM11
Fahrzeuge_OEM1_Typ11 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv") #Datei für die Informationen zum Fahrzeug O11 einlesen
names(Fahrzeuge_OEM1_Typ11)[3] <- "ID"
Fahrzeuge_OEM1_Typ11 <- clean(Fahrzeuge_OEM1_Typ11) #Dataframe O11 mit der Funktion clean() (siehe helpers.R) saubermachen
#OEM12
Fahrzeuge_OEM1_Typ12 <- read.csv2("Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv") #Datei für die Informationen zum Fahrzeug O12 einlesen
names(Fahrzeuge_OEM1_Typ12)[3] <- "ID"
Fahrzeuge_OEM1_Typ12 <- clean(Fahrzeuge_OEM1_Typ12) #Dataframe O12 mit der Funktion clean() (siehe helpers.R) saubermachen
#OEM21
Fahrzeuge_OEM2_Typ21 <- read.csv("Data/Fahrzeug/Fahrzeuge_OEM2_Typ21.csv") #Datei für die Informationen zum Fahrzeug O21 einlesen
names(Fahrzeuge_OEM2_Typ21)[3] <- "ID"
Fahrzeuge_OEM2_Typ21 <- clean(Fahrzeuge_OEM2_Typ21) #Dataframe O21 mit der Funktion clean() (siehe helpers.R) saubermachen
#OEM22
Fahrzeuge_OEM2_Typ22 <- read.csv2("Data/Fahrzeug/Fahrzeuge_OEM2_Typ22.csv") #Datei für die Informationen zum Fahrzeug O22 einlesen
names(Fahrzeuge_OEM2_Typ22)[3] <- "ID" 
Fahrzeuge_OEM2_Typ22 <- clean(Fahrzeuge_OEM2_Typ22) #Dataframe O22 mit der Funktion clean() (siehe helpers.R) saubermachen

#Bestandteile OEMs
BOT11 <- read.csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv") #Importieren der Bestandteile aller Fahrzeuge OEM11
BOT11 <- BOT11[,-1] #unnötige laufende Nummerierung löschen
BOT12 <- read.csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv") #Importieren der Bestandteile aller Fahrzeuge OEM12
BOT12 <- BOT12[,-1] #unnötige laufende Nummerierung löschen
BOT21 <- read.csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv") #Importieren der Bestandteile aller Fahrzeuge OEM21
BOT21 <- BOT21[,-1] #unnötige laufende Nummerierung löschen
BOT22 <- read.csv2("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv") #Importieren der Bestandteile aller Fahrzeuge OEM22
BOT22 <- BOT22[,-1] #unnötige laufende Nummerierung löschen

#ic_zugew.R-------------------
#import and clean Geodaten
#Werke
OEM_Werke <- import_werk("Data/Geodaten/OEM_Werke_2017-07-04_TrR.csv") #Datei für die Geodaten der OEM-Werke einlesen
Tier2_Werke <- import_werk("Data/Geodaten/Tier2_Werke_2017-07-11_v1.2_TrR.csv") #Datei für die Geodaten der Einzelteile-Werke einlesen
Tier2_Werke <- import_werk("Data/Geodaten/Tier1_Werke_2017-07-11_v1.2_TrR.csv") # #Datei für die Geodaten der Sitze-Werke einlesen

#Zulassung
zulassung <- fread("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv",header=T) #Datei für die Zulassung der Fahrzeuge einlesen
zulassung <- zulassung[,-1]  #laufende Nummerierung lÃ¶schen
names(zulassung)[1] <- "ID" 
zulassung$Zulassung <- ymd(zulassung$Zulassung) #Datum konvertieren

#Gemeinde
Geodaten_Gemeinden <- fread("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv",header=T) #Datei der Geodaten der Gemeinden einlesen
Geodaten_Gemeinden <- Geodaten_Gemeinden[,-1:-2]    #erste zwei Spalten lÃ¶schen
Geodaten_Gemeinden <- Geodaten_Gemeinden[,c(1,2,4,3)]  #Reheinfolge der Spalten mit WOEM, Tier2 und Tier1 Daten einheitlich machen
Geodaten_Gemeinden$Laengengrad <- as.numeric(sub(",",".", Geodaten_Gemeinden$Laengengrad,fixed=T))  #Laengengrad muss numerisch sein, dafÃ¼r muss ein Punkt anstatt eine Komma eingefÃ¼hrt werden
Geodaten_Gemeinden$Breitengrad <- as.numeric(sub(",",".", Geodaten_Gemeinden$Breitengrad,fixed=T))  #Breitengrad muss numerisch sein, dafÃ¼r muss ein Punkt anstatt eine Komma eingefÃ¼hrt werden
names(Geodaten_Gemeinden) <- c("PLZ", "Gemeinden", names(Geodaten_Gemeinden)[3:4]) #Spaltennamen anpassen

#ana_Einzelteile.R------------
#Die Daten werden zur Analyse vorbereitet. 
#Um eine konventionelle Fehlerhaft_Leistung fÃ¼r die Bauteile zu bestimmen, muss das 3 Quantile von den fehlerhaften Bauteile der letzten 10 Jahren berechnet.
#Wir nehmen also an dass das Teil selbstverstÃ¤ndlich kaputt gehen kann wenn man den Wert des 75%Quantil Ã¼berschreitet. 
#Diese werden in einem Vektor ergÃ¤nzt. c_leistung1 fÃ¼r die fehlerhaften Bauteile von K2LE1, c_leistung2 fÃ¼r die von K2LE2
c_leistung1<-c(quantile(Einzelteil_T11_tidy$Fehlerhaft_Fahrleistung,0.75,names=F),quantile(Einzelteil_T14_tidy$Fehlerhaft_Fahrleistung,0.75,names=F),quantile(Einzelteil_T15_tidy$Fehlerhaft_Fahrleistung,0.75,names=F))
c_leistung2<-c(quantile(Einzelteil_T16_tidy$Fehlerhaft_Fahrleistung,0.75,names=F),quantile(Einzelteil_T19_tidy$Fehlerhaft_Fahrleistung,0.75,names=F),quantile(Einzelteil_T20_tidy$Fehlerhaft_Fahrleistung,0.75,names=F))
#Somit wird das als obere Grenze betrachtet. Wir filtern nun die Bauteile die im letzten Jahr 2018 kaputt gingen 
#obwohl sie die 75% Grenze noch nicht Ã¼berschritten haben. 
#K2LE1 defective parts
#
T11f_2018<-prepare(Einzelteil_T11,"2018-01-01",c_leistung1[1])   
T14f_2018<-prepare(Einzelteil_T14,"2018-01-01",c_leistung1[2])
T15f_2018<-prepare(Einzelteil_T15,"2018-01-01",c_leistung1[3])
#K2LE2 defective parts
T16f_2018<-prepare(Einzelteil_T16,"2018-01-01",c_leistung2[1])
T19f_2018<-prepare(Einzelteil_T19,"2018-01-01",c_leistung2[2])
T20f_2018<-prepare(Einzelteil_T20,"2018-01-01",c_leistung2[3])

#Nur T19f_2018 enthÃ¤lt EintrÃ¤ge. Die einzelbauteile T19 leben 678 Tage. Nun filtern wir die produzierten Bauteile zwischen 
#23-02-2016 (Produktionsdatum fÃ¼r die am 01-01-2018 fehlerhaft gemeldeten Bauteile) bis zum 19-09-2016 (Produktionsdatum von die letzten fehlerhaft gemeldeten Bauteile)
T19_2016<-filter(Einzelteil_T19,Produktionsdatum >= "2016-02-23" & Produktionsdatum <= "2016-09-19")
Anteil_T19f_2018<-(dim(T19f_2018)[1]/dim(T19_2016)[1])*100
Anteil_T19f_10jahre<-(dim(Einzelteil_T19_tidy)[1]/dim(Einzelteil_T19)[1])*100
#Die fehlerhaften Bauteile T19 sind circa 5.49% von der gesamten produzierten Menge im selben Zeitraum. 
#Der Anteil fehlerhafter Bauteile betrÃ¤gt in den letzen 10 Jahren 10.03%. Somit ist ein Produktionsproblem bei den 
#Bauteilen T19 ausgeschlossen. 

#Somit kann man beschlieÃen dass die Liferanten keine groÃe Produktionsprobleme in den letzten Jahren hatten, die 
#einen direkten zusammenhang mit den Kundenreklamation haben. 

#ana_Sitze.R------------------
#Es werden nun die Sitze gefiltert, die entweder fehlerhaft sind, oder einen fehlerhaften Einzelteil haben.
#Da es sich um die möglicherweise im letzten Jahr kaputt gegangenen Sitze geht, und da ein Sitz maximal 2 Jahre überlebt, 
#filtern wir die seit dem Jahr 2016 produzierten Sitze 

fehl_Komponente<- KL %>% filter(Produktionsdatum>="2016-01-01") 
  fehl_Komponente<-fehl_Komponente[,c(1,2,3,4,6)]
names(fehl_Komponente)<-c("ID Sitz","Fhl Datum St","Fhl km St","Prod Datum St","Werk St")

#Analog zu den Einzelteile fügen wir den Datensatz KL_Tf mit dem BOT zusammen.
fehl_Komponente<-filter(fehl_Komponente,`Fhl km St`<=100000)      #Die betroffenen Sitze die weniger als 100 000 Km Fahrleistung aufweisen





#Da uns nur die Sitze und die Fahrzeuge interessieren, reduzieren wir die DatensÃ¤tze BOTxy auf zwei Spalten, nÃ¤mlich ID_Sitze und ID_Fahrzeuge
BOT1<-BOT11[,c(3,5)] 
BOT2<-BOT12[,c(3,5)]
BOT3<-BOT21[,c(3,5)]
BOT4<-BOT22[,c(3,5)]
BOT<-rbind(BOT1,BOT2,BOT3,BOT4) #Diese werden zusammengefÃ¼gt in einem Datensatz
#Nun wird der Datensatz KL_TF mit der Spalte "Fahrzeug ID" erweitert.
KL_T_Cid<-merge(fehl_Komponente,BOT,by.x = "ID Sitz",by.y = "ID_Sitze")
#Die DatensÃ¤tze Oxy werden reduziert, da wir nur Werksnummer brauchen, die restlichen Informationen sind fÃ¼r unsere Analyse nicht relevant
O1<-Fahrzeuge_OEM1_Typ11[,c(1,6)]
O2<-Fahrzeuge_OEM1_Typ12[,c(1,6)]
O3<-Fahrzeuge_OEM2_Typ21[,c(1,6)]
O4<-Fahrzeuge_OEM2_Typ22[,c(1,6)]
O<-rbind(O1,O2,O3,O4) #Analog werden die Fahrzeuglisten zusammengefÃ¼gt
#Der Datensatz wird erneut erweitert
KL_cars<-merge(KL_T_Cid,O,by.x="ID_Fahrzeug",by.y="ID")  

#Wir brauchen nun die Zulassungsdaten, diese werden in unseren Datensatz eingetragen
KL_cars_zul<-merge(KL_cars,zulassung,by.x="ID_Fahrzeug",by.y="ID")

#Nun fÃ¼gen wir die Geodaten hinzu, und somit haben wir einen fertigen Datensatz was wir in die Shinyapp einfÃ¼hren kÃ¶nnen.
KL_final<-merge(KL_cars_zul,Geodaten_Gemeinden,by = "Gemeinden")

#Da sehen wir dass die in den letzten Jahren zugelassenen Autos die große Mehrheit der defekten Sitze aufweisen. Diese werden gefiltert.
Finaler_Datensatz<-filter(KL_final,Zulassung>="2016-01-01")
#Wir werden nun den Anteil der fehlerhaften Elementen (Einzelteil,Sitz,Fahrzeug) pro Werk bestimmen.
prop.table(table(Finaler_Datensatz$`Werk St`)) # 87,7% der fehlerhaften Fahrzeuge wurden im Werk 1111 (Dortmund) produziert 
prop.table(table(Finaler_Datensatz$Werksnummer)) # ungefähr 88% der zugelassenen Fahrzeuge die einen fehlerhaften Sitz haben, sind von der Marke 1
#Die Fahrzeuge 11 werden in Nürnberg und die Fahrzeuge 12 in Bonn. 

#Es wird also weiter gefiltert, das ist nun unser finaler Datenstaz. 
Finaler_Datensatz<-filter(Finaler_Datensatz,(`Werk St`==1111 & (Werksnummer==11 | Werksnummer==12)))




####ShinyApp

###Importieren Final Datensatz
gem_prod<-as.data.frame(table(Finaler_Datensatz$Gemeinden)) #Pro Gemeinde wie viele betroffene Fahrzeuge

#Es werden jetzt jeder Gemeinde die Koordinaten beigefügt
map<-merge(gem_prod,Geodaten_Gemeinden,by.x = "Var1",by.y = "Gemeinden")
#Wir betrrachten nun die Gemeinden wo mehr als 20 betrefonnene Fahrzeuge zugelassen wurden.
map<-filter(map,Freq>=100)

#Da sehen wir dass die fehlerhaften Fahrzeuge in bestimmten Gemeinden mehr betroffen sind. Wir filtern also nun die
#gemeinden die am meisten betroffen sind (Also mehr als 50 Fälle)
heatmap<-merge(Finaler_Datensatz,map,by = "PLZ")
#Wir filtern den finalen Datensatz dementsprechend:
Finaler_Datensatz<-filter(Finaler_Datensatz,PLZ %in% map$PLZ)
saveRDS(Finaler_Datensatz,"Finaler_Datensatz_26.RData")
#heatmap
heatmap<-data.matrix(table(heatmap$Gemeinden,heatmap$Zulassung))
saveRDS(heatmap,"Zusätzliche Dateien/heatmap.RData")
#Map vorbereiten für die Bubble Map die man in server.R aufruft
g <- list(
  scope = 'europe',
  projection = list(type = ''),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

#ui für die Shinyapp
ui <- dashboardPage(
  
  skin = "blue",
  dashboardHeader(title = "CASE STUDY R"),
  dashboardSidebar(
    
    
    sidebarMenu(
      
      menuItem("Analyse fehlerhafter Sitze",
               tabName = "rev",
               icon = icon("briefcase"))
    )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "rev",
        
        fluidRow(
          align="center",
          fluidRow(
            align="center",
            box(
              title = "Liste aller betroffenen zugelassenen Fahrzeuge",
              status = "success",
              solidHeader = TRUE,
              dataTableOutput('data'),
              
              width=12
            ),
            box(
              title = "Verteilung der betroffenen Fahrzeuge in den Gemeinden",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput('mapplot', height = "900px"),
              
              width=12
            ),
            
            box(
              title = "Zulassungsverlauf über Gemeinde",
              status = "success",
              solidHeader = TRUE,
              plotlyOutput('heatplot', height = "900px"),
              
              width=12
            )
            
          )
          
        )
      )
      
      
    )
  )
)

#Server von dem Shiny
server <- shinyServer(function(input, output) {
  
  data<-Finaler_Datensatz
  output$data = renderDataTable({
    DT::datatable(data, colnames = colnames(data),
                  extensions = c('Buttons','ColReorder'), options = list(
                    
                    columnDefs = list(list(className = 'dt-center', targets ="_all")),
                    scrollX=TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    
                    colReorder = TRUE
                  )) })
  
  
  output$mapplot <- renderPlotly({
    
    # build graph with ggplot syntax
    p1 <- plot_geo(map, locationmode = 'europe', sizes = c(1, 250)) %>%
      add_markers(
        x = ~Laengengrad, y = ~Breitengrad, size = ~Freq, color = ~Var1, hoverinfo = "text",
        text = ~paste(map$name, map$Freq, map$Var1)
      ) %>%
      layout(title = 'Fehlerhafte Sitze', geo = g)
    
    
  })
  
  
  output$heatplot <- renderPlotly({
    
    # build graph with ggplot syntax
    plot_ly(x=colnames(heatmap), y=rownames(heatmap), z = heatmap , type = "heatmap",color = "Grey")
    
  })
})

shinyApp(ui = ui, server = server)










