setwd("~/Oktatás 2021221/Többváltozós adatelemzés/R jegyzet") # Írd át a saját Working Directory-d elérési útjára!

#-----------------------------------------------------------------------------------------------------------------------------

# Titanic utasok adatbáis beolvasása
library(readr)
titanic <- read_delim("titanic_utasok.csv", 
                      delim = ";", escape_double = FALSE,
                      locale = locale(decimal_mark = ",",
                                      grouping_mark = ".",
                                      encoding = "ISO-8859-2"), 
                      trim_ws = TRUE)
str(titanic)

#-----------------------------------------------------------------------------------------------------------------------------

# Befektetés adatbázis beolvasása
install.packages("readxl")
library(readxl)
befektetes <- read_excel("Befektetes.xlsx")
str(befektetes)

# várható hozam = átlagos hozam

mean(befektetes$ClimateChange_Reszveny)
mean(befektetes$BondMaxx_Kotveny)

summary(befektetes)
hist(befektetes$ClimateChange_Reszveny)

# szórás: a változónk egy értékének az átlagtól vett várható eltérése

befektetes$Elteresek_Reszveny <- befektetes$ClimateChange_Reszveny -
  mean(befektetes$ClimateChange_Reszveny)
str(befektetes)

mean(abs(befektetes$Elteresek_Reszveny))

variancia <- mean((befektetes$Elteresek_Reszveny)^2) # ^: alt gr + 3
szoras <- sqrt(variancia)

sd(befektetes$ClimateChange_Reszveny) # standard deviation = szórás
sd(befektetes$BondMaxx_Kotveny)

# kötvény befektetés kevésbé volatilis

# relatív szórás = szórás / átlag

sd(befektetes$ClimateChange_Reszveny) / mean(befektetes$ClimateChange_Reszveny)
sd(befektetes$BondMaxx_Kotveny) / mean(befektetes$BondMaxx_Kotveny)

#-----------------------------------------------------------------------------------------------------------------------------

# Érettségi adatok beolvasása
Erettsegi <- read_excel("Erettsegi_2018.xlsx")
str(Erettsegi)

hist(Erettsegi$Matematika) # jobbra elnyúló
hist(Erettsegi$Magyar) # balra elnyúló
hist(Erettsegi$Tortenelem) # szimmetrikus / normális

# Helyzetmutatók: átlag, Mo, Me

gyak_tabla <- table(Erettsegi$Tortenelem)
# módusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 60
median(Erettsegi$Tortenelem, na.rm = TRUE) # 58
mean(Erettsegi$Tortenelem, na.rm = TRUE) # 57

# matek
gyak_tabla <- table(Erettsegi$Matematika)
# módusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 26
median(Erettsegi$Matematika, na.rm = TRUE) # 48
mean(Erettsegi$Matematika, na.rm = TRUE) # 51.8

# magyar
gyak_tabla <- table(Erettsegi$Magyar)
# módusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 80
median(Erettsegi$Magyar, na.rm = TRUE) # 62
mean(Erettsegi$Magyar, na.rm = TRUE) # 62.6

# Centrális-határeloszlástétel CHT
# normális eloszlású egy változó, ha értékei véletlen hatások
# összegzõdéseként álltak elõ

hist(befektetes$ClimateChange_Reszveny)

install.packages("psych")
library(psych)

describe(Erettsegi)
# trimmed = nyesett átlag--> átlag a legnagyobb/legkisebb 10% nélkül

describe(befektetes)
hist(befektetes$BondMaxx_Kotveny)

# Doboz ábra: kilógó értékeket azonosítsuk vele

summary(titanic$Viteldij)

boxplot(titanic$Viteldij)

# interkvartilis terjedelem = IKT = Q3-Q1
35.08+1.5*(35.08-8.05)
8.05-1.5*(35.08-8.05)
# Tukey - féle kerítések = Tukey's fences
35.08+3*(35.08-8.05)
8.05-3*(35.08-8.05)

boxplot(Erettsegi$Matematika)

boxplot(titanic$Viteldij ~ titanic$Tulelt_e)


#-----------------------------------------------------------------------------------------------------------------------------

# ESS2018 adatbázis beolvasása - European Social Survey 2018: magyar adatok
ESS2018 <- read_excel("ESS2018.xlsx")
str(ESS2018)
# 1568 kitöltõ
ESS2018$Partpref <- as.factor(ESS2018$Partpref)
ESS2018$Megye <- as.factor(ESS2018$Megye)
ESS2018$Regio <- as.factor(ESS2018$Regio)
str(ESS2018)

mean(ESS2018$Hirek)
median(ESS2018$Hirek)
sd(ESS2018$Hirek)
boxplot(ESS2018$Hirek)

prop.table(table(ESS2018$Partpref))

# sokaság VS minta = részhalmaz a sokaságomból

# mintabeli stat mutató --> sokasági mutató értéké
# elõfeltelt: mintavételnek VÉLETLENNEK kell lennie-->reprezentatív

# statisztika mutató, amit vizsgálunk a valós, sokasági értékét
# TORZÍTATLANUL becsli-e
# átlag, arány, medián (kvantilis) torzítatlanul becs.
# szórás N-nel osztva torzít --> szórás N-1-gyel torzítatlan

# átlagos hírnézési idõ 95%-os megbízhatóságó KI-a

n <- nrow(ESS2018)
s <- sd(ESS2018$Hirek)
mintaatlag <- mean(ESS2018$Hirek)

# KI AH
mintaatlag - s/sqrt(n) * 2
# KI FH
mintaatlag + s/sqrt(n) * 2







#-----------------------------------------------------------------------------------------------------------------------------

# Riport exportálás feltételes formázással Excelbe
install.packages("openxlsx")
library(openxlsx)

# intervallumok mentése
KonfInt <- groupwiseMean(Gyerekek ~ Megye,
                         data   = ESS2018,
                         conf   = 0.95,
                         digits = 3)

# Excel fájl létrehozása
wb <- createWorkbook()

# Cél munkalap létrehozása
addWorksheet(wb, "Gyerekszám Megyénként")

# Elmentett intervallumok kiírása a munkalapra az A1 cellától kezdve
writeData(wb, "Gyerekszám Megyénként", KonfInt)

# Feltételes formázás a mintaátlagokra
conditionalFormatting(wb, "Gyerekszám Megyénként",
                      cols = 3, rows = (2:(nrow(KonfInt)+1)), # a data frame Mean (azaz 3.) oszlopára és a 2-tõl kezdve az összes sorára vonatkozzon a formázás
                      style = c("red", "white", "green"), # színskála-átmenet megadása
                      rule = NULL,
                      type = "colourScale")

# Feltételes formázás a konfidencia-intervallum alsó határaira
conditionalFormatting(wb, "Gyerekszám Megyénként",
                      cols = 5, rows = (2:(nrow(KonfInt)+1)), # a data frame Trad.lower (azaz 5.) oszlopára és a 2-tõl kezdve az összes sorára vonatkozzon a formázás
                      style = c("red", "white", "green"), # színskála-átmenet megadása
                      rule = NULL,
                      type = "colourScale")

#Excel fájl elmentése: a beállított Working Directory-ba rakja majd az eredményt
saveWorkbook(wb, "GyerekszamMegye.xlsx", overwrite = TRUE)


