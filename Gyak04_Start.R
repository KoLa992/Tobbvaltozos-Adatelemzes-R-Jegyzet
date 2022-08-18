setwd("~/Oktatás 2021221/Többváltozós adatelemzés/R jegyzet") # Írd át a saját Working Directory-d elérési útjára!

#-----------------------------------------------------------------------------------------------------------------------------

# ESS2018 adatbázis beolvasása - European Social Survey 2018: magyar adatok
library(readxl)
ESS2018 <- read_excel("ESS2018.xlsx")

# adattípusok megfelelõ beállítása
ESS2018$Partpref <- as.factor(ESS2018$Partpref)
ESS2018$Megye <- as.factor(ESS2018$Megye)
ESS2018$Regio <- as.factor(ESS2018$Regio)
str(ESS2018)

# átlagos hírnézési idõ 95%-os megbízhatóságú konfidencia-intrevalluma

n <- nrow(ESS2018)
s <- sd(ESS2018$Hirek)
mintaatlag <- mean(ESS2018$Hirek)

# Alsó határ
mintaatlag - s/sqrt(n) * 2
# Felsõ határ
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

#-----------------------------------------------------------------------------------------------------------------------------

# CSOK adatbázis beolvasása: 15800 magyar ingatlanról 5 változó
CSOK <- read_excel("CSOK.xlsx")

# adattípusok megfelelõ beállítása
CSOK$Settlement <- as.factor(CSOK$Settlement)
CSOK$type <- as.factor(CSOK$type)
CSOK$CSOK3 <- as.logical(CSOK$CSOK3)
str(CSOK)

# ggplot csomag telepítése és behivatkozása
install.packages("ggplot2")
library(ggplot2)