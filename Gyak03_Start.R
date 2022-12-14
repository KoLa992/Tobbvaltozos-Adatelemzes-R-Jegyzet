setwd("~/Oktat?s 2021221/T?bbv?ltoz?s adatelemz?s/R jegyzet") # ?rd ?t a saj?t Working Directory-d el?r?si ?tj?ra!

#-----------------------------------------------------------------------------------------------------------------------------

# Titanic utasok adatb?is beolvas?sa
library(readr)
titanic <- read_delim("titanic_utasok.csv", 
                      delim = ";", escape_double = FALSE,
                      locale = locale(decimal_mark = ",",
                                      grouping_mark = ".",
                                      encoding = "ISO-8859-2"), 
                      trim_ws = TRUE)
str(titanic)

#-----------------------------------------------------------------------------------------------------------------------------

# Befektet?s adatb?zis beolvas?sa
install.packages("readxl")
library(readxl)
befektetes <- read_excel("Befektetes.xlsx")
str(befektetes)

# v?rhat? hozam = ?tlagos hozam

mean(befektetes$ClimateChange_Reszveny)
mean(befektetes$BondMaxx_Kotveny)

summary(befektetes)
hist(befektetes$ClimateChange_Reszveny)

# sz?r?s: a v?ltoz?nk egy ?rt?k?nek az ?tlagt?l vett v?rhat? elt?r?se

befektetes$Elteresek_Reszveny <- befektetes$ClimateChange_Reszveny -
  mean(befektetes$ClimateChange_Reszveny)
str(befektetes)

mean(abs(befektetes$Elteresek_Reszveny))

variancia <- mean((befektetes$Elteresek_Reszveny)^2) # ^: alt gr + 3
szoras <- sqrt(variancia)

sd(befektetes$ClimateChange_Reszveny) # standard deviation = sz?r?s
sd(befektetes$BondMaxx_Kotveny)

# k?tv?ny befektet?s kev?sb? volatilis

# relat?v sz?r?s = sz?r?s / ?tlag

sd(befektetes$ClimateChange_Reszveny) / mean(befektetes$ClimateChange_Reszveny)
sd(befektetes$BondMaxx_Kotveny) / mean(befektetes$BondMaxx_Kotveny)

#-----------------------------------------------------------------------------------------------------------------------------

# ?retts?gi adatok beolvas?sa
Erettsegi <- read_excel("Erettsegi_2018.xlsx")
str(Erettsegi)

hist(Erettsegi$Matematika) # jobbra elny?l?
hist(Erettsegi$Magyar) # balra elny?l?
hist(Erettsegi$Tortenelem) # szimmetrikus / norm?lis

# Helyzetmutat?k: ?tlag, Mo, Me

gyak_tabla <- table(Erettsegi$Tortenelem)
# m?dusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 60
median(Erettsegi$Tortenelem, na.rm = TRUE) # 58
mean(Erettsegi$Tortenelem, na.rm = TRUE) # 57

# matek
gyak_tabla <- table(Erettsegi$Matematika)
# m?dusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 26
median(Erettsegi$Matematika, na.rm = TRUE) # 48
mean(Erettsegi$Matematika, na.rm = TRUE) # 51.8

# magyar
gyak_tabla <- table(Erettsegi$Magyar)
# m?dusz
gyak_tabla[gyak_tabla==max(gyak_tabla)] # 80
median(Erettsegi$Magyar, na.rm = TRUE) # 62
mean(Erettsegi$Magyar, na.rm = TRUE) # 62.6

# Centr?lis-hat?reloszl?st?tel CHT
# norm?lis eloszl?s? egy v?ltoz?, ha ?rt?kei v?letlen hat?sok
# ?sszegz?d?sek?nt ?lltak el?

hist(befektetes$ClimateChange_Reszveny)

install.packages("psych")
library(psych)

describe(Erettsegi)
# trimmed = nyesett ?tlag--> ?tlag a legnagyobb/legkisebb 10% n?lk?l

describe(befektetes)
hist(befektetes$BondMaxx_Kotveny)

# Doboz ?bra: kil?g? ?rt?keket azonos?tsuk vele

summary(titanic$Viteldij)

boxplot(titanic$Viteldij)

# interkvartilis terjedelem = IKT = Q3-Q1
35.08+1.5*(35.08-8.05)
8.05-1.5*(35.08-8.05)
# Tukey - f?le ker?t?sek = Tukey's fences
35.08+3*(35.08-8.05)
8.05-3*(35.08-8.05)

boxplot(Erettsegi$Matematika)

boxplot(titanic$Viteldij ~ titanic$Tulelt_e)


#-----------------------------------------------------------------------------------------------------------------------------

# ESS2018 adatb?zis beolvas?sa - European Social Survey 2018: magyar adatok
ESS2018 <- read_excel("ESS2018.xlsx")
str(ESS2018)
# 1568 kit?lt?
ESS2018$Partpref <- as.factor(ESS2018$Partpref)
ESS2018$Megye <- as.factor(ESS2018$Megye)
ESS2018$Regio <- as.factor(ESS2018$Regio)
str(ESS2018)

mean(ESS2018$Hirek)
median(ESS2018$Hirek)
sd(ESS2018$Hirek)
boxplot(ESS2018$Hirek)

prop.table(table(ESS2018$Partpref))

# sokas?g VS minta = r?szhalmaz a sokas?gomb?l

# mintabeli stat mutat? --> sokas?gi mutat? ?rt?k?
# el?feltelt: mintav?telnek V?LETLENNEK kell lennie-->reprezentat?v

# statisztika mutat?, amit vizsg?lunk a val?s, sokas?gi ?rt?k?t
# TORZ?TATLANUL becsli-e
# ?tlag, ar?ny, medi?n (kvantilis) torz?tatlanul becs.
# sz?r?s N-nel osztva torz?t --> sz?r?s N-1-gyel torz?tatlan

# ?tlagos h?rn?z?si id? 95%-os megb?zhat?s?g? KI-a

n <- nrow(ESS2018)
s <- sd(ESS2018$Hirek)
mintaatlag <- mean(ESS2018$Hirek)

# KI AH
mintaatlag - s/sqrt(n) * 2
# KI FH
mintaatlag + s/sqrt(n) * 2







#-----------------------------------------------------------------------------------------------------------------------------

# Riport export?l?s felt?teles form?z?ssal Excelbe
install.packages("openxlsx")
library(openxlsx)

# intervallumok ment?se
KonfInt <- groupwiseMean(Gyerekek ~ Megye,
                         data   = ESS2018,
                         conf   = 0.95,
                         digits = 3)

# Excel f?jl l?trehoz?sa
wb <- createWorkbook()

# C?l munkalap l?trehoz?sa
addWorksheet(wb, "Gyereksz?m Megy?nk?nt")

# Elmentett intervallumok ki?r?sa a munkalapra az A1 cell?t?l kezdve
writeData(wb, "Gyereksz?m Megy?nk?nt", KonfInt)

# Felt?teles form?z?s a minta?tlagokra
conditionalFormatting(wb, "Gyereksz?m Megy?nk?nt",
                      cols = 3, rows = (2:(nrow(KonfInt)+1)), # a data frame Mean (azaz 3.) oszlop?ra ?s a 2-t?l kezdve az ?sszes sor?ra vonatkozzon a form?z?s
                      style = c("red", "white", "green"), # sz?nsk?la-?tmenet megad?sa
                      rule = NULL,
                      type = "colourScale")

# Felt?teles form?z?s a konfidencia-intervallum als? hat?raira
conditionalFormatting(wb, "Gyereksz?m Megy?nk?nt",
                      cols = 5, rows = (2:(nrow(KonfInt)+1)), # a data frame Trad.lower (azaz 5.) oszlop?ra ?s a 2-t?l kezdve az ?sszes sor?ra vonatkozzon a form?z?s
                      style = c("red", "white", "green"), # sz?nsk?la-?tmenet megad?sa
                      rule = NULL,
                      type = "colourScale")

#Excel f?jl elment?se: a be?ll?tott Working Directory-ba rakja majd az eredm?nyt
saveWorkbook(wb, "GyerekszamMegye.xlsx", overwrite = TRUE)


