setwd("~/Oktat?s 2021221/T?bbv?ltoz?s adatelemz?s/R jegyzet") # ?rd ?t a saj?t Working Directory-d el?r?si ?tj?ra!

#-----------------------------------------------------------------------------------------------------------------------------

# ESS2018 adatb?zis beolvas?sa - European Social Survey 2018: magyar adatok
library(readxl)
ESS2018 <- read_excel("ESS2018.xlsx")

# adatt?pusok megfelel? be?ll?t?sa
ESS2018$Partpref <- as.factor(ESS2018$Partpref)
ESS2018$Megye <- as.factor(ESS2018$Megye)
ESS2018$Regio <- as.factor(ESS2018$Regio)
str(ESS2018)

# ?tlagos h?rn?z?si id? 95%-os megb?zhat?s?g? konfidencia-intrevalluma

n <- nrow(ESS2018)
s <- sd(ESS2018$Hirek)
mintaatlag <- mean(ESS2018$Hirek)

# Als? hat?r
mintaatlag - s/sqrt(n) * 2
# Fels? hat?r
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

#-----------------------------------------------------------------------------------------------------------------------------

# CSOK adatb?zis beolvas?sa: 15800 magyar ingatlanr?l 5 v?ltoz?
CSOK <- read_excel("CSOK.xlsx")

# adatt?pusok megfelel? be?ll?t?sa
CSOK$Settlement <- as.factor(CSOK$Settlement)
CSOK$type <- as.factor(CSOK$type)
CSOK$CSOK3 <- as.logical(CSOK$CSOK3)
str(CSOK)

# ggplot csomag telep?t?se ?s behivatkoz?sa
install.packages("ggplot2")
library(ggplot2)