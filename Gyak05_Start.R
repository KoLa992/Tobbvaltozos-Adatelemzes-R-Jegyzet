setwd("~/Oktat?s 2021221/T?bbv?ltoz?s adatelemz?s/R jegyzet") # ?rd ?t a saj?t Working Directory-d el?r?si ?tj?ra!

#-----------------------------------------------------------------------------------------------------------------------------

# CSOK adatb?zis beolvas?sa:
# 15800 magyar ingatlanr?l 5 v?ltoz?
library(readxl)
CSOK <- read_excel("CSOK.xlsx")
str(CSOK)
unique(CSOK$Settlement)
unique(CSOK$type)

# adatt?pusok megfelel? be?ll?t?sa
CSOK$Settlement <- as.factor(CSOK$Settlement)
CSOK$type <- as.factor(CSOK$type)
CSOK$CSOK3 <- as.logical(CSOK$CSOK3)
str(CSOK)

# ggplot csomag telep?t?se ?s behivatkoz?sa
install.packages("ggplot2")
library(ggplot2)

boxplot(CSOK$price)

# Doboz ?bra ggplottal az ?rakr?l
ggplot(data = CSOK, aes(y = price)) +
  geom_boxplot() +
  labs(y="Lak?s ?ra MFt-ban",
       title = "Lak?s?rak doboz ?br?ja")

CSOK <- CSOK[CSOK$price < 900,]

ggplot(data = CSOK, aes(y=area)) +
  geom_boxplot() +
  geom_hline(yintercept = 10000, color="red")

CSOK <- CSOK[CSOK$area < 10000,]

ggplot(data = CSOK, aes(y=area)) +
  geom_boxplot()

# vegyes kapcsolat
# min?s?gi = CSOK3
# mennyis?gi = price

# 1) kapcsolat le?r?sa grafikusan

ggplot(data = CSOK, aes(y = price, x = CSOK3,
                        fill=CSOK3)) +
  geom_boxplot() +
  labs(y="Lak?s ?ra MFt-ban",
       title = "Lak?s?rak doboz ?br?ja")

# 2) Mennyire er?s a kapcs. a mint?ban

aov(price ~ CSOK3, data = CSOK)

# Sum of Squares = N?gyzet?sszeg
# SSBetween = SSB = 1956478
# SSResiduals = SSR = 13971232
# SSTotal = SST = SSR+SSB = 13971232+1956478
# SST = sz?r?sn?gyzet * (N-1)

var(CSOK$price)*(15798-1)
13971232+1956478

# variancia-h?nyados = H^2 = SSB/SST --> [0,1]
1956478/(13971232+1956478) # 12%
# CSOK jogosults?g az ?rak alakul?s?nak 12%-?t
# magyar?zza meg

# H^2 < 10% --> gyenge
# 10% <= H^2 <= 50% --> k?zepes
# 50% < H^2 --> er?s/szoros

# sz?r?s-h?nyados = H
sqrt(1956478/(13971232+1956478)) # 0.35
# 1) NEM sz?zal?kban!
# 2) hat?rok gy?k alatt!
#     10% = 0.1 --> 0.3
#     50% = 0.5 --> 0.7

# +1) --> asszoc + korrel kapcsolatban bizonyos mutat?k
# csak sz?r?s-h?nyadossal ?sszem?rhet?k!

# 3) Kapcs fenn?ll-e a sokas?gban?

# H0: H^2 = 0 || H1: H^2 > 0
# p-?rt?k (ANOVA = Analysis Of Variances)
summary(aov(price ~ CSOK3, data = CSOK))
# p-?rt?k <2e-16 --> kisebb m?g alfa=1%-n?l is
# egy?rtelm?en elutas?that? a H0

# 2 el?felt?tele
# 1) a mennyis?gi v?ltoz? norm?lis eloszl?s?
# 2) a mennyis?gi v?ltoz? sz?r?sai a min?s?gi
#    ism?rv csoportjaiban azonosak a sokas?gban

hist(log(CSOK$price))

# H0: a csoportok sz?r?sai AZONOSAK
# H1: a csoportok sz?r?sai k?z?l LEGAL?BB 1 elt?r


install.packages("lawstat")
library(lawstat)
# Levene-teszt --> p-?rt?k
#  Felt: a mennyis?gi v?ltoz? norm?lis elo-j?
levene.test(log(CSOK$price), CSOK$CSOK3)
# p-?rt?k = 1.8% --> alfa=1% --> H0
# alfa = 5% --> H1

# H0: H^2 = 0 || H1: H^2 > 0
# p-?rt?k (ANOVA = Analysis Of Variances)
summary(aov(log(price) ~ CSOK3, data = CSOK))
# p-?rt?k <2e-16 --> H1 "a kapcsolat szignifik?ns"

# p-?rt?k < 0.01 --> H1
# 0.01 <= p-?rt?k <= 0.1 --> d?nt?s bizonytalan
# p-?rt?k > 0.1 --> H0

# asszoci?ci?s
# CSOK3 ?S Settlement

# Grafikus ?br?zol?s
# 100%-ig Halmozott oszlopdiagram

ggplot(data = CSOK, aes(x=Settlement, fill=CSOK3)) +
  geom_bar(position = "fill")

# kontingencia t?bl?zat =
# = gyakorsi?gi t?bl?zat 2 min?s?gi v?ltoz?ra

prop.table(table(CSOK[,c("Settlement", "CSOK3")]),1)

# Cramer-egy?tthat? (Cramer's V coefficient)
# alap?tlet: megn?zi, hogy n?z ki a 
# kontingencia t?bla ha a magyar?zer? = 0

# ett?l az ?llapott?l vett t?vols?g?t n?zi
# a m?rt kontingencia t?bl?nak 0-1 sk?l?n

install.packages("questionr")
library(questionr)

cramer.v(table(CSOK[,c("Settlement", "CSOK3")]))
# 0.08 --> NEM %-ban ?rtelmezz?k!
# K?pletben van gy?kvon?s --> sz?r?s-h?nyadossal rokon
# gyenge - k?zepes hat?r = 0.3
# k?zepes - er?s hat?r = 0.7


# Fennmarad-e a sokas?gban a kapcsolat?

# H0: C = 0 || H1: C > 0

# Felt?tel: a kontingencia t?bl?ban
# minden elem gyakoris?ga > 5

# Khi-n?gyzet f?le f?ggetlens?gvizsg?lat

chisq.test(table(CSOK[,c("Settlement", "CSOK3")]))
# p-?rt?k = < 2.2e-16 --> H1--> kapcsolat szignifik?ns

# p-?rt?k < 0.01 --> H1
# 0.01 <= p-?rt?k <= 0.1 --> d?nt?s bizonytalan
# p-?rt?k > 0.1 --> H0

# Korrel?ci?s kapcsolat

Mort <- read_excel("PreventableMortality_Eurostat.xlsx")

str(Mort) # 2017

# Korrel?ci? mutat?sz?m = r

cor(Mort$PreventableMortality, Mort$HealthExp)
cor(Mort$PreventableMortality, Mort$Smokers)

# -1 <= r <= +1
# 1) el?jel --> ir?ny --> +: egyir?ny? || -: ellent?tes
# 2) absz. ?rt?k --> kapcs. er?ss?g?t
#       sz?r?s-h?nyadossal: 0.3 k?zepes-gyenge
#                           0.7 er?s-k?zepes


# Determin?ci?s egy?tthat?: r^2 = R^2 'R-n?gyzet'
# 0 <= R^2 <= 1 --> %-ban is ?rtelmezz?k!
cor(Mort$PreventableMortality, Mort$HealthExp)^2 # 39%
cor(Mort$PreventableMortality, Mort$Smokers)^2 # 24%

# Az e?g-i kiad?sok ismerete a megel?zhet? hal?loz?s
# alaklul?s?t/sz?r?d?s?t kb. 39%-ban magyar?zza

# R^2 a H^2 rokona --> gyenge-k?zepes = 0.1=10%
#                      k?zepes - er?s = 0.5=50%

# Pontdiagram
# K?t mennyis?gi v?ltoz?t x ?s y koordin?t?knak
# haszn?ljuk pontdiagramon

# y = eredm?nyv?ltoz?
# x = magyar?z?v?ltoz?

ggplot(data = Mort, aes(x=HealthExp,
                        y=PreventableMortality)) +
  geom_point() + geom_smooth(method = lm)

# pontokra legjobban illeszked? egyenes =
# = regresszi?s egyenes
# sz?rke s?v = 95%-os megb?zhat?s?g?
# konfidencia-intervalluma az egyenesnek

# regresszi?s egyenes egyenlete
# egyenes egyenlete --> y = m*x + b
# m = meredeks?ge || b = tengelymetszet

# B?ta1 = -22.98
# B?ta0 = 368.02

# GDP ar e?g kiad?sok ?rt?ke n? 1 sz?zal?kponttal
# akkor v?rhat?an 22.98 eset/100 ezer f?vel kevesebb
# megel?zhet? hal?leset lesz

# 22.98*10^5 = 2.3 milli? --> 1%-pont e?g-i kiad?s
# 2.3 milli? ember?letet ?r


lm(PreventableMortality ~ HealthExp, data = Mort)

# Mo-n HealthExp = 6.79
# Becs?lt Halaloz?s = 368.02 - 22.98*HealthExp
368.02-22.98*6.79

RegEgyenes <- lm(PreventableMortality ~ HealthExp,
                 data = Mort)

Mort$PredictedMortality <- predict(RegEgyenes,
                                   Mort)
str(Mort)

# H0: B?ta1 = 0 || H1: B?ta1 != 0

summary(RegEgyenes)
# p-?rt?k = 0.000234 < 0.01 --> H1
# Residual standard error: 53.94
# --> ?tlagos hibat?r y_kalapokra










































