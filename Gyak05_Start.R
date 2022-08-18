setwd("~/Oktatás 2021221/Többváltozós adatelemzés/R jegyzet") # Írd át a saját Working Directory-d elérési útjára!

#-----------------------------------------------------------------------------------------------------------------------------

# CSOK adatbázis beolvasása:
# 15800 magyar ingatlanról 5 változó
library(readxl)
CSOK <- read_excel("CSOK.xlsx")
str(CSOK)
unique(CSOK$Settlement)
unique(CSOK$type)

# adattípusok megfelelõ beállítása
CSOK$Settlement <- as.factor(CSOK$Settlement)
CSOK$type <- as.factor(CSOK$type)
CSOK$CSOK3 <- as.logical(CSOK$CSOK3)
str(CSOK)

# ggplot csomag telepítése és behivatkozása
install.packages("ggplot2")
library(ggplot2)

boxplot(CSOK$price)

# Doboz ábra ggplottal az árakról
ggplot(data = CSOK, aes(y = price)) +
  geom_boxplot() +
  labs(y="Lakás ára MFt-ban",
       title = "Lakásárak doboz ábrája")

CSOK <- CSOK[CSOK$price < 900,]

ggplot(data = CSOK, aes(y=area)) +
  geom_boxplot() +
  geom_hline(yintercept = 10000, color="red")

CSOK <- CSOK[CSOK$area < 10000,]

ggplot(data = CSOK, aes(y=area)) +
  geom_boxplot()

# vegyes kapcsolat
# minõségi = CSOK3
# mennyiségi = price

# 1) kapcsolat leírása grafikusan

ggplot(data = CSOK, aes(y = price, x = CSOK3,
                        fill=CSOK3)) +
  geom_boxplot() +
  labs(y="Lakás ára MFt-ban",
       title = "Lakásárak doboz ábrája")

# 2) Mennyire erõs a kapcs. a mintában

aov(price ~ CSOK3, data = CSOK)

# Sum of Squares = Négyzetösszeg
# SSBetween = SSB = 1956478
# SSResiduals = SSR = 13971232
# SSTotal = SST = SSR+SSB = 13971232+1956478
# SST = szórásnégyzet * (N-1)

var(CSOK$price)*(15798-1)
13971232+1956478

# variancia-hányados = H^2 = SSB/SST --> [0,1]
1956478/(13971232+1956478) # 12%
# CSOK jogosultság az árak alakulásának 12%-át
# magyarázza meg

# H^2 < 10% --> gyenge
# 10% <= H^2 <= 50% --> közepes
# 50% < H^2 --> erõs/szoros

# szórás-hányados = H
sqrt(1956478/(13971232+1956478)) # 0.35
# 1) NEM százalékban!
# 2) határok gyök alatt!
#     10% = 0.1 --> 0.3
#     50% = 0.5 --> 0.7

# +1) --> asszoc + korrel kapcsolatban bizonyos mutatók
# csak szórás-hányadossal összemérhetõk!

# 3) Kapcs fennáll-e a sokaságban?

# H0: H^2 = 0 || H1: H^2 > 0
# p-érték (ANOVA = Analysis Of Variances)
summary(aov(price ~ CSOK3, data = CSOK))
# p-érték <2e-16 --> kisebb még alfa=1%-nál is
# egyértelmûen elutasítható a H0

# 2 elõfeltétele
# 1) a mennyiségi változó normális eloszlású
# 2) a mennyiségi változó szórásai a minõségi
#    ismérv csoportjaiban azonosak a sokaságban

hist(log(CSOK$price))

# H0: a csoportok szórásai AZONOSAK
# H1: a csoportok szórásai közül LEGALÁBB 1 eltér


install.packages("lawstat")
library(lawstat)
# Levene-teszt --> p-érték
#  Felt: a mennyiségi változó normális elo-jú
levene.test(log(CSOK$price), CSOK$CSOK3)
# p-érték = 1.8% --> alfa=1% --> H0
# alfa = 5% --> H1

# H0: H^2 = 0 || H1: H^2 > 0
# p-érték (ANOVA = Analysis Of Variances)
summary(aov(log(price) ~ CSOK3, data = CSOK))
# p-érték <2e-16 --> H1 "a kapcsolat szignifikáns"

# p-érték < 0.01 --> H1
# 0.01 <= p-érték <= 0.1 --> döntés bizonytalan
# p-érték > 0.1 --> H0

# asszociációs
# CSOK3 ÉS Settlement

# Grafikus ábrázolás
# 100%-ig Halmozott oszlopdiagram

ggplot(data = CSOK, aes(x=Settlement, fill=CSOK3)) +
  geom_bar(position = "fill")

# kontingencia táblázat =
# = gyakorsiági táblázat 2 minõségi változóra

prop.table(table(CSOK[,c("Settlement", "CSOK3")]),1)

# Cramer-együttható (Cramer's V coefficient)
# alapötlet: megnézi, hogy néz ki a 
# kontingencia tábla ha a magyarázerõ = 0

# ettõl az állapottól vett távolságát nézi
# a mért kontingencia táblának 0-1 skálán

install.packages("questionr")
library(questionr)

cramer.v(table(CSOK[,c("Settlement", "CSOK3")]))
# 0.08 --> NEM %-ban értelmezzük!
# Képletben van gyökvonás --> szórás-hányadossal rokon
# gyenge - közepes határ = 0.3
# közepes - erõs határ = 0.7


# Fennmarad-e a sokaságban a kapcsolat?

# H0: C = 0 || H1: C > 0

# Feltétel: a kontingencia táblában
# minden elem gyakorisága > 5

# Khi-négyzet féle függetlenségvizsgálat

chisq.test(table(CSOK[,c("Settlement", "CSOK3")]))
# p-érték = < 2.2e-16 --> H1--> kapcsolat szignifikáns

# p-érték < 0.01 --> H1
# 0.01 <= p-érték <= 0.1 --> döntés bizonytalan
# p-érték > 0.1 --> H0

# Korrelációs kapcsolat

Mort <- read_excel("PreventableMortality_Eurostat.xlsx")

str(Mort) # 2017

# Korreláció mutatószám = r

cor(Mort$PreventableMortality, Mort$HealthExp)
cor(Mort$PreventableMortality, Mort$Smokers)

# -1 <= r <= +1
# 1) elõjel --> irány --> +: egyirányú || -: ellentétes
# 2) absz. érték --> kapcs. erõsségét
#       szórás-hányadossal: 0.3 közepes-gyenge
#                           0.7 erõs-közepes


# Determinációs együttható: r^2 = R^2 'R-négyzet'
# 0 <= R^2 <= 1 --> %-ban is értelmezzük!
cor(Mort$PreventableMortality, Mort$HealthExp)^2 # 39%
cor(Mort$PreventableMortality, Mort$Smokers)^2 # 24%

# Az eüg-i kiadások ismerete a megelõzhetõ halálozás
# alaklulását/szóródását kb. 39%-ban magyarázza

# R^2 a H^2 rokona --> gyenge-közepes = 0.1=10%
#                      közepes - erõs = 0.5=50%

# Pontdiagram
# Két mennyiségi változót x és y koordinátáknak
# használjuk pontdiagramon

# y = eredményváltozó
# x = magyarázóváltozó

ggplot(data = Mort, aes(x=HealthExp,
                        y=PreventableMortality)) +
  geom_point() + geom_smooth(method = lm)

# pontokra legjobban illeszkedõ egyenes =
# = regressziós egyenes
# szürke sáv = 95%-os megbízhatóságú
# konfidencia-intervalluma az egyenesnek

# regressziós egyenes egyenlete
# egyenes egyenlete --> y = m*x + b
# m = meredeksége || b = tengelymetszet

# Béta1 = -22.98
# Béta0 = 368.02

# GDP ar eüg kiadások értéke nõ 1 százalékponttal
# akkor várhatóan 22.98 eset/100 ezer fõvel kevesebb
# megelõzhetõ haláleset lesz

# 22.98*10^5 = 2.3 millió --> 1%-pont eüg-i kiadás
# 2.3 millió emberéletet ér


lm(PreventableMortality ~ HealthExp, data = Mort)

# Mo-n HealthExp = 6.79
# Becsült Halalozás = 368.02 - 22.98*HealthExp
368.02-22.98*6.79

RegEgyenes <- lm(PreventableMortality ~ HealthExp,
                 data = Mort)

Mort$PredictedMortality <- predict(RegEgyenes,
                                   Mort)
str(Mort)

# H0: Béta1 = 0 || H1: Béta1 != 0

summary(RegEgyenes)
# p-érték = 0.000234 < 0.01 --> H1
# Residual standard error: 53.94
# --> átlagos hibatár y_kalapokra










































