#### cvicenie 2.1. ####
setwd("D:/Skola/PaS2")


y = TRUE
is.numeric(y)
z = as.numeric(y)
z
is.numeric(z)


height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
ind1 <- (height > 1.75)
ind2 <- 1*ind1

is.logical(ind1)
is.logical(ind2)

# akonahle logicku premennu pouzijem s nejakou numerickou operaciou,
# tak sa z nej stava numericka premenna

height[ind1]
height[ind2]

# mozeme si pozriet, ako to funguje
height[c(1,2,3)]
# zavolala som prvy az treti udaj
height[c(1,1,3)]
# zavolala som dva krat prvy a potom treti
height[c(1,0,1)]
# zavolala som prvy, ziadny, prvy

# POZOR, R-ko indexuje od 1 !!!



#### cvicenie 2.2 ####----------------------------------------------------------
(0.1 + 0.2) == 0.3

# ale 
0.1+0.2

# skusime
(0.1 + 0.2) - 0.3

# vidime, ze je tam nejaka drobna chyba
# ak chcem vediet, ci sa nieco rovna
# je lepsie pouzit to, ze v absolutnej hodnote
# ma rozdiel byt mensi ako nieco
# to nieco je super vediet, ake ma byt podla toho,
# aka chyba moze vzniknut

abs((0.1 + 0.2) - 0.3) <= 1e-10

abs((0.1 + 0.2) - 0.3) <= 1e-15

abs((0.1 + 0.2) - 0.3) <= 1e-18



#### cvicenie 2.3 filmy ####----------------------------------------------------
install.packages("dplyr")
library(dplyr)
library(ggplot2movies)


## Aký je rozpočet akčných filmov, vydaných po 1995?

#option 1
ind <- movies$year > 1995 & movies$Action==1
#look at column called "budget"
movies[ind,"budget"]
#or 
# "budget" is in the 4th column
names(movies)
#look at the fourth column
movies[ind,4]


#option 2 directly (less readable)
movies[movies$year > 1995 & movies$Action==1,"budget"]
movies[movies$year > 1995 & movies$Action==1,4]


#option 3 nicer
# use function with, takes dataset as the first argument
with(movies, budget[year > 1995 & Action==1])


#option 4 much nicer (use dplyr package)
movies %>% 
  filter(year > 1995, Action==1) %>%
  select(budget)


# chceme zoradit animovane filmy s aspon 1000 hlasmi vydane po roku 1980
# dlhsie ako 30 minut
# podla ich hodnotenia od najlepsieho po najhorsi
?filter
?arrange
?desc

# ako by sme to robili predtym
ind_v_a_80_30 <- (movies$votes>=1000) & (movies$Animation==1) & (movies$year>1980) & (movies$length>30)
order_movies <- order(movies[ind_v_a_80_30,]$rating,decreasing=T)
movies[ind_v_a_80_30,][order_movies,]

# ked pouzijeme kniznicu dplyr
movies %>% 
  filter(votes>=1000, Animation==1, year>1980, length>30) %>%
  arrange(desc(rating))

# elegrantnejsie, citatelnejsie

# chcem dramy, rating vacsi ako 8, dlhsie ako 2hodiny
# aky je priemerny budget 20 najdrahsich filmov?

# viem si tento vystup aj ukladat ako dataframe a krajsie don pozerat


dd <- movies %>% 
  filter(Drama==1, rating > 8, length > 120) %>% 
  arrange(desc(budget))

dd[,"budget"]
mean(dd$budget[1:20])



#### Cvicenie 2.4 ####------------------------------------------------------------

bmi <- function(height, weight){
  return(weight/((height/100)^2))
}

bmi(180, 70)
bmi(70, 180)
bmi(weight = 70, height = 180)

# da sa zadat aj parameter nejaky, ktory bude defaultne prednastaveny
# vtedy ak to nezadam, tak zoberie ten prednastaveny
# dava sa to pri zadavani funkcie
bmi_def <- function(height, weight=70){
  return(weight/((height/100)^2))
}
# uzitocne, ak chcem mat moznost vykreslit aj obrazok, ale nie vzdy ho chcem kreslit

?mean
# mean(x, trim = 0, na.rm = FALSE, ...)
# vidime, ze trim je defaultne nastavene na 0, atd

#### Cvicenie 2.5 ####---------------------------------------------------------------

# suma
# musim nejako defaulne nastavit x
# aby mi to nic nezmenilo na sume, tak to bude 1
x <- 0
for (i in 1:350){
  x <- x + (i*(i-1)/(i+2))
}
x

# pozor pri pustani znova, najprv musim vynulovat to x zase
# inak mi to bude pripocitavat znova celu sumu uz k tomuto x, co mam teraz

# sucin
# tu pozor, keby sme nastavili y = 0, tak vynulujeme cely sucin
y <- 0
# preto tu potrebujeme neutralny prvok nasobenie, tj. 1
y <- 1

for (i in 1:350){
  y <- y*(i+1)/(i+2)
}
y

# mozem to aj naraz, kedze idem s i cez rovnake hodnoty

x <- 0
y <- 1

for (iNumber in 1:350){
  x <- x + (iNumber*(iNumber-1)/(iNumber+2))
  y <- y*(iNumber+1)/(iNumber+2)
}


#### Cvicenie 2.6 ####------------------------------------------------------------

# na nacitavanie csv treba Rku povedat, z ktoreho adresara ho ma tahat
# treba nastavit working directory
# getwd()
# setwd()

# session --> set working directory ---> choose/to source file location
# mozem si to napisat sem, ak ten working directory nebudem menit

# setwd("C:/Users/------/2023/pas2ex")


data_wine <- read.csv("winequality-red.csv")

plot(data_wine$alcohol,data_wine$quality)
with(data_wine,plot(alcohol,quality))

getwd()

library(ggplot2)
data_wine

ggplot(data=data_wine, aes(x = alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)

install.packages("GGally")

library(GGally)
# https://r-charts.com/correlation/ggpairs/


ggpairs(data_wine,                 # Data frame
        columns = c("quality","fixed.acidity","residual.sugar","chlorides","pH","alcohol"),        # Columns
        aes(alpha = 0.5))     # Transparency

# ak to chceme farebne podla jednotlivych skupin (kvalitu)
# musime tu premennu ale dat ako factor
is.factor(data_wine$quality)
# je na to funkcia as.factor, skontrolujeme si ju

is.factor(as.factor(data_wine$quality))

# vidime, ze uz to bude faktorova premenna, ale nezmenili sme povodnu premmenu

ggpairs(data_wine,                 # Data frame
        columns = c("fixed.acidity","residual.sugar","chlorides","pH","alcohol"),        # Columns
        aes(color = as.factor(quality),  # Color by group (cat. variable)
            alpha = 0.5))     # Transparency


#### Cvicenie 2.7 ####


data_student <- read.csv("StudentsPerformance.csv")

summary(data_student)

# vidime, ze vzdelanie rodica nie je faktorova premenna

data_student$parental.level.of.education <- as.factor(data_student$parental.level.of.education)

# skontrolujeme, ci je to uz faktorova premenna
is.factor(data_student$parental.level.of.education)
summary(data_student)
# vidime aj v sumamry, ze uz to je faktorova

# chceme si zobrazit napr. skore z matiky a citania pre deti rodicov, ktori maju len nejaku strednu skolu
library(dplyr)

ggplot(data=data_student %>% filter(parental.level.of.education=="some high school"), aes(x = math.score, y = reading.score,col=parental.level.of.education)) +
  geom_point() 

# pozrieme si aj deti rodicov s vysokou skolou, ci sa to nejako prejavi

ggplot(data=data_student %>% filter(parental.level.of.education %in% c("some high school","master's degree")), aes(x = math.score, y = reading.score,col=parental.level.of.education)) +
  geom_point() 

# vyzera, ze ak rodicia boli na vysokej, deti maju vyssie skore v matike aj v citani
