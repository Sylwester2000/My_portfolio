##### CZĘŚĆ 1 PROJEKTU #####

library(readxl) 
library(dplyr)
library(stringr)
library(gmodels)

##### WCZYTANIE DANYCH: #####
#Choroba <- read_xlsx("/Users/justyna/Desktop/STATISTICAL/Plik_1.xlsx")
Choroba <- read_xlsx("C:\\Users\\sylwe\\OneDrive\\Pulpit\\Projekt z damami\\Plik_1.xlsx")
#Choroba <- read_xlsx("C:/Users/Malgorzata/Desktop/time-series/Plik_1.xlsx")
dim(Choroba)  #Sprawdzenie wymiarów zbioru danych
names(Choroba)
colnames(Choroba) <- c('wiek', 'plec', 'b_klat_pier', 'cis', 'chol', 'cuk', 'EKG', 'max_tet', 'dusz_bol', 'zm_ST', 'skr_odc_ST', 'licz_nacz', 'defekt', 'klasa')
names(Choroba)

#Opis kolumn:
#ból_klat_pier - rodzaj bólu w klatce piersiowej
#ciś - ciśnienie krwi w spoczynku
#chol - poziom cholesterolu w surowicy
#cuk - poziom cukru we krwi na czczo
#EKG - wynik spoczynkowego EKG
#max_tet - maksymalne tętno
#dusz_bol - ból w klatce piersiowej wywołany wysiłkiem
#zm_ST - obniżenie odcinka ST w wyniku wysiłku fizycznego
##skr_odc_ST- nachylenie odcinka ST przy maksymalnym wysiłku
#licz_nacz - liczba naczyń głównych z kontrastem fluorkowym (angiografia)
#defekt - wynik badania talowego (test obrazowania perfuzji) : 3 = normalny (normal)
#                                                              6 = stały defekt (fixed defect)
#                                                              7 = odwracalny defekt (reversible defect)
#klasa- diagnoza choroby serca (status choroby na podstawie angiografii)
#jest zmienną przewidywaną, która może oznaczać ryzyko lub obecność choroby serca 
#(zwykle w kontekście diagnozy: 0 = brak choroby serca, 1 = obecna choroba serca)

# Przy zmiennych skr_odc_ST , licz_nacz , defekt pojawiają się brakujące dane - postanowiliśmy je usunąć:
sum(str_count(Choroba$skr_odc_ST,'\\?'))
sum(str_count(Choroba$licz_nacz,'\\?'))
sum(str_count(Choroba$defekt,'\\?'))
Choroba <- Choroba[, -c(11,12,13)]
names(Choroba)

#Usuwamy również obserwacje z pojedynczymi brakami w kolumnach: cis, chol, cuk, EKG, max_tet, dusz_bol
Choroba <- Choroba %>% 
  filter(!(cis == '?' | chol == '?' | EKG == '?' | cuk == '?' | max_tet == '?' | dusz_bol == '?'))
dim(Choroba)
head(Choroba)
#Usunięcie 32 obserwacji spośród 294 - nie jest to duży ubytek danych

##### PRZYGOTOWANIE DANYCH: #####
#Zamieniamy kolumny na odpowiednie typy danych (numeryczne, faktorowe)
Choroba$cis <- as.numeric(as.character(Choroba$cis))
Choroba$chol <- as.numeric(as.character(Choroba$chol))
Choroba$cuk <- as.factor(as.character(Choroba$cuk))
Choroba$max_tet <- as.numeric(as.character(Choroba$max_tet))
Choroba$dusz_bol <- as.factor(as.character(Choroba$dusz_bol))
Choroba$zm_ST <- as.numeric(as.character(Choroba$zm_ST))
#Zmieniamy kolumnę predykcji na typ czynnikowy !
Choroba$klasa <- as.factor(as.character(Choroba$klasa))

##### PROBLEM: #####
#Cel analizy: Chcemy ocenić obecność lub brak obecności choroby serca u pacjentów na podstawie różnych parametrów.

#Tworzymy macierz rozrzutu dla par zmiennych:
library(psych)
Choroba_matrix <- data.matrix(Choroba)
lookup <- c(Down='blue', Up='green')
col.ind <- lookup[Choroba$klasa]
pairs(Choroba_matrix, pch=21, col='gray', bg=col.ind)

cor(Choroba[,c(1,2,3,4,5,7,8,10)])
#Analiza korelacji między zmiennymi nie wykazuje istotnych zależności


##### ZBIÓR TRENINGOWY I TESTOWY: #####

set.seed(123) #Ustawiamy seed dla powtarzalności wyników
permuted_indices <- sample(nrow(Choroba)) #Losowe permutowanie indeksów
Choroba <- Choroba[permuted_indices,]

## Tworzymy zbiór uczący i testowy:
train_indices <- 1:196
test_indices <- 197:261

Choroba_train <- Choroba[train_indices,]
Choroba_test <- Choroba[test_indices,]
Choroba_test_pred <- Choroba_test$klasa #Klasy rzeczywiste zbioru testowego

# Proporcja podziału klas w zbiorach 
prop.table(table(Choroba_test$klasa))
prop.table(table(Choroba_train$klasa))
head(Choroba)


##### Normalizacja danych #####
# Sprawdzamy czy zmienne mają rozkład normalny. 
# Zostało to sprawdzone wcześniej.

# Przygotowanie funkcji z -score:
norm <- function(x) {
  return ((x - mean(x)) / sd(x))
}

numeric_columns <- c("wiek", "plec", "b_klat_pier", "EKG", "cis",  "chol", "max_tet", "zm_ST")

Choroba_train_1 <- Choroba_train
Choroba_test_1 <- Choroba_test

# Cholesterol
shapiro.test(Choroba_train$chol)
shapiro.test(Choroba_test$chol)

# Normalizacja danych
Choroba_train_1[numeric_columns] <- lapply(Choroba_train_1[numeric_columns], norm)
Choroba_test_1[numeric_columns] <- lapply(Choroba_test_1[numeric_columns], norm)

# Cholesterol
shapiro.test(Choroba_train_1$chol)
boxplot(Choroba_train_1$chol)

shapiro.test(Choroba_test_1$chol)
boxplot(Choroba_test_1$chol)

##### Cholesterol
## Usuwanie wartości odstających w train 
Q1 <- quantile(Choroba_train_1$chol, 0.25)
Q3 <- quantile(Choroba_train_1$chol, 0.75)
IQR <- Q3 - Q1
outliers <- Choroba_train_1$chol < (Q1 - 1.5 * IQR) | Choroba_train_1$chol > (Q3 + 1.5 * IQR)
Choroba_train_1 <- Choroba_train_1[!outliers, ]

boxplot(Choroba_train_1$chol)
shapiro.test(Choroba_train_1$chol)

## Usuwanie wartości odstających w test 
Q1 <- quantile(Choroba_test_1$chol, 0.25)
Q3 <- quantile(Choroba_test_1$chol, 0.75)
IQR <- Q3 - Q1
outliers <- Choroba_test_1$chol < (Q1 - 1.5 * IQR) | Choroba_test_1$chol > (Q3 + 1.5 * IQR)
Choroba_test_1 <- Choroba_test_1[!outliers, ]

boxplot(Choroba_test_1$chol)
shapiro.test(Choroba_test_1$chol)


##### max_tet
shapiro.test(Choroba_train_1$max_tet)
boxplot(Choroba_train_1$max_tet)

shapiro.test(Choroba_test_1$max_tet)
boxplot(Choroba_test_1$max_tet)

## Usuwanie wartości odstających w train 
Q1 <- quantile(Choroba_train_1$max_tet, 0.25)
Q3 <- quantile(Choroba_train_1$max_tet, 0.75)
IQR <- Q3 - Q1
outliers <- Choroba_train_1$max_tet < (Q1 - 1.5 * IQR) | Choroba_train_1$max_tet > (Q3 + 1.5 * IQR)
Choroba_train_1 <- Choroba_train_1[!outliers, ]

#print(head(Choroba1))
boxplot(Choroba_train_1$max_tet)
shapiro.test(Choroba_train_1$max_tet)

## Usuwanie wartości odstających w test
Q1 <- quantile(Choroba_test_1$max_tet, 0.25)
Q3 <- quantile(Choroba_test_1$max_tet, 0.75)
IQR <- Q3 - Q1
outliers <- Choroba_test_1$max_tet < (Q1 - 1.5 * IQR) | Choroba_test_1$max_tet > (Q3 + 1.5 * IQR)
Choroba_test_1 <- Choroba_test_1[!outliers, ]

#print(head(Choroba1))
boxplot(Choroba_test_1$max_tet)
shapiro.test(Choroba_test_1$max_tet)

# Tworzymy zbiory pomniejszone o wartości odstające
Choroba_train <- Choroba_train_1
Choroba_test <- Choroba_test_1

# Proporcja podziału klas w zbiorach 
prop.table(table(Choroba_test$klasa))
prop.table(table(Choroba_train$klasa))

# Ilość obserwacji w zbiorach po usunięciu obserwacji odstających
nrow(Choroba_train)
nrow(Choroba_test)

##### BUDOWA MODELU LDA: #####
Choroba_test_pred <- Choroba_test$klasa

library(MASS)

##Budowanie modelu LDA na zbiorze treningowym:
lda.fit1 <- lda(klasa ~ wiek + plec + b_klat_pier + cis + chol + cuk + EKG + max_tet + dusz_bol + zm_ST, data = Choroba, subset = train_indices)
lda.fit1

##Histogramy dla jednej funkcji LD1:
plot(lda.fit1)
# NIE nakładanie się tych histogramów oznacza, że nasz model dobrze separuje dane

##Predykcja na zbiorze testowym:
lda.pred1 <- predict(lda.fit1, Choroba_test)

#posterior - macierz prawdopodobieństw a posteriori dla obu klas
#x- wartość liniowych dyskryminant

lda.class1 <- lda.pred1$class
CrossTable(Choroba_test$klasa, lda.class1, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(lda.class1 == Choroba_test_pred)

##Wykres analizy modelu LDA:
install.packages("klaR")
install.packages("shiny")
library(klaR)

partimat(klasa ~ chol + zm_ST, data=Choroba_train, method = "lda")
#Partition Plot - wykres podziału, pokazuje, jak przestrzeń zmiennych jest podzielona na różne klasy w wyniku zastosowania
#danego klasyfikatora jak np. lda/qda

##Ustawienie progu prawdopodobieństw w modelu LDA:
lda.pred.posterior <- lda.pred1$posterior  # Macierz prawdopodobieństw a posteriori

##Przykładowy próg: prawdopodobieństwo klasy 1 (choroba obecna) musi wynosić co najmniej 0.6
lda.class2.threshold <- ifelse(lda.pred.posterior[, 2] >= 0.6, 1, 0)
CrossTable(Choroba_test$klasa, lda.class2.threshold, #Tablica wyników z progiem 0.6
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))   
mean(lda.class2.threshold == Choroba_test_pred)  #Trafność klasyfikacji z progiem 0.6

lda.class3.threshold <- ifelse(lda.pred.posterior[, 2] >= 0.8, 1, 0)
CrossTable(Choroba_test$klasa, lda.class3.threshold, #Tablica wyników z progiem 0.8
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))  
mean(lda.class3.threshold == Choroba_test_pred)  #Trafność klasyfikacji z progiem

#Zmieniliśmy domyślny próg prawdopodobieństwa dla klasyfikacji klasy 1 (choroba obecna) na 0,6, by zwiększyć dokładność modelu 
#w identyfikowaniu przypadków choroby
#Po zmianie progu model uzyskał  dokładność 72,13% przy różnym podziale klasyfikacji.

# Biblioteka do ROC
install.packages("pROC")
library(pROC)

# Obliczenie prawdopodobieństw klasy 1 
lda.pred.posterior <- lda.pred1$posterior[, 2]
roc_curve <- roc(Choroba_test_pred, lda.pred.posterior) # Krzywa ROC
auc(roc_curve)  # Współczynnik AUC

# Wykres ROC
par(mfrow = c(1, 1))
plot(roc_curve, col = "blue", main = "ROC dla modelu LDA", lwd = 2)

##### MODEL QDA: #####
#Budowanie modelu QDA na zbiorze treningowym:
qda.fit1 <- qda(klasa ~ wiek + plec + b_klat_pier + cis + chol + cuk + EKG + max_tet + dusz_bol + zm_ST, 
               data = Choroba, subset = train_indices)
qda.fit1

##Predykcja na zbiorze testowym:
qda.pred1 <- predict(qda.fit1, Choroba_test)
qda.class1 <- qda.pred1$class  #Domyślna klasyfikacja bez progu
CrossTable(Choroba_test$klasa, qda.class1, #Tablica wyników 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))   
mean(qda.class1 == Choroba_test$klasa)  #Trafność klasyfikacji bez progu

#Ustawienie progu prawdopodobieństw:
qda.pred.posterior <- qda.pred1$posterior  # Macierz prawdopodobieństw a posteriori

#Przykładowy próg: prawdopodobieństwo klasy 1 (choroba obecna) musi wynosić co najmniej 0.6
qda.class2.threshold <- ifelse(qda.pred.posterior[, 2] >= 0.6, 1, 0)
CrossTable(Choroba_test$klasa, qda.class2.threshold, #Tablica wyników 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted')) 
mean(qda.class2.threshold == Choroba_test$klasa)  

qda.class3.threshold <- ifelse(qda.pred.posterior[, 2] >= 0.8, 1, 0)
CrossTable(Choroba_test$klasa, qda.class3.threshold, #Tablica wyników 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted')) 
mean(qda.class3.threshold == Choroba_test$klasa)  

#Zmieniliśmy domyślny próg prawdopodobieństwa na 0,6 i 0,8.
#Jednak nie polepszyło to naszej jakości początkowej która wynosiła 77,05%.


# Krzywa ROC
qda.pred.posterior <- qda.pred1$posterior[, 2]
roc_curve <- roc(Choroba_test_pred, qda.pred.posterior) # Krzywa ROC
auc(roc_curve)  # Współczynnik AUC

# Wykres ROC
plot(roc_curve, col = "blue", main = "ROC dla modelu QDA", lwd = 2)



# PODSUMOWANIE:
# Zastosowanie progów w LDA i QDA pozwala dostosować model do sytuacji, w której chcemy być bardziej
# lub mniej pewni obecności choroby. Próg 0.5 jest standardowy i zakłada równe prawdopodobieństwo.
# Wyższy próg dla klasy 1 oznacza, że model musi być bardziej pewny, aby przewidzieć chorobę, co
# zmniejsza liczbę fałszywych pozytywów kosztem potencjalnie większej liczby fałszywych negatywów.
# Eksperymentowanie z progami umożliwia optymalizację klasyfikatora w zależności od potrzeb.



#############################################
#CZĘŚĆ 2 PROJEKTU 

########## DRZEWA DECYZYJNE ###########
#Model:
library(tree)
par(mfrow = c(1, 1))

#### Budowa drzewa klasyfikacyjnego:
tree.Choroba_1 <- tree(klasa ~ ., data = Choroba_train)
summary(tree.Choroba_1)
tree.Choroba_1
# kryterium podziału:
# liczba obserwacji spełniających kryterium w danej gałęzi,
# dewiancja,
# predykcja dla danego punktu gałęzi/drzewa,
# frakcje przykładów dla danych klas

plot(tree.Choroba_1)
text(tree.Choroba_1, pretty = 0)

# Predykcja głównego modelu
tree.pred_1 <- predict(tree.Choroba_1, Choroba_test, type = "class")
CrossTable(Choroba_test$klasa, tree.pred_1, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(tree.pred_1 == Choroba_test$klasa)

#### Przycinanie drzewa - wyszukanie odpowiedniego rozmiaru alpha
#### Używając CV
cv.Choroba <- cv.tree(tree.Choroba_1, FUN = prune.misclass)
par(mfrow = c(1, 2))

# cv.tree() zwraca: liczbę liści dla każdego drzewa (size),
# błąd klasyfikacji z CV (u nas, mimo że jest podpisany dev),
# współczynnik złożoności drzewa (k jako \alpha)

plot(cv.Choroba$size, cv.Choroba$dev, type = "b")
plot(cv.Choroba$k, cv.Choroba$dev, type = "b")
cv.Choroba
which.min(cv.Choroba$dev)

# 1) Właściwe przycinanie drzewa, dla size = 4 i size = 5
prune.Choroba2 <- prune.tree(tree.Choroba_1, best = 5)
par(mfrow = c(1, 1))
plot(prune.Choroba2)
text(prune.Choroba2, pretty = 0)

## Sprawdzamy jak przycięte drzewo zachowuje się na zbiorze testowym:
tree.pred2 <- predict(prune.Choroba2, Choroba_test, type = "class")
CrossTable(Choroba_test$klasa, tree.pred2, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(tree.pred2 == Choroba_test$klasa)


# 2) Właściwe przycinanie drzewa, dla size = 3
prune.Choroba3 <- prune.tree(tree.Choroba_1, best = 3)
plot(prune.Choroba3)
text(prune.Choroba3, pretty = 0)

## Sprawdzamy jak przycięte drzewo zachowuje się na zbiorze testowym:
tree.pred3 <- predict(prune.Choroba3, Choroba_test, type = "class")
CrossTable(Choroba_test$klasa, tree.pred3, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(tree.pred3 == Choroba_test$klasa)

# 3) Właściwe przycinanie drzewa, dla size = 6
prune.Choroba4 <- prune.tree(tree.Choroba_1, best = 6)

plot(prune.Choroba4)
text(prune.Choroba4, pretty = 0)

## Sprawdzamy jak przycięte drzewo zachowuje się na zbiorze testowym:
tree.pred4 <- predict(prune.Choroba4, Choroba_test, type = "class")
CrossTable(Choroba_test$klasa, tree.pred4, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(tree.pred4 == Choroba_test$klasa)

# 4) Właściwe przycinanie drzewa, dla size = 7
prune.Choroba5 <- prune.tree(tree.Choroba_1, best = 7)
plot(prune.Choroba5)
text(prune.Choroba5, pretty = 0)

## Sprawdzamy jak przycięte drzewo zachowuje się na zbiorze testowym:
tree.pred5 <- predict(prune.Choroba5, Choroba_test, type = "class")
CrossTable(Choroba_test$klasa, tree.pred5, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(tree.pred5 == Choroba_test$klasa)


########### BAGGING ###########
library(randomForest)

## Budowa modelu metodą bagging dla wszystkich zmiennych objaśniających:
bag.Choroba1 <- randomForest(klasa ~ ., data = Choroba_train, 
                             mtry = 10, importance = TRUE) #mtry ilość cech - zmiennych objaśniających
bag.Choroba1

# Sprawdzamy model na danych testowych:
yhat.bag1 <- predict(bag.Choroba1, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag1, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag1 == Choroba_test$klasa)

### Szukamy ręcznie najlepszego współczynnika ntree - liczba drzew
# ntree = 10
bag.Choroba2 <- randomForest(klasa ~ ., data = Choroba_train,
                             mtry = 10, importance = TRUE, ntree = 10)
yhat.bag2 <- predict(bag.Choroba2, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag2, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag2 == Choroba_test$klasa)

# ntree=20
bag.Choroba3 <- randomForest(klasa ~ ., data = Choroba_train,
                             mtry = 10, importance = TRUE, ntree = 20)
yhat.bag3 <- predict(bag.Choroba3, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag3, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag3 == Choroba_test$klasa)

# ntree=50
bag.Choroba4 <- randomForest(klasa ~ ., data = Choroba_train,
                             mtry = 10, importance = TRUE, ntree = 50) 
yhat.bag4 <- predict(bag.Choroba4, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag4, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag4 == Choroba_test$klasa)

# ntree=100
bag.Choroba5 <- randomForest(klasa ~ ., data = Choroba_train,
                             mtry = 10, importance = TRUE, ntree = 100)
yhat.bag5 <- predict(bag.Choroba5, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag5, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag5 == Choroba_test$klasa)

# ntree=170
bag.Choroba6 <- randomForest(klasa ~ ., data = Choroba_train,
                             mtry = 10, importance = TRUE, ntree = 170) 
yhat.bag6 <- predict(bag.Choroba6, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.bag6, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.bag6 == Choroba_test$klasa)

########### LAS LOSOWY ###########

#Funkcja importance() wyświetla ranking istotności zmiennych:
importance(bag.Choroba1)

# Bierzemy mniejszą liczbę zmiennych objaśniających do modelu.
# Dla trzech zmiennych  - jest to odpowiednik zaokrąglonego pierwiastka z ilości zmiennych objaśniających
rf.Choroba1 <- randomForest(klasa ~ ., data = Choroba_train,
                            mtry = 3, importance = TRUE)
yhat.rf1 <- predict(rf.Choroba1, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.rf1, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf1 == Choroba_test$klasa)

# Ranking istotności zmiennych
importance(rf.Choroba1)
varImpPlot(rf.Choroba1)

# dla 5 zmiennych
rf.Choroba2 <- randomForest(klasa ~ ., data = Choroba_train,
                            mtry = 5, importance = TRUE)
yhat.rf2 <- predict(rf.Choroba2, newdata = Choroba_test)
CrossTable(Choroba_test$klasa, yhat.rf2, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf2 == Choroba_test$klasa)

# Ranking istotności zmiennych
importance(rf.Choroba2)
varImpPlot(rf.Choroba2)

############## BOOSTING ############

# Przekonwertowanie naszych zbiorów danych
Choroba_train_boo <- Choroba_train
Choroba_test_boo <- Choroba_test
Choroba_train_boo$klasa <- as.numeric(Choroba_train_boo$klasa) - 1 
Choroba_test_boo$klasa <- as.numeric(Choroba_test_boo$klasa) - 1 
#Tutaj zmieniamy typ na numeryczny, ale przypisanie odbywa się za pomocą liczb 1,2, więc odejmujemy 1, by dostać liczby 0,1

library(gbm)
#set.seed(123)
boost.Choroba1 <- gbm(klasa ~ ., data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 50,
                      interaction.depth = 1)

summary(boost.Choroba1)

#Sprawdzamy nasz model na danych testowych:
yhat.rf.boo1 <- predict(boost.Choroba1, newdata = Choroba_test_boo, ntrees = 50)
yhat.rf.boo1 <- ifelse(yhat.rf.boo1 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo1, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo1 == Choroba_test_boo$klasa)

#Domyślnie lambda = 0.1, ale można to zmieniać za pomocą parametru shrinkage:
boost.Choroba2 <- gbm(klasa ~ .,  data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 20,
                      interaction.depth = 1, shrinkage = 0.2)

yhat.Choroba.boo2 <- predict(boost.Choroba2, newdata = Choroba_test_boo, n.trees = 20)
yhat.rf.boo2 <- ifelse(yhat.Choroba.boo2 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo2, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo2 == Choroba_test$klasa)


boost.Choroba3 <- gbm(klasa ~ .,  data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 20,
                      interaction.depth = 1)

yhat.Choroba.boo3 <- predict(boost.Choroba3, newdata = Choroba_test_boo, n.trees = 20)
yhat.rf.boo3 <- ifelse(yhat.Choroba.boo3 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo3, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo3 == Choroba_test$klasa)


# Do tej pory próbowaliśmy wyszukać najlepszy model używając boostingu
# szukając odpowiednich hiperparametrów: shrinkage oraz ntrees.
# W tym momencie spróbujemy zbudować pętle, w których dla odpowiednich hiperparametrów
# będzie budowany model, na którym będzie oceniania jakość.

library(pROC)

# Zbiory hiperprarametrów
ntrees_c <- seq(10,150, by = 20)
shrinkage_c <- c(0.01, 0.05, 0.1, 0.3, 0.5)

# Zmienne do przechowywania danych
wyniki1 <- data.frame() # ramka do przechowywania powyższych danych

for(shrinkage in shrinkage_c)
{
  for (ntrees in ntrees_c)
  {
    boost.Choroba_model <- gbm(klasa ~ .,  data = Choroba_train_boo,
                               distribution = "bernoulli", n.trees = ntrees,
                               interaction.depth = 1, shrinkage = shrinkage, cv.folds = 5)
    
    # Predykcja dla zbioru testowego 
    pred_model <- predict(boost.Choroba_model, newdata = Choroba_test_boo, n.trees = ntrees)
    roc_obj <- roc(Choroba_test$klasa, pred_model) # budowa krzywej ROC
    auc <- auc(roc_obj) # współczynnik auc
    
    # Dodanie wyników do ramki
    wyniki1 <- rbind(wyniki1, c(shrinkage, ntrees, auc))
    
  }
}


colnames(wyniki1) <- c("shrinkage", "ntrees", "auc")
#wyniki
#wyniki[order(-wyniki[,3]),]
top_5_auc <- head(wyniki1[order(-wyniki1[,3]),], 10)
top_5_auc

# Spróbujemy zbudować trzy najlepsze modele według współczynnika AUC
# Pamiętajmy jednak, że należy pamiętać o błędzie I typu, którego nie kontrolowaliśmy
# w trakcie wyboru najlepszego modelu względem współczynnika AUC

# Model dla n.trees = 10, shrinkage = 0.5
boost.Choroba4 <- gbm(klasa ~ .,data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 10,
                      interaction.depth = 1, shrinkage = 0.5)

yhat.Choroba.boo4 <- predict(boost.Choroba4, newdata = Choroba_test_boo, n.trees = 20)
yhat.rf.boo4 <- ifelse(yhat.Choroba.boo4 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo4, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo4 == Choroba_test$klasa)

# Model dla n.trees = 70, shrinkage = 0.05
boost.Choroba5 <- gbm(klasa ~ .,  data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 70,
                      interaction.depth = 1, shrinkage = 0.05)

yhat.Choroba.boo5 <- predict(boost.Choroba5, newdata = Choroba_test_boo, n.trees = 20)
yhat.rf.boo5 <- ifelse(yhat.Choroba.boo5 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo5, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo5 == Choroba_test$klasa)

# Model dla n.trees = 130, shrinkage = 0.1
boost.Choroba6 <- gbm(klasa ~ .,  data = Choroba_train_boo,
                      distribution = "bernoulli", n.trees = 130,
                      interaction.depth = 1, shrinkage = 0.1)

yhat.Choroba.boo6 <- predict(boost.Choroba6, newdata = Choroba_test_boo, n.trees = 20)
yhat.rf.boo6 <- ifelse(yhat.Choroba.boo6 > 0.5, 1, 0)
CrossTable(Choroba_test_boo$klasa, yhat.rf.boo6, 
           prop.chisq = FALSE, prop.c= FALSE, prop.r =FALSE,
           dnn= c('actual', 'predicted'))
mean(yhat.rf.boo6 == Choroba_test$klasa)

##### XGBoost #####
library(xgboost)
library(caret)
# Przygotowujemy dane treningowe i testowe 
# poprzez zrobienia macierzy z danych objaśniających (wtedy mamy macierz liczb!)
# oraz wektora liczb z danych objaśnianych
Choroba_train_x <- data.matrix(Choroba_train[,-11])
Choroba_train_y <- unlist(as.numeric(Choroba_train$klasa) - 1)  # bo as.numeric robi 1,2 zamiast 0,1
Choroba_test_x <- data.matrix(Choroba_test[,-11])
Choroba_test_y <- unlist(as.numeric(Choroba_test$klasa) - 1)
# Tworzymy macierz xgb.Matrix
xgb_train <- xgb.DMatrix(data = Choroba_train_x, label = Choroba_train_y) 
xgb_test <- xgb.DMatrix(data = Choroba_test_x, label = Choroba_test_y)

# Zbiory hiperprarametrów
eta_c <- seq(0.1, 1, by = 0.1)
max_depth_c <- seq(2, 10, by = 1)
# nrounds - ale ten nie będzie potrzebny by go implementować

# Zmienne do przechowywania danych
best_nround_rmse <- 0 # miejsce najmniejszego błędu
best_nround_auc <- 0 # miejsce wskaźnika największej jakości modelu
best_rmse_test <- 0 # wartość najmniejszego błędu
best_auc_test <- 0 # wartość największej jakości modelu
wyniki <- data.frame() # ramka do przechowywania powyższych danych

# Budujemy miarę FPR - false positive rate
# jako dodatkowy wskaźnik, który będziemy minimalizować
# Dlaczego?
# Uwględnić go chcemy w CV, zobaczyć na jakim poziomie modele popełniają błędy I typu
# Naszym zdaniem warto uwzględnić go w wyborze odpowiednich parametrów

# Positive - zdrowy - 0
# Negative - chory - 1
# FP = (actual = 1 i pred = 0)
# TN + FP = (actual=1)
# FPR = FP / (TN + FP)

# Budowa miary FPR:
FPR_funtion <- function(pred, train) {
  klasa <- getinfo(train, "label") # funkcja getinfo służy do pobierania "metadanych" z obiektu klasy xgb.DMatrix
  pred_klasa <- ifelse(pred > 0.5, 1, 0)
  FP <- sum(pred_klasa == 0 & klasa == 1)  
  TN <- sum(klasa == 1)  
  FPR <- FP / TN
  return(FPR)
}


# Sprawdzaliśmy, czy znaczenie ma od jakiej pętli zaczniemy, czy dla eta czy dla max_depth - nie ma to jednak znaczenia
for(eta in eta_c)
{
  for(max_depth in max_depth_c)
  {   
    cv <- xgb.cv(data = xgb_train, max_depth = max_depth, eta = eta, nrounds = 100, 
                 nthread = 2, objective = "binary:logistic", eval_metric ="auc", nfold = 4, prediction = TRUE)
    
    # błąd rmse w problemie klasyfikacji jest "mało" interesujący
    # best_nround_rmse <- which.min(cv$evaluation_log$test_rmse_mean)
    # best_rmse_test <- cv$evaluation_log$test_rmse_mean[best_nround_rmse]
    
    # auc, czyli miara jakości modeli ( jak dobrze model odróżnia obie klasy)
    # w przypadku problemu klasyfikacji jest dobrym rozwiązaniem
    # czyli dla danej ety i max_depth, wybieramy najlepszą iterację nrounds
    # przy której model ma największą wartość auc
    
    best_nround_auc <- which.max(cv$evaluation_log$test_auc_mean)
    best_auc_test <- cv$evaluation_log$test_auc_mean[best_nround_auc]
    
    # Predykcje z wyników cross-validation i policzenie FPR
    pred <- cv$pred
    result <- FPR_funtion(pred, xgb_test)
    FPR <- result 
    
    # dopisujemy najlepsze wyniki dla danej ety i max_depth do listy najlepszych wyników
    wyniki <- rbind(wyniki, data.frame(eta, max_depth, best_nround_auc, best_auc_test, FPR)) 
  }   
}

# Wybieramy trzy najlepsze wyniki ze względu na AUC
# i dla danych et, max_depth oraz nrounds budujemy model
top_3_best_auc_test <- head(wyniki[order(-wyniki$best_auc_test),], 10)
top_3_best_auc_test

top_3_fpr <- head(wyniki[order(wyniki$FPR),], 10)
top_3_fpr

# Budujemy 3 modele z najlepszymi jakościami modelu:
# 1) eta = 0.5, max_depth = 2, best_nround_auc = 13
model1 <- xgboost(data = xgb_train, eta = 0.5, max.depth = 2, nrounds = 13, objective = "binary:logistic", eval_metric = list("auc","error"))
pred1 <- predict(model1, xgb_test)
pred_class1 <- ifelse(pred1 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class1, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))
mean(pred_class1 == Choroba_test_y)

# 2) eta = 0.2, max_depth = 7, best_nround_auc = 7
model2 <- xgboost(data = xgb_train, eta = 0.2, max.depth = 7, nrounds = 7, objective = "binary:logistic", eval_metric = list("auc","error"))
pred2 <- predict(model2, xgb_test)
pred_class2 <- ifelse(pred2 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class2, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
mean(pred_class2 == Choroba_test_y)

# 3) eta = 0.1, max_depth = 2, best_nround_auc = 81
model3 <- xgboost(data = xgb_train, eta = 0.1, max.depth = 2, nrounds = 81, objective = "binary:logistic", eval_metric = list("auc","error"))
pred3 <- predict(model3, xgb_test)
pred_class3 <- ifelse(pred3 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class3, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
mean(pred_class3 == Choroba_test_y)

# Budujemy jeszcze 2 modele z najlepszymi jakościami modelu uwzględniając wskaźnik FPR:

# 4) eta = 0.4, max_depth = 8, best_nround_auc = 13
model4 <- xgboost(data = xgb_train, eta = 0.4, max.depth = 8, nrounds = 13, objective = "binary:logistic", eval_metric = list("auc","error"))
pred4 <- predict(model4, xgb_test)
pred_class4 <- ifelse(pred4 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class4, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
mean(pred_class4 == Choroba_test_y)

# 5) eta = 0.3, max_depth = 7, best_nround_auc = 3
model5 <- xgboost(data = xgb_train, eta = 0.3, max.depth = 7, nrounds = 3, objective = "binary:logistic", eval_metric = list("auc","error"))
pred5 <- predict(model5, xgb_test)
pred_class5 <- ifelse(pred5 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class5, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted')) 
mean(pred_class5 == Choroba_test_y)

# Zrobimy jeszcze inny model na wymyślonych przez nad danych
# 6) eta = 0.6, max_depth = 10, best_nround_auc = 7
model6 <- xgboost(data = xgb_train, eta = 0.6, max.depth = 10, nrounds = 7, objective = "binary:logistic", eval_metric = "error")
pred6 <- predict(model5, xgb_test)
pred_class6 <- ifelse(pred5 > 0.5, 1, 0)
CrossTable(Choroba_test$klasa, pred_class6, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
mean(pred_class6 == Choroba_test_y)

##### BART #####
library(BART)
# Uruchommy z domyślnymi parametrami:
bartfit <- pbart(Choroba_train_x, Choroba_train_y, x.test = Choroba_test_x )
yhat.bart <- bartfit$prob.test.mean  # Prawdopodobieństwo klasy pozytywnej
yhat.class <- ifelse(yhat.bart > 0.5, 1, 0)

# Obliczenie jakości modelu
CrossTable(Choroba_test$klasa, yhat.class, 
           prop.chisq = FALSE,  prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
mean(yhat.class == Choroba_test_y)

# Teraz spróbujemy samodzielnie dobrać paramtr ntree.

# ntree=230

# Uruchommy model z ntree = 230:
bartfit_230 <- pbart(Choroba_train_x, Choroba_train_y, x.test = Choroba_test_x, ntree = 230)
yhat_bart_230 <- bartfit_230$prob.test.mean  # Prawdopodobieństwo klasy pozytywnej
yhat_class_230 <- ifelse(yhat_bart_230 > 0.5, 1, 0)


CrossTable(Choroba_test$klasa, yhat_class_230, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
accuracy_230 <- mean(yhat_class_230 == Choroba_test_y)
accuracy_230

# ntree= 330


bartfit_330 <- pbart(Choroba_train_x, Choroba_train_y, x.test = Choroba_test_x, ntree = 330)
yhat_bart_330 <- bartfit_330$prob.test.mean  
yhat_class_330 <- ifelse(yhat_bart_330 > 0.5, 1, 0)

# Obliczenie jakości modelu
CrossTable(Choroba_test$klasa, yhat_class_330, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
accuracy_330 <- mean(yhat_class_330 == Choroba_test_y)
accuracy_330


# ntree=430

bartfit_430 <- pbart(Choroba_train_x, Choroba_train_y, x.test = Choroba_test_x, ntree = 430)
yhat_bart_430 <- bartfit_430$prob.test.mean 
yhat_class_430 <- ifelse(yhat_bart_430 > 0.5, 1, 0)

# Obliczenie jakości modelu
CrossTable(Choroba_test$klasa, yhat_class_430, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,      
           dnn = c('actual', 'predicted'))  
accuracy_430 <- mean(yhat_class_430 == Choroba_test_y)
accuracy_430


#############

