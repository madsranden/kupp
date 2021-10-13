

#### Pakker ####
library(tidyverse)
library(dplyr)
library(randomForest)
library(e1071)
library(pROC)


#### Setter working directory og laster inn datasett ####
load("C:/Users/solsh/OneDrive/Documents/STV1515/Seminar 8/coups.RData")
coups <- x
remove(x)
fore

#### For-loops ####

# For-loops inneholder følgende argumenter:
## 1. "for", som indikerer at vi vil ha en for-loop
## 2. En indikator som sier at vi vil gjøre hver operasjon på denne.
## 3. Et objekt med mange verdier (en vektor) som vi har liggende.
## 4. En kode mellom krøllparentesene som sier hva slags funksjon som skal gjøres for hver indikator i vektoren (dvs. hver i).

# for(i in vector) {
#   .. do something ..
# }

# Eksempel 1: For hver enhet av "year", print verdi av democracy

for (i in coups$year) {
  demo <- print(coups$democracy)
  return(demo)
}


# Eksempel 2: For hver kolonne i coups, gi meg gjennomsnittet av variablene

out <- vector("double", ncol(coups)) # Vi må lage et objekt som vi kan plassere gjennomsnittene inni

for (i in seq_along(coups)) { # Bruker seq_along for å si at vi vil gjøre operasjonen for hver del av coups.
  out[[i]] <- mean(coups[[i]], na.rm = TRUE) # Går inni kolonnene [[i]] i coups og beregner gjennomsnitt uten å ta hensyn til missing. Plasserer i out
  out <- round(out, 2) # Runder av gjennomsnittet til to desimaler
  tmp <- data.frame(out = out, # Lager en dataframe med to kolonner; gjennomsnittene
                    var = colnames(coups)) # ... og navnene på kolonnene til coups.
}

tmp


#### Repetisjon: Manipulering av data ####

# Hente ut enkelte variabler
## Tar bare med variablene year, democracy, agereg, trade og currency
coup1 <- coups %>%
  select(year, democracy, agereg, trade, currency)


# Hente ut enkelte verdier
## Tar bare med observasjoner for årene 1980 til 1995 og år 2001.
coup2 <- coup1 %>%
  filter(year %in% 1980:1995 | year %in% 2001)


# Endre navn på variabler

head(coup2) # Ser på variablene fra før

## Gir variablene norske navn
coup3 <- coup2 %>%
  rename("Aar" = year,
         "Demokrati" = democracy,
         "Regimealder" = agereg,
         "Handel" = trade,
         "Valuta" = currency)


# Lager en ny variabel
## Lager variabel som måler omkoder handel til proporsjoner
coup4 <- coup3 %>%
  mutate(Handel_prop = Handel/100)


# Lager en variabel som tar verdi 1 hvis året er over 1980, 0 ellers.
coup5 <- coup4 %>%
  mutate(Demokrati_aar = ifelse(Aar >= 1980, 1,
                                0))

remove(coup1, coup2, coup3, coup4, coup5)


#### Tar ut missing og deler i trenings- og testdata ####

coups <- coups %>%
  select(coup, democracy, unrest, coupyears, milper1, milexp3) %>%
  na.omit()

set.seed(28914)

coup_train <- coups %>% 
  sample_frac(.70) # trekker ut 70 % av datasettet (tilfeldige rader)

coup_test <- coups %>% 
  anti_join(coup_train)


#### Regresjon for å predikere demokrati ####

form1 <- coup ~ democracy + unrest + coupyears + milper1 + milexp3

lm1 <- lm(form1,
          data = coup_train)

stargazer::stargazer(lm1, type = "text")

# Dersom vi ikke vil ha med tilfeller der det er over 100 år siden sist kuppforsøk.

form2 <- update(form1, .~. - coupyears + I(coupyears <= 100))

lm2 <- lm(form2,
          data = coup_train)

stargazer::stargazer(lm1, lm2, type = "text") # Rapporterer de to modellene ved siden av hverandre



#### Logistisk modell ####

glm1 <- glm(form1, 
            family = binomial("logit"), 
            data = coup_train)

stargazer::stargazer(lm1, lm2, glm1, type = "text")


#### Random forest ####

forest1 <- randomForest(as.factor(coup) ~ # Konverterer "coup" til factor for å kunne gjøre klassifisering.
                          democracy + unrest + coupyears + milper1 + milexp3, 
                        mtry = 3, # mtry < antall prediktorer gir random forest. Like mange grener som prediktorer (her 5) = en bagging modell.
                        na.action = "na.exclude",
                        data = coup_train)

forest1

?randomForest

# cutoff	
## (Classification only) A vector of length equal to number of classes. The 'winning' class for an observation is the one with the maximum 
## ratio of proportion of votes to cutoff. Default is 1/k where k is the number of classes (i.e., majority vote wins).




# Figur og tabell som viser viktige variabler

varImpPlot(forest1)
importance(forest1)


#### Confusion matrix for å måle hvor godt modellene predikerer (i test-data) ####
pred1 <- predict(lm1)
pred2 <- predict(lm2)
pred3 <- predict(glm1, type = "response")

pred1 <- ifelse(pred1 > 0.1, # Når sjansen for kupp er over 10 % (svært lavt siden kupp er veldig sjeldent)
                1, # Prediker kupp
                0) # Hvis ikke, prediker ikke-kupp

pred2 <- if_else(pred2 > 0.1, 
                     1, 
                     0)

pred3 <- if_else(pred3 > 0.1, 
                 1, 
                 0)

tab1 <- table(prediksjon = pred1, virklighet = coup_train$coup)
tab2 <- table(prediksjon = pred2, virkelighet = coup_train$coup)
tab3 <- table(prediksjon = pred3, virkelighet = coup_train$coup)
tab4 <- forest1$confusion

knitr::kable(list(tab1, tab2, tab3, tab4))


# Estimerer accuracy, dvs. ratioen av antall korrekt predikerte kupp i forhold til hvor mange prediksjoner som ble gjort.

(tab1[1,1] # Radplass 1 og kolonneplass 1 (dvs. der de korrekt predikerte ikke-kuppene er)
  + tab1[2,2])/ # Radplass 2 og kolonneplass 2 (dvs. der de korrekt predikerte kuppene er)
  nrow(coup_train) # Del på antall observasjoner av kupp.
# 0.943 - lineær modell

(tab2[1,1] + tab2[2,2])/nrow(coup_train) # 0.945 - modifisert lineær modell
(tab3[1,1] + tab3[2,2])/nrow(coup_train) # 0.842 - logistisk modell
(tab4[1,1] + tab4[2,2])/nrow(coup_train) # 0.951 - random forest


#### Support vektor maskin ####

# For å bestemme "levels" i data:
coup_train$coup = ordered(coup_train$coup, labels = c(0, 1))
levels(coup_train$coup)
### Modellen tar alltid den første som referansekategori.

coup_test$coup <- as.factor(coup_test$coup)
coup_train$coup <- as.factor(coup_train$coup)

support <- svm(form1, 
               data = coup_train, 
               kernel = "linear",
               cost = 10, # 
               decision.values = TRUE)

summary(support)

tab5 <- table(fitted(support), coup_train$coup)

(tab5[1,1] + tab5[2,2])/nrow(coup_train) # 0.953



#### ROC-kurver ####
# Hvordan gjør modellene det på treningsdata?

roc1 <- roc(response = coup_test$coup, 
            predictor = predict(glm1, coup_test)) # Logistisk modell

roc2 <- roc(response = coup_test$coup,
            predictor = predict(forest1, coup_test, type = "prob")[,2]) # Random forest

support_pred <- attributes(
  predict(support, coup_test, decision.values= TRUE))

roc3 <- roc(response = coup_test$coup,
            predictor = as.vector(support_pred$decision.values)) # Support vektor

rocs <- ggroc(list("logit" = roc1,
                   "forest" = roc2,
                   "support" = roc3))

rocs

# Area under the curve (AUC)
auc(roc1) # 0.781 --- logistisk regresjon er vinneren!
auc(roc2) # 0.746 
auc(roc3) # 0.631
