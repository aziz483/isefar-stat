###### Exemple 1 : modelisation du montant du loyer médian en fonction du secteur et de la valeur médiane de l'immobilier (villes aux US) #######


# library variables instrumentales et test d'endogénéité

library(AER); #Applied Econometric with R


# library d'importaion de données externes

library(foreign); hsng2 <- read.dta("http://www.stata-press.com/data/r11/hsng2.dta");



# Regression sans tenir compte du problème d'endogénéité

flm <- lm(rent~hsngval+pcturban,data=hsng2);
summary(flm); 
fiv <- ivreg(rent~hsngval+pcturban|pcturban+faminc+reg2+reg3+reg4,data = hsng2);

summary(fiv);

# estimateurs de la variances (robuste à l'hypothèse d'hétéroscédasticité) et tests de significativité; 

library(sandwich);
library(lmtest);
coeftest(fiv, vcov=sandwich);

#Regression instrumentales pour tester les instruments 

step1 <- lm(hsngval~pcturban+faminc+reg2+reg3+reg4, data = hsng2);
summary(step1);

library(MASS)
stepAIC(step1)

# test de Wald pour tester quels sont les variables les plus influentes (cas d'une variable exogène) 

waldtest(step1, .~.-faminc-reg2-reg3-reg4, vcov=sandwich)

# test d'endogenité (test sur le coeff des residus de l'equation instrumentale, inclus dans le modèle) 

hsng2$resistep1=step1$resi;
step2<-lm(rent~hsngval+pcturban+resistep1,data=hsng2);

summary(step2)



#####################################################################################################################################
#####################################################################################################################################
###### Example 2 : determination de la consommation de cigarettes en fonctions des prix et du revenu (courbe de Engle)
###### Calculs des elasticités prix et revenus : endogénéité des prix (determinés eux même par la conso selon le principe offre/demande) 


## Données : création de variables additionnelles

data("CigarettesSW", package = "AER")  #va chercher les données CigarettesSW dans le package AER
cig=CigarettesSW


cig$rprice <- with(cig, price/cpi)
cig$rincome <- with(cig, income/population/cpi)
cig$tdiff <- with(cig, (taxs - tax)/cpi)

## modele de regression simple 

flm<-lm(log(packs) ~ log(rprice) + log(rincome),data=cig)

summary(flm)

## Estimation par double moindre carré sur l'ensemble des données
 
fiv <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
  data = cig);
summary(fiv)

coeftest(fiv, vcov=sandwich);

summary(fiv, vcov = sandwich, df = Inf, diagnostics = TRUE)  #similaire à la commande precedente


## Regression instrumentale 

 
step1=lm(log(rprice)~log(rincome) + tdiff + I(tax/cpi), data = cig) 
summary(step1)


# test de Wald pour tester quels sont les variables les plus influentes (cas d'une variable endogène), ici on regarde si le revenu est une instrument faible 

waldtest(step1, .~.-log(rincome), vcov=sandwich)


# test d'endogenité (test sur le coeff des residus de l'equation instrumentale, inclus dans le modèle) 
cig$resistep1=step1$resi;
step2<-lm(log(packs) ~ log(rprice) + log(rincome)+resistep1,data=cig);

summary(step2)


# modele avec toutes les variables 
step2<-lm(log(packs) ~ log(rprice) + log(rincome)+tdiff + I(tax/cpi),data=cig);

summary(step2)


######################################################################
#lm uniquement sur un sous-echantillon 1995

flm95<-lm(log(packs) ~ log(rprice) + log(rincome),data=cig,subset = year == "1995")
summary(flm95)


## Estimation par double moindre carré sur 1995
 
fiv95 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
  data = cig, subset = year == "1995");
summary(fiv95)
summary(fiv95, vcov = sandwich, df = Inf, diagnostics = TRUE)

## Regression instrumentale (sur 1995)

 
step1=lm(log(rprice)~log(rincome) + tdiff + I(tax/cpi), data = cig,subset = year == "1995") 
summary(step1)


# test d'endogenité (test sur le coeff des residus de l'equation instrumentale, inclus dans le modèle) pour 1995 seulement

cig$resistep1=step1$resi;
step2<-lm(log(packs) ~ log(rprice) + log(rincome)+resistep1,data=cig,subset = year == "1995");

summary(step2)







