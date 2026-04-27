# Si ce n'est fait, sourcer les données assurance.R2
#source("assurance2.R")


#Modèle de type Poison */ 

#Modèle de poisson sur le nombre de sinistres

dat$Nbe=(dat$Sinistre1>0)+(dat$Sinistre2>0)+(dat$Sinistre3>0)
pois=glm(Nbe~RUC+Ahabi+agecat, family=poisson(), data=dat)
summary(pois)

# Modèle tobit, tobit généralisé, double Hurdle et triple hurdle  avec la librarie mhurdle (nouvelle version 2020)


library (mhurdle);

# eventuellement mettre plutot le log(RUC plutot que le RUC)

dat$lruc<-log(dat$RUC)

# tobit simple */ 

rT <- mhurdle(Sinistre1 ~ 0 | pcs+lruc+Ahabi+agecat+nbpers+enfants+Anat+Bauto| 0, data = dat, dist = "n", h2=TRUE, method = "bfgs")
summary(rT)

# attention de bien preciser le h2=TRUE dans la nouvelle version de mhurdle (qui assure qu'on a quand même un modele en 2 etape)


# tobit generalise de Craag: selection + modelisation valeur positive (sans corrélation entre les etapes) avec transformation en log des valeurs positives

rTGl <- mhurdle(Sinistre1 ~ lruc+Ahabi | lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, dist = "l", corr = FALSE)
summary(rTGl)

# tobit generalise : selection + idem modelisation valeur positive (avec corrélation entre les etapes), ici en log (Craag avec correlation)

rTGcorl <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, dist = "l", corr = TRUE)
summary(rTGcorl)


# tobit generalise : selection + modelisation valeur positive (sans corrélation entre les etapes): pas transformation en log des valeurs positives
# dans ce cas de figure, les predictions de la deuxieme etape peuvent être négatives

rTG <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, dist = "n", corr = FALSE)
summary(rTG)

# tobit generalise : selection + idem modelisation valeur positive sans transformation (avec corrélation entre les etapes)
# dans ce cas de figure, les predictions de la deuxieme etape peuvent être négatives

rTGcor <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, dist = "n", corr = TRUE)
summary(rTGcor)


#double hurdle  sans corrélation entre les deux étapes*/ 

rDH <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, h2=TRUE, dist = "n", corr = FALSE)
# bien preciser h2=TRUE pour tenir compte de la selection positive de la deuxieme etape
summary(rDH)

# Double hurdle avec correlation des etapes 
rDHcor <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|0, data = dat, h2=TRUE, dist = "n", corr = TRUE)
# bien preciser h2=TRUE pour tenir compte de la selection positive de la deuxieme etape
summary(rDHcor)


#Triple hurdle avec "infréquences" (triple hurdle) : explication de 0 ici du aux regions (sous déclarations sans certaines région)*/

rTH <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|region, data = dat, h2=TRUE, dist = "n", corr = FALSE)
summary(rTH)

#Triple hurdle avec infréquences (triple hurdle) et correlation */

rTHcor <- mhurdle(Sinistre1 ~ lruc | pcs+lruc+region+Ahabi+agecat+nbpers+enfants+Anat+Bauto|region, data = dat, h2=TRUE, dist = "n", corr = TRUE)
summary(rTHcor)




