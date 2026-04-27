library(ChainLadder)

RAA;
plot(RAA); \* triangle cumulé *\

Chainladder <- MackChainLadder(RAA, est.sigma="Mack")

Chainladder$FullTriangle
Chainladder$f

plot(Chainladder)


resultatsboot <- BootChainLadder(RAA, R=999, process.distr="od.pois")
resultatsboot <- BootChainLadder(RAA, R=999, process.distr="gamma")

resultatsboot$ChainLadder.Residuals
resultatsboot$simClaims
resultatsboot$IBNR.Totals

hist(resultatsboot$IBNR.Totals)



GenIns
plot(GenIns)
plot(GenIns, lattice=TRUE)
GNI <- MackChainLadder(GenIns, est.sigma="Mack")
GNI$f
GNI$sigma^2
GNI # compare to table 2 and 3 in Mack’s 1993 paper
plot(GNI)
plot(GNI, lattice=TRUE)



/*    Modèle de type Poison */ 

Modèle de poisson 


dat$Nbe=(dat$Sinistre1>0)+(dat$Sinistre2>0)+(dat$Sinistre3>0)
pois=glm(Nbe~RUC, family=poisson(), data=dat)



/* Données de survie  */ 

library(survival)

help(lung)
help(aml)

KP <- survfit(Surv(time, status) ~ 0, data = aml)   /* estimateur de Kaplan Meier */ 
Plot(KP)

KPx<-survfit(Surv(time, status) ~ x, data = aml)   /* estimateur selon une variable */ 

plot(KPx, lty = 2:3) 

legend(100, .8, c("Maintained", "Nonmaintained"), lty = 2:3) 




test1 <- list(time=c(4,3,1,1,2,2,3,5,6,9,10), 
              status=c(1,1,1,0,1,1,0,1,0,1,1), 
              x=c(0,1,1,1,1,0,0,0,1,0,0), 
              sex=c(0,0,0,0,1,1,1,0,1,1,0)) 

# Estimateur de Kaplan Meier
KP<-survfit(Surv(time,status)~0, data=test1) 

KP<-survfit(Surv(time,status)~x+sex, data=test1) 


# model de cox simple  
cox=coxph(Surv(time, status) ~ x + sex, test1) 

KP=survfit(cox, data=test1)



# Create a simple data set for a time-dependent model 
test2 <- list(start=c(1,2,5,2,1,7,3,4,8,8), 
              stop=c(2,3,6,7,8,9,9,9,14,17), 
              event=c(1,1,1,1,1,1,1,0,0,0), 
              x=c(1,0,0,1,0,1,1,1,0,0)) 
summary(coxph(Surv(start, stop, event) ~ x, test2)) 



























