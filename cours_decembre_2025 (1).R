getwd()
source("C:\\Users\\patri\\Dropbox\\Cours Nanterre\\cours\\stat-des-assurances\\assurance.R")

dump("dat",file="dat2.R")
dump(ls(),file="sauve.R")

Sinistre.1=dat$Sinistre1

n=length(Sinistre.1);n;
Sinistre.2=dat$Sinistre2

mean(Sinistre.1)
var(Sinistre.1)  # unbiased

hist(Sinistre.1, col='magenta', main="Distribution of Claims 1",nclass=30) 

# Histogram of full data 
hist(Sinistre.2, col='magenta', main="Distribution of Claims 2") 
p=mean(Sinistre.2==0)
hist(Sinistre.2[(Sinistre.2>0)], nclass=35,col='blue', main="Distribution positive claims 2") 


attach(dat)

# A PRIORI Tarification : study of variable Sinistre1 without 0
model1<- lm(Sinistre1~pcs+RUC+region+Ahabi+Atyph+agecat+Acompm+nbpers+Bauto+Nbadulte,data=dat)
summary(model1)
plot(model1)

#Stepwise rejgression 
library(MASS)
stepAIC(model1,direction="backward")


#calibration of the model (choice of variables) : remove variables one by one 
model1<- lm(Sinistre1~RUC+Acompm,data=dat)
summary(model1)

#A POSTERIORI Tarification (POlice1 may have a strong influence and may bias the first regression when omitted
model2<- lm(Sinistre1~RUC+region+agecat+Acompm+nbpers+Bauto+Police1,data=dat)
summary(model2)


#Tarification :
RUCnew=4000
Bautonew= "Pas de vehicule"
Agenew="51-60"
regionnew= "4"
Acompmnew= "Couple avec enfant(s)"
habinew= 8
Ahabinew= "Unit1"
Police.1new= 4.69
nbpersnew=4

xnew=data.frame(RUC=RUCnew,Bauto=Bautonew,agecat=Agenew,nbpers=nbpersnew,region=regionnew,Acompm=Acompmnew,habi=habinew,Ahabi=Ahabinew,Police1=Police.1new)
predictions<- predict(model2,xnew)
predictions # pure premium 
fixedcost=1.5
predictions+fixedcost # Total price pre premium+ fixed cost (salary, operating cost)

