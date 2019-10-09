rm(list=ls())
library(readxl)
library(corrplot)
library(car)
install.packages("corrplot")
population=read_excel("6304 Regression Project Data.xlsx",sheet="midwest")
colnames(population) = tolower(make.names(colnames(population)))

population$popcollege=population$poptotal*(population$percollege/100)
population$popprof=population$poptotal*(population$perprof/100)

population$poptotal2=population$poptotal^2;
population$popwhite2=population$popwhite^2;

population$percollege2=population$percollege^2
population$perprofln=log(population$perprof)



population$perprof2=population$perprof^2


attach(population)
#This clearly follows a curve pattern hence we will be moving squaring the term for a better fit
plot(population$percollege,population$perelderlypoverty,pch=19)

#this also follows a curve pattern from the looks of it
plot(population$popdensity,population$perelderlypoverty,xlim=c(0,1000),pch=19)
population$ratioca=population$popchild/popadult;

population$popperchildpoverty=(population$popchild*(population$perchildpoverty/100))
population$popperelderpoverty=(population$popadult*(population$perelderlypoverty/100))
population$popdensity2=population$popdensity^2;

population.metropolitan=subset(population, inmetro==1,)
population.rural=subset(population,inmetro==0,)
attach(population.rural)

set.seed(01366282)
rural.60=population.rural[sample(1:nrow(population.rural),60,replace=FALSE),]
attach(rural.60)
metro.30=population.metropolitan[sample(1:nrow(population.metropolitan),30,replace=FALSE),]
attach(metro.30)

some.of.rural.60=subset(rural.60,select=c("area","poptotal","popdensity","popwhite","popblack","popasian","popadult",
                                          "popchild","percollege","perprof","perchildpoverty","perelderlypoverty",
                                          "popcollege","popprof","ratioca","popperchildpoverty","poptotal2","popwhite2","percollege2","popdensity2","popperelderpoverty"))
plot(some.of.rural.60)
cor(some.of.rural.60)

summary(rural.60)
attach(some.of.rural.60)

plot(population$popadult,population$perelderlypoverty,pch=19)
plot(percollege,perelderlypoverty,pch=19)
#There seems to be good relation between the perchild poverty and pereldery poverty which can be witnessed
# by the corelation and also by the good fit i am getting which i am choosing it as an independant
#variable

step(lm(perelderlypoverty~area+poptotal+popdensity+popwhite+popblack+popasian+popadult+popchild+percollege+perprof+perchildpoverty+perelderlypoverty+popcollege+percollege2+popprof+ratioca+popperchildpoverty+popperelderpoverty),direction="both")

rural.60$popblack2=log(rural.60$popblack)

rural.60$popadult2=rural.60$popadult^2;


attach(rural.60)
rural.out=lm(perelderlypoverty ~ popdensity + popblack + popblack2 + percollege + perchildpoverty  + ratioca ,data = rural.60)
summary(rural.out)
vif(rural.out)

rural.out=lm( perelderlypoverty ~ percollege + poptotal + perchildpoverty + 
               ratioca, data = rural.60)




summary(rural.out)
vif(rural.out)
plot(rural.out$fitted.values,perelderlypoverty,pch=19)



#Whenever there is lot of emphasis on the percollege , there is less pereldeerlypoverty , this is 
#one we need to consider.
#Line assumptions


#Actual vs fitted
plot(rural.out$fitted.values,perelderlypoverty,main="Actual vs fitted")

#Equality of variances
plot(perelderlypoverty,rural.out$residuals,pch=19,main="Residual Plot Base Model")
abline(0,0,lwd=3,col="red")




#Normality
qqnorm(rural.out$residuals,pch=19)
qqline(rural.out$residuals,col="red",lwd=3)




#co-linearity
some.of.rural=subset(rural.60,select=c( "perelderlypoverty","popdensity","popblack","popblack2","percollege","perchildpoverty","ratioca"))
plot(some.of.rural)
xx=cor(some.of.rural)
corrplot(xx,method="ellipse")
vif(rural.out)



#Lev
lev=hat(model.matrix(rural.out))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
rural.60[lev>(3*mean(lev)),2]



plot(metro.out$fitted.values,perelderlypoverty,main="Actual vs fitted")

plot(perelderlypoverty,metro.out$residuals,pch=19,main="Residual Plot Base Model")
abline(0,0,lwd=3,col="red")





metro.out=lm(perelderlypoverty ~ popdensity + popblack + popblack2 + percollege + perchildpoverty  + ratioca 
             + popperelderpoverty,data = metro.30)
summary(metro.out)



plot(metro.out$fitted.values,perelderlypoverty,main="Actual vs fitted")

plot(metro.30$perelderlypoverty,metro.out$residuals,pch=19,main="Residual Plot Base Model")
abline(0,0,lwd=3,col="red")


qqnorm(metro.out$residuals,pch=19)
qqline(metro.out$residuals,col="red",lwd=3)


