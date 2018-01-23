```{r}
library(readr)
wine <- read_csv("~/Stats101A/wine.csv")
WineTesting <- read_csv("~/Stats101A/WineTesting.csv")
attach(wine)

#check for correlation or our numeric variables
library(corrplot)
corrgraph <- cor(wine[,3:14])
corrplot(corrgraph)

#we see that alcohol has the highest r with quality and should definitely include that in our model. 
#Meanwhile alcohol and density are related pretty highly and may be redundent.
#Total sulfur and free sulfur also have high r and we should only include one if we want to.

#Our first model
m1 <- lm(Quality ~Wine.Color +density + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol)
summary(m1)
plot(m1)

#remove outliers
wine <- wine[-c(270,4135,5523),]
#diagnosic shows transformation is not needed
detach(wine)
attach(wine)

#see changes
m1a <- lm(Quality ~Wine.Color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol)
summary(m1a)
plot(m1a)
#r2 is .1326 r2a

#horrendous r2 
#let us start variable selection using forward selection
X <- lm(Quality~1)
forwardAIC <- step(X,scope=list(lower=~1, upper=~Wine.Color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol),direction="forward")
#result: Quality ~ alcohol + volatile.acidity + sulphates + residual.sugar +  pH + free.sulfur.dioxide + total.sulfur.dioxide

m2 <- lm(Quality ~ alcohol + volatile.acidity + sulphates + residual.sugar + 
           pH + free.sulfur.dioxide + total.sulfur.dioxide)
summary(m2) #.1321
par(mfrow=c(2,2))
plot(m2)

#transformations of our best model
library(car)
summary(powerTransform(cbind(alcohol, volatile.acidity , sulphates , residual.sugar , pH , free.sulfur.dioxide , total.sulfur.dioxide, Quality)~1))

#apply suggest transformations
m3 <- lm(Quality ~ I(alcohol^-2) + log(volatile.acidity) + I(sulphates^-.5) + log(residual.sugar) + 
           I(pH^-1) + I(free.sulfur.dioxide^(1/3)) + I(total.sulfur.dioxide^(3/4)))
summary(m3) #.1326
par(mfrow=c(2,2))
plot(m3)
vif(m3)

#before and afters of transformations
par(mfrow=c(2,2))
plot(Quality ~ alcohol)
plot(Quality ~ I(alcohol^-2))
plot(Quality ~ volatile.acidity)
plot(Quality ~ log(volatile.acidity))

par(mfrow=c(2,2))
plot(Quality ~ sulphates)
plot(Quality ~  I(sulphates^-.5) )
plot(Quality ~ residual.sugar)
plot(Quality ~ log(residual.sugar))


par(mfrow=c(2,2))
plot(Quality ~ pH)
plot(Quality ~ I(pH^-1))
plot(Quality ~ free.sulfur.dioxide)
plot(Quality ~ I(free.sulfur.dioxide^(1/3)))

plot(Quality ~ total.sulfur.dioxide)
plot(Quality ~ I(total.sulfur.dioxide^(3/4)))

```

```{r}
#now lets start checking interactions between wine color and our current predictors
#Quality ~ I(alcohol^-2) + log(volatile.acidity) + I(sulphates^-.5) + log(residual.sugar) + I(pH^-1) + I(free.sulfur.dioxide^(1/3)) + I(total.sulfur.dioxide^(3/4))

summary(lm(Quality ~ Wine.Color)) #significant
boxplot(Quality~Wine.Color)

int1a <- lm(Quality ~ Wine.Color*I(alcohol^-2))
int1b <-lm(Quality ~ I(alcohol^-2) + Wine.Color)
anova(int1b,int1a) #signif .002
summary(int1a)

int1a <- lm(Quality ~ log(volatile.acidity) + Wine.Color + Wine.Color:log(volatile.acidity))
int1b <-lm(Quality ~ log(volatile.acidity) + Wine.Color)
anova(int1b,int1a) #signif .01
summary(int1a)

int1a <- lm(Quality ~ log(residual.sugar) + Wine.Color + Wine.Color:log(residual.sugar))
int1b <-lm(Quality ~ log(residual.sugar) + Wine.Color)
anova(int1b,int1a) #insig

int1a <- lm(Quality ~ I(free.sulfur.dioxide^(1/3)) + Wine.Color + Wine.Color:I(free.sulfur.dioxide^(1/3)))
int1b <-lm(Quality ~ I(free.sulfur.dioxide^(1/3)) + Wine.Color)
anova(int1b,int1a) #insignif


int1a <- lm(Quality ~ I(total.sulfur.dioxide^(3/4)) + Wine.Color + Wine.Color:I(total.sulfur.dioxide^(3/4)))
int1b <-lm(Quality ~ I(total.sulfur.dioxide^(3/4)) + Wine.Color)
anova(int1b,int1a) #insignif


int1a <- lm(Quality ~ I(pH^-1) + Wine.Color + Wine.Color:I(pH^-1))
int1b <-lm(Quality ~ I(pH^-1) + Wine.Color)
anova(int1b,int1a) #signif  3e-7
summary(int1a)

int1a <- lm(Quality ~ I(sulphates^-.5) + Wine.Color + Wine.Color:I(sulphates^-.5))
int1b <-lm(Quality ~ I(sulphates^-.5) + Wine.Color)
anova(int1b,int1a) #signif 2e-10
summary(int1a)

#verifying interaction significance using plots
par(mfrow=c(2,2))
plot(c(5,15), c(0,12), type='n', ylab='Quality', xlab='Alcohol')
lines(alcohol[Wine.Color=="W"], Quality[Wine.Color=="W"], type='p', col='blue', pch = 1, cex=1.2)
lines(alcohol[Wine.Color=="R"], Quality[Wine.Color=="R"], type='p', col='red', pch = 20, cex=1.2)
legend('topleft', c('White Wine 1','Red Wine 2'), pch=c(1,20,8), col=c('blue','red'), cex=0.8)
reg1<-lm(Quality[Wine.Color=="W"]~alcohol[Wine.Color=="W"])
abline(reg1,col="blue")
reg2<-lm(Quality[Wine.Color=="R"]~alcohol[Wine.Color=="R"])
abline(reg2,col="red")

plot(c(0,2), c(0,12), type='n', ylab='Quality', xlab='volatile.acidity')
lines(volatile.acidity[Wine.Color=="W"], Quality[Wine.Color=="W"], type='p', col='blue', pch = 1, cex=1.2)
lines(volatile.acidity[Wine.Color=="R"], Quality[Wine.Color=="R"], type='p', col='red', pch = 20, cex=1.2)
legend('topleft', c('White Wine 1','Red Wine 2'), pch=c(1,20,8), col=c('blue','red'), cex=0.8)
reg1<-lm(Quality[Wine.Color=="W"]~volatile.acidity[Wine.Color=="W"])
abline(reg1,col="blue")
reg2<-lm(Quality[Wine.Color=="R"]~volatile.acidity[Wine.Color=="R"])
abline(reg2,col="red")

plot(c(2,4), c(0,12), type='n', ylab='Quality', xlab='pH')
lines(pH[Wine.Color=="W"], Quality[Wine.Color=="W"], type='p', col='blue', pch = 1, cex=1.2)
lines(pH[Wine.Color=="R"], Quality[Wine.Color=="R"], type='p', col='red', pch = 20, cex=1.2)
legend('topleft', c('White Wine 1','Red Wine 2'), pch=c(1,20,8), col=c('blue','red'), cex=0.8)
reg1<-lm(Quality[Wine.Color=="W"]~pH[Wine.Color=="W"])
abline(reg1,col="blue")
reg2<-lm(Quality[Wine.Color=="R"]~pH[Wine.Color=="R"])
abline(reg2,col="red")

plot(c(0,2), c(0,12), type='n', ylab='Quality', xlab='sulphates')
lines(sulphates[Wine.Color=="W"], Quality[Wine.Color=="W"], type='p', col='blue', pch = 1, cex=1.2)
lines(sulphates[Wine.Color=="R"], Quality[Wine.Color=="R"], type='p', col='red', pch = 20, cex=1.2)
legend('topleft', c('White Wine 1','Red Wine 2'), pch=c(1,20,8), col=c('blue','red'), cex=0.8)
reg1<-lm(Quality[Wine.Color=="W"]~sulphates[Wine.Color=="W"])
abline(reg1,col="blue")
reg2<-lm(Quality[Wine.Color=="R"]~sulphates[Wine.Color=="R"])
abline(reg2,col="red")


#now we add interactions and remove 1 by 1 based on anova 
m4a <- lm(Quality ~ I(alcohol^-2) + log(volatile.acidity) + I(sulphates^-.5) + log(residual.sugar) + 
            I(pH^-1) + I(free.sulfur.dioxide^(1/3)) + I(total.sulfur.dioxide^(3/4))+ Wine.Color:I(sulphates^-.5) + Wine.Color:I(pH^-1) + Wine.Color:log(volatile.acidity) + Wine.Color:I(alcohol^-2) + Wine.Color)
anova(m4a)

#remove color:alc, contribution is 0 to SSR
m4b <-lm(Quality ~ I(alcohol^-2) + log(volatile.acidity) + I(sulphates^-.5) + log(residual.sugar) + 
           I(pH^-1) + I(free.sulfur.dioxide^(1/3)) + I(total.sulfur.dioxide^(3/4))+ Wine.Color:I(sulphates^-.5) + Wine.Color:I(pH^-1) + Wine.Color:log(volatile.acidity) + Wine.Color)
anova(m4b)
summary(m4b)
vif(m4b)

#remove wine color
m4c <- lm(Quality ~ I(alcohol^-2) + log(volatile.acidity) + I(sulphates^-.5) + log(residual.sugar) + 
            I(pH^-1) + I(free.sulfur.dioxide^(1/3)) + I(total.sulfur.dioxide^(3/4))+ Wine.Color:I(sulphates^-.5) + Wine.Color:I(pH^-1)  + Wine.Color:log(volatile.acidity))
anova(m4c)
summary(m4c)
vif(m4c)

plot(m4c)
wine <- wine[-c(630,3717,2029),]
detach(wine)
attach(wine)

#we should compare our current model if all transformations were simply logs to see if we are overfitting
m4d <- lm(Quality ~ log(alcohol) + log(volatile.acidity) + log(sulphates) + log(residual.sugar) + 
            log(pH) + log(free.sulfur.dioxide) + log(total.sulfur.dioxide)+ Wine.Color:log(sulphates) + Wine.Color:log(pH)  + Wine.Color:log(volatile.acidity))
anova(m4d)
summary(m4d)

#we actually see a smaller SSE and suspect our power transforms were overfitting. get rid of interaction between color and vol acidity
m5a <- lm(Quality ~ log(alcohol) + log(volatile.acidity) + log(sulphates) + log(residual.sugar) + 
            log(pH) + log(free.sulfur.dioxide) + log(total.sulfur.dioxide)+ Wine.Color:log(sulphates) + Wine.Color:log(pH))
anova(m5a)

#remove interaction between ph and color
m5b <- lm(Quality ~ log(alcohol) + log(volatile.acidity) + log(sulphates) + log(residual.sugar) + 
            log(pH) + log(free.sulfur.dioxide) + log(total.sulfur.dioxide)+ Wine.Color:log(sulphates))
anova(m5b) #significant contributions to SS
summary(m5b)

#m5b is our final model. Diagnositc plots are below.
plot(m5b)
vif(m5b)
