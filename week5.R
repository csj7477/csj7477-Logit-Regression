#Regression

lq <- read.csv(file = "C:/Stat/lifequality_2012.csv", header = TRUE)

#Data preprocessing

#life Quality
table(lq$lifeq, useNA = 'always')
lq$life.quality <- lq$lifeq
lq$life.quality[lq$lifeq ==8 | lq$lifeq == 9] <- NA
lq$life.quality <- 6 - lq$life.quality

#sex
table(lq$sex, useNA = 'always')
lq$female <- 0
lq$female[lq$sex == 2] <- 1

#race
table(lq$race, lq$hisp, useNA = 'always')
lq$race4 <- 4
lq$race4[lq$race==1 & lq$hisp==2] <- 1
lq$race4[lq$race==2 & lq$hisp==2] <- 2
lq$race4[lq$hisp==1] <- 3
lq$race4[lq$hisp==8|lq$hisp==9|lq$race==8|lq$race==9] <- NA
lq$race4 <- factor(lq$race4,labels=c('white','black','hispanic','other races'))
table(lq$race4,useNA='always')

lq$black <- 1
lq$hispanic <- 1
lq$other.race <- 1
lq$black[lq$race4 != 'black'] <- 0
lq$hispanic[lq$race4 != 'hispanic'] <- 0
lq$other.race[lq$race4 != 'other races'] <- 0
lq$black[is.na(lq$race4)] <- lq$hispanic[is.na(lq$race4)] <- lq$other.race[is.na(lq$race4)] <- NA
table(lq$black, lq$race4, useNA='always')
table(lq$hispanic, lq$race4, useNA='always')
table(lq$other.race, lq$race4, useNA='always')

#age
table(lq$age, useNA = 'always')
lq$age.m <- lq$age
lq$age.m[lq$age >= 98] <- NA
lq$age.m.c <- lq$age.m - mean(lq$age.m,na.rm=T)
table(lq$educ, useNA = 'always')

#edu
lq$educ.m <- lq$educ
lq$educ.m[lq$educ >= 8] <- NA
lq$educ.m.c <- lq$educ.m - mean(lq$educ.m,na.rm=T)

#income
table(lq$inc, useNA = 'always')
lq$inc.m <- lq$inc
lq$inc.m[lq$inc >= 98] <- NA
lq$inc.m.c <- lq$inc.m - mean(lq$inc.m,na.rm=T)

table(lq$snsuse, useNA = 'always')
lq$sns.user <- 0
lq$sns.user[lq$snsuse >= 98] <- NA
lq$sns.user[lq$snsuse <= 12] <- 1

#missing variable
lq.listwise <- lq[complete.cases(lq[,c('female','race4','age.m.c','educ.m.c','inc.m.c','sns.user','life.quality')]),]
par(mfrow=c(1,3))
plot(jitter(lq.listwise$age.m,factor=2),jitter(lq.listwise$life.quality,factor=2),
     xlab = 'age, years', ylab = 'Subjectively felt life quality', main = 'Life quality & Age, years')
plot(jitter(lq.listwise$educ.m,factor=2),jitter(lq.listwise$life.quality,factor=2),
     xlab = 'Education level', ylab = 'Subjectively felt life quality', main = 'Life quality & Education level')
plot(jitter(lq.listwise$inc.m,factor=2),jitter(lq.listwise$life.quality,factor=2),
     xlab = 'Income level', ylab = 'Subjectively felt life quality', main = 'Life quality & Income level')

#correlation
round(cor(lq.listwise[,c('life.quality','female','black','hispanic','other.race','age.m.c',
                         'educ.m.c','inc.m.c','sns.user')]),2)

#histogram
hist(lq.listwise$life.quality, breaks=5, ylab='Frequency',
xlab='Subjectively felt life quality',main='Distribution of outcome variable')

#Regression
obj.ols.1 <- lm(life.quality ~ female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
                + educ.m.c + sns.user, data = lq.listwise)
summary(obj.ols.1)

obj.ols.2 <- lm(life.quality ~ (female + black + hispanic + other.race + age.m.c + I(age.m.c^2) 
                 + educ.m.c + inc.m.c)*sns.user, data = lq.listwise)
summary(obj.ols.2)
anova(obj.ols.1, obj.ols.2)

#Logit transformation
x <- -600:600/100
y <- 1/(1+exp(-x))
plot(x, y, type='l', col = 'blue', 
     xlab='x', ylab='F(x)', main = 'Logistic function')

mylogit <- log(y/(1-y))
plot(x, mylogit, type='l', col = 'blue', 
     xlab='x', ylab='Logit', main = 'Transformed by Logit function')

#Logit Regression-1

obj.logit.1 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c,
                   data = lq.listwise, family = binomial(link='logit'))
summary(obj.logit.1)
obj.logit.2 <- glm(sns.user ~ female + black + hispanic + other.race + age.m.c 
                   + educ.m.c + inc.m.c,data = lq.listwise, family = binomial(link='logit'))
summary(obj.logit.2)

#Logit Regression-2
library(rms)
lrm(sns.user ~ female + black + hispanic + other.race + age.m.c, data = lq.listwise)
lrm(sns.user ~ female + black + hispanic + other.race + age.m.c 
    + educ.m.c + inc.m.c,data = lq.listwise)


#LR test-1
anova(obj.logit.1,obj.logit.2)
1 - pchisq(anova(obj.logit.1,obj.logit.2)$Deviance[2],anova(obj.logit.1,obj.logit.2)$Df[2])

#LR test-2
library(lmtest)
lrtest(obj.logit.1, obj.logit.2)

#Summary
logit.table.1 <- round(cbind(summary(obj.logit.1)$coef, exp(summary(obj.logit.1)$coef[,1])),3)
colnames(logit.table.1) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.1
logit.table.2 <- round(cbind(summary(obj.logit.2)$coef, exp(summary(obj.logit.2)$coef[,1])),3)
colnames(logit.table.2) <- c('Coef','S.E.','Z.value','P.value','Odds.Ratio')
logit.table.2


#Prediction Plot
pred.lq.age <- lq.listwise[18:97,]
pred.lq.age$age.m <- 18:97
pred.lq.age$female <- mean(lq.listwise$female)
pred.lq.age$black <- mean(lq.listwise$black)
pred.lq.age$hispanic <- mean(lq.listwise$hispanic)
pred.lq.age$other.race <- mean(lq.listwise$other.race)
pred.lq.age$inc.m.c <- mean(lq.listwise$inc.m.c)
pred.lq.age$educ.m.c <- mean(lq.listwise$educ.m.c)
pred.lq.age$age.m.c <- pred.lq.age$age.m - mean(lq.listwise$age.m)
logit.lq.age <- predict.glm(obj.logit.2, pred.lq.age, type = 'response', se.fit = TRUE)
pred.lq.age$fit <- logit.lq.age$fit
pred.lq.age$lwr <- logit.lq.age$fit - 1.96*logit.lq.age$se.fit
pred.lq.age$upr  <- logit.lq.age$fit + 1.96*logit.lq.age$se.fit

plot(pred.lq.age$age.m, pred.lq.age$fit, ylim = c(0, 1),
     type = 'l', lwd = 3, col = 'blue',
     ylab = "predicted probability to use SNS", xlab = "Age, year",
     main = "The effect of age years on respondents' probability to use SNS ")
points(pred.lq.age$age.m, pred.lq.age$lwr, type = 'l', lty = 2, col = 'blue')
points(pred.lq.age$age.m, pred.lq.age$upr, type = 'l', lty = 2, col = 'blue')


#PseudoR2-1

LogisticPseudoR2 <- function(logistic_object1, logistic_object2, mydigit) {
  obj1.model <- -2*logLik(logistic_object1)
  obj1.null <- summary(logistic_object1)$null.deviance
  obj2.model <- -2*logLik(logistic_object2)
  obj2.null <- summary(logistic_object2)$null.deviance
  obj1.size <- summary(logistic_object1)$df.null + 1
  obj2.size <- summary(logistic_object1)$df.null + 1
  obj1.df <- summary(logistic_object1)$df[2]
  obj2.df <- summary(logistic_object2)$df[2]
  obj1.MCFR2 <- 1 - (obj1.model/obj1.null)
  obj2.MCFR2 <- 1 - (obj2.model/obj2.null)
  obj1.CSR2 <- 1 - exp((obj1.model-obj1.null)/obj1.size)
  obj2.CSR2 <- 1 - exp((obj2.model-obj2.null)/obj2.size)
  obj1.NGKR2 <- obj1.CSR2/(1-exp(-1*obj1.null/obj1.size))
  obj2.NGKR2 <- obj2.CSR2/(1-exp(-1*obj2.null/obj2.size))
  delta.chi <- obj1.model - obj2.model
  delta.df <- obj1.df - obj2.df
  mypval <- 1 - pchisq(delta.chi, delta.df)
  finalreport <- round(rbind(
    c(obj1.MCFR2,obj2.MCFR2,(obj2.MCFR2-obj1.MCFR2)),
    c(obj1.CSR2,obj2.CSR2,(obj2.CSR2-obj1.CSR2)),
    c(obj1.NGKR2,obj2.NGKR2,(obj2.NGKR2-obj1.NGKR2))),mydigit)
  teststat <- paste('Chi2 = ',round(delta.chi,mydigit),", df = ", delta.df, ",", " p = ", round(mypval, mydigit), sep='')
  colnames(finalreport) <- c('Model1','Model2','Model.difference')
  rownames(finalreport) <- c('McFadden R2','Cox&Snell R2','Negelkerke R2')
  myfinalreport <- list(finalreport,teststat)
  myfinalreport
}
LogisticPseudoR2(obj.logit.1, obj.logit.2, 3)

#PseudoR2-2
install.packages("BaylorEdPsych")
library(BaylorEdPsych)
PseudoR2(obj.logit.1)
PseudoR2(obj.logit.2)
