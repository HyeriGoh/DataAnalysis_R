data = read.csv('Attitude-A4.csv')

#====================================================Accuracy====================================================#
summary(data)
str(data) #Q5, Q10, Q20, Q22 are not integer

apply(data, 2, table) #Q5(1.915510719), Q10(2.170670038), Q20(1.174022699), Q22(-0.477931904) includes decimal

mean(data$Q5[data$Q5<=1 | data$Q5>=2])
mean(data$Q10[data$Q10<=2 | data$Q10>=3])
mean(data$Q20[data$Q20<=1 | data$Q20>=2])
mean(data$Q22[data$Q22<=-1 | data$Q22>=0])
#All not integer number was exactly same to the mean of observations in each columns except a non-integer cell

#====================================================Missing Value====================================================#
#no missing data found

#====================================================Outliers====================================================#
mahal = mahalanobis(data, colMeans(data), cov(data))

cutoff=qchisq(1-.001, ncol(data)) #df = 25-1 = 24
cutoff

table(mahal < cutoff)
nooutlier = subset(data, mahal < cutoff)

#====================================================Assupmtions====================================================#
library(psych)
library(GPArotation)
library(car)

##assumption set up
random = rchisq(nrow(nooutlier), 10)
random
fake_lm = lm(random~., data = nooutlier)
summary(fake_lm)
standardized = rstudent(fake_lm) #Returns the Studentized residuals
fitted = scale(fake_lm$fitted.values)

##normality
hist(standardized) #histogram of studentized residual
sapply(nooutlier,shapiro.test)

##linearity:qqplot of studentized residual. Studentized residuals meet normality.
qqnorm(standardized) 
abline(0,1)

###Homoscedasticity/Heteroscedasticity(equal variance assumption) - no certain pattern or trend shown. Valid
plot(fitted,standardized) #studentized residual plot
abline(0,0)
abline(v = 0)

##additivity
library(corrplot)
corrplot(cor(nooutlier), method='circle')

correl = cor(nooutlier)
correl[correl < 0.5 & correl > -0.5]=NA
correl


#correlation adequacy Bartlett's test
#Bartlett's test assume that variances are equal across groups/samples
#H0: the samples have equal variance
#Ha: at least one sample has a significantly different variance.
correlations = cor(nooutlier)
cortest.bartlett(correlations, n = nrow(nooutlier)) #the result shown that p value <0.05. So, the FA might be useful with you data

##sampling adequacy KMO test
KMO(correlations)

##Overall MSA =  0.89 meaning (the closer to 1, the better), There are a significant number of factors in the dataset


#====================================================number of factors====================================================#
no.factors = fa.parallel(nooutlier, fm="ml", fa="fa")
#suggest 6 factors
#but scree plot suggest 4.(after 4, slope drops very slowly)
no.factors$fa.values #eigenvalues
sum(no.factors$fa.values > 1.0) ##old kaiser criterion
sum(no.factors$fa.values > .7) ##new kaiser criterion

##simple structure with a 3-factor model
EFA.model <-fa(nooutlier, nfactors=3, rotate = "none", fm = "ml")
fa.diagram(EFA.model)

#====================================================simple structure====================================================#
#simple structure with a 4 factor model
EFA.model.one <- fa(nooutlier, nfactors=4, rotate = "oblimin", fm = "ml")
fa.diagram(EFA.model.one)

#simple structure with a 4 factor model
EFA.model.two <- fa(nooutlier, nfactors=4, rotate = "varimax", fm = "ml")
fa.diagram(EFA.model.two)

#ML1 physical health
#ML4 physical appearance
#ML2 winning Competition, oneself prior to others
#ML3 earning new skills, self-practice, focusing on oneself


#look at loadings
print(EFA.model.one$loading, cutoff = 0.4) #lower than 0.4 = Q9, Q11, Q12, Q17
print(EFA.model.two$loading, cutoff = 0.4) #lower than 0.4 = Q19


#====================================================Adequacy====================================================#
#get CFI
finalmodel = EFA.model.two
1 - ((finalmodel$STATISTIC-finalmodel$dof)/
       (finalmodel$null.chisq-finalmodel$null.dof))

#reliability
factor1 = c(14, 21, 23)
factor2 = c(6, 7, 8, 9, 10, 15, 17, 24, 25)
factor3 = c(1, 2, 4, 5, 11, 12, 18, 19)
factor4 = c(3, 13, 16, 20, 22)
alpha(nooutlier[, factor1]) #cronbach's alpha
alpha(nooutlier[, factor2]) #cronbach's alpha
alpha(nooutlier[, factor3]) #cronbach's alpha
alpha(nooutlier[, factor4]) #cronbach's alpha

fa.diagram(finalmodel)
summary(finalmodel$scores)

#====================================================Exporting====================================================#
library(xlsx)
write.xlsx(nooutlier, file = "Final_dataset_FA.xlsx",sheetName = "Final_Clean", append = FALSE)
