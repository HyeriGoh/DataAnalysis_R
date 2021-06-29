data = read.csv('Data-screening-1.csv')

########################
##### Accuracy ########
########################
table(data$Gender)
data$Gender[data$Gender > 2] = NA
data$Gender[data$Gender == ' '] = NA
data$Gender[data$Gender == 'LGBT'] = NA
table(data$Gender)

table(data$Education)

table(data$Occupation)

table(data$Income)
data$Income[data$Income > 4] = NA
table(data$Income)

table(data$Age)
data$Age[data$Age == '0'] = NA
data$Age[data$Age == '1'] = NA
data$Age[data$Age == ' '] = NA
data$Age[data$Age == '16 years-old'] = NA
data$Age[data$Age == '1x'] = NA
data$Age[data$Age == 'Test'] = NA
data$Age[data$Age == 'X'] = NA
data$Age[data$Age == '211'] = NA
data$Age[data$Age == '242'] = NA
data$Age[data$Age == '1970'] = NA
data$Age[data$Age == '1985'] = NA
table(data$Age)


names(data)
summary(data[ , -c(1:21)])


table(data$Q6K)
data$Q6K[data$Q6K>2] = NA
table(data$Q6K)


summary(data[ , c(44:48)])
data$Q11C1[data$Q11C1 > 3] = NA
data$Q11C2[data$Q11C2 > 3] = NA
data$Q11C3[data$Q11C3 > 3] = NA
data$Q11C4[data$Q11C4 > 3] = NA
data$Q11C5[data$Q11C5 > 3] = NA
summary(data[ , c(44:48)])

#ranking
summary(data[ , 49:55]) #data$Q12C1 ~ Q12C6, Q12C8
data[, 49:55][data[, 49:55] > 6] = NA
summary(data[ , 49:55]) #exclude Q12C8

summary(data[ , 83:88]) #data$Q22C1 ~ Q22C4, Q22c5, Q22c6


#multiple choices(selected answer will be coded as 1, esle as 0)
summary(data[, c(26:32)])#data$Q5K1 ~ Q5K7
data[, c(26:32)][is.na(data[, c(26:32)]) == TRUE] = 0
summary(data[, c(26:32)])

summary(data[, c(63:68)])#data$Q15C1 ~ Q15C6
data[, c(63:68)][is.na(data[, c(63:68)]) == TRUE] = 0
summary(data[, c(63:68)])

summary(data[, c(69:73)]) #data$Q16P1 ~ Q16P5
data[, c(69:73)][is.na(data[, c(69:73)]) == TRUE] = 0
summary(data[, c(69:73)])

#three choices(1,2,3 will be coded as 1, 4,5,6 as 0, elas as NA)
summary(data[ , 56:61]) #data$Q13C1 ~ Q13C6
data[, 56:61][data[, 56:61] > 6] = NA
data[, 56:61][data[, 56:61] <= 3] = 1
data[, 56:61][data[, 56:61] >3 & data[, 56:61] <=6] = 0
summary(data[ , 56:61])


summary(data[,-c(1:21)])




###########################
###### Missing Value#######
###########################
percentmiss = function(x) {sum(is.na(x) / length(x)) * 100}

##over 5% missing per column
apply(data[, -c(1:16)], 2, percentmiss) # >5% : Income, Q12C5, Q12C8, Q13C1, Q13C2, Q13C3, Q13C4, Q13C5, Q13C6

##below 5% missing per row
data_good_row = apply(data[, -c(1:16)], 1, percentmiss)
table(data_good_row)
goodrow=subset(data, data_good_row<=5) # rows that are exceeding 5% of missing value will be deleted 
View(goodrow)
summary(goodrow)

##rows <= 5% and cols <= 5%
goodrowgoodcol=goodrow[, -c(21, 53, 55, 56:61)] #exclude Income, Q12C5, Q12C8, Q13C1, Q13C2, Q13C3, Q13C4, Q13C5, Q13C6
goodrowbadcol=goodrow[ , c(21, 53, 55, 56:61)]


##################
#####outliers#####
##################
names(goodrowgoodcol)
nomiss=goodrowgoodcol[, -c(1:16)]
nomiss2 = na.omit(nomiss)
summary(nomiss2)

nomiss2$Gender <- as.numeric(nomiss2$Gender)
nomiss2$Age <- as.numeric(nomiss2$Age)
summary(nomiss2)

mahalanobis(nomiss2, colMeans(nomiss2), cov(nomiss2), tol=4.38442e-19)
nomiss2$mahal <- mahalanobis(nomiss2, colMeans(nomiss2), cov(nomiss2), tol=4.38442e-19)


nomiss2$p <- pchisq(nomiss2$mahal, df=62, lower.tail=FALSE)

cutoff=qchisq(1-.001, ncol(nomiss2))
cutoff

table(nomiss2$mahal < cutoff)
nooutlier = subset(nomiss2, nomiss2$mahal < cutoff)

hist(nomiss2$mahal, xlab='mahalanobis distance', main='histogram of mahalanobis distance', breaks=seq(0, 240, 20), xlim=c(0, 260))
hist(nooutlier$mahal, xlab='mahalanobis distance', main='histogram of mahalanobis distance without outliers', breaks=seq(0, 240, 20), xlim=c(0, 260))

###########################
#####Multicollinearity#####
###########################
str(nooutlier)
cortable = cor(nooutlier)
cortable[cortable < 0.5 & cortable > -0.5] = NA
cortable

nooutliermatrix = data.matrix(nooutlier, rownames.force = NA)
heatmap(nooutliermatrix)
options(max.print=5000)

##################################
#####Data Export(csv, xlsx)#######
##################################
write.csv(nooutlier,"Dataset_Cleaned.csv", row.names = TRUE)
library(xlsx)
write.xlsx(nooutlier, file = "Dataset_Cleaned.xlsx",sheetName = "after_screen", append = FALSE)

rm(list = ls()) #cleaning environment
