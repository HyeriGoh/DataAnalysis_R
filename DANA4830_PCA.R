rm(list = ls())

master <- read.csv('PCA-Dataset.csv')


############
##Accuracy##
############
summary(master)
str(master)

data <- master[, -c(1, 7)] #remove Identifier, Year 

table(data$Item_Fat_Content)
data$Item_Fat_Content[data$Item_Fat_Content=='LF']='Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content=='low fat']='Low Fat'
data$Item_Fat_Content[data$Item_Fat_Content=='reg']='Regular'
table(data$Item_Fat_Content)

table(data$Outlet_Size)
data$Outlet_Size[data$Outlet_Size=='']='Unknown' # I don't want to lose 2410 values out of 8523 observations
table(data$Outlet_Size)


###########
##missing##
###########
data$Item_Weight[is.na(data$Item_Weight) == TRUE] = median(data$Item_Weight[is.na(data$Item_Weight) == FALSE])

##################
#####outliers#####
##################
goodrow=data
summary(goodrow) #categorical: 2, 4, 6, 7, 8

goodrow$mahal <- mahalanobis(goodrow[,-c(2, 4, 6, 7, 8)], colMeans(goodrow[,-c(2, 4, 6, 7, 8)]), cov(goodrow[,-c(2, 4, 6, 7, 8)]))
goodrow$p <- pchisq(goodrow$mahal, df=3, lower.tail=FALSE)

cutoff=qchisq(1-.001, ncol(goodrow[,-c(2, 4, 6, 7, 8)])) #check only numerical variables since categorical variables are not omittable
cutoff

table(goodrow$mahal < cutoff) #24 outliers 
nooutlier = subset(goodrow, goodrow$mahal < cutoff)


#####################
##Multicollinearity##
#####################
library(car)
model <- lm(nooutlier$Item_Outlet_Sales ~ nooutlier$Item_Weight + factor(nooutlier$Item_Fat_Content) + nooutlier$Item_Visibility + factor(nooutlier$Item_Type)
            + nooutlier$Item_MRP + factor(nooutlier$Outlet_Size) + factor(nooutlier$Outlet_Location_Type)
            + factor(nooutlier$Outlet_Type), data = nooutlier)
vif(model)  #'outlet_size' has high VIF

model.new <- lm(nooutlier$Item_Outlet_Sales ~ nooutlier$Item_Weight + factor(nooutlier$Item_Fat_Content) + nooutlier$Item_Visibility + factor(nooutlier$Item_Type)
            + nooutlier$Item_MRP + factor(nooutlier$Outlet_Type) + factor(nooutlier$Outlet_Location_Type), data = nooutlier[,2:11])
vif(model.new)


#######
##PCA##
#######
clean_matrix <- data.matrix(nooutlier[, c(0:9)]) # NOT remove 'outlet_size'

##princomp
pca1 <- princomp(clean_matrix, cor=T)
pca1
summary(pca1) #comp4: 62.66% of total variance / comp5:73%
print(pca1$loadings, cutoff = 0.3)
pca1$scores

biplot(pca1, col=c("white","red"), cex=c(0.7,0.8))

#how many PCs are selected - eigenvalues >= 1 ------> comp1, 2, 3/ maybe comp4,5
the.eigen <- eigen(cor(clean_matrix))
the.eigen


#criteria to select PCs
screeplot(pca1)
screeplot(pca1, npcs=8, type="lines")

#Exporting
library(xlsx)
write.xlsx(nooutlier, file = "Final_dataset_PCA.xlsx",sheetName = "Final", append = FALSE)
