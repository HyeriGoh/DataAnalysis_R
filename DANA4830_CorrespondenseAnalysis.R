rm(list=ls())

data <- read.csv('breakfast.csv')
rownames(data) <- data$X
meal <- data[,-1]

library(ggplot2)
library("FactoMineR")
library("factoextra")

dt <- as.table(as.matrix(meal))
res.ca <- CA(meal, graph = FALSE)
print(res.ca)

#Step 1 is to evaluate whether there is a significant dependency between the rows and columns
chisq <- chisq.test(meal)
chisq
#In our example, the association is highly significant (chi-square: 1944.456, p = 0).

eig.val <- get_eigenvalue(res.ca) #we have 7 dimention, since we have 4 columns. 8-1=7
eig.val
#Eigenvalues correspond to the amount of information retained by each axis.
#The cumulative percentage explained is obtained by adding the successive proportions of variation explained to obtain the running total. 
#For instance, 52.5% plus 21.1% equals 73.6% -> First 2 dimensions
#Eigenvalues can be used to determine the number of axes to retain
#the first two axes explain 73.6% of the variation. This is an acceptably large percentage
#View through The scree plot can be produced using the function


#----------------------------------------------------------------------------------biplot - symmetric plot
1/(ncol(meal)-1) #the average axis in terms of 14 rows = 14.29%
1/(nrow(meal)-1) #the average axis in terms of 8 columns = 7.69%

#To be clearer - display with dashed line of 14.29%

fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(res.ca) +
  geom_hline(yintercept=14.29, linetype=2, color="red")
fviz_ca_biplot(res.ca, repel = TRUE)
#The dimension 3 explains only 11.9% of the total inertia which is below the average eigeinvalue(14.29%) and too little to be kept for further analysis.
#Dimensions 1 and 2 explain approximately 52.5% and 21.1% of the total inertia respectively.


#Second conclusion is the biplot - symmetric plot show the global pattern
# repel= TRUE to avoid text overlapping (slow if many point)
#The distance between any row points or column points gives a measure of their similarity

#Note that with symmetric plot: The distance between any row and column items is not meaningful! You can only make a general statements about the observed pattern.
#In order to interpret the distance between column and row points, the column profiles must be presented in row space or vice-versa.
#This type of map is called asymmetric biplot and is discussed at the end of this article
#values on contingency table ---> standardize the values -----> decomposing vectors -----> drawing biplot



#------------------------------------------------------------------------------------rows and columns
#Third conclusion is look deeper into rows and columns
row <- get_ca_row(res.ca)
row
#row$coord: coordinates of each row point in each dimension (1, 2 and 3). Used to create the scatter plot.
#row$cos2: quality of representation of rows
#var$contrib: contribution of rows (in %) to the definition of the dimensions.

#example: 
head(row$coord)
row$coord
fviz_ca_row(res.ca, repel = TRUE)# relationship between row points

#example: Quality of representation of rows
head(row$cos2, 4)
row$cos2
#the quality of representation of the rows on the factor map is called the squared cosine (cos2) or the squared correlations
#The result of the analysis shows that, the contingency table has been successfully represented in low dimension space using correspondence analysis.
#The two dimensions 1 and 2 are sufficient to retain 73.6% of the total inertia (variation) contained in the data.

#If a row item is well represented by two dimensions, the sum of the cos2 is closed to one. For some of the row items, more than 2 dimensions are required to perfectly represent the data
#Visualize the cos2 of row points on all the dimensions using the corrplot package
library("corrplot")
corrplot(row$cos2, is.corr=FALSE)

#Create a bar plot of rows cos2
# Cos2 of rows on Dim.1 and Dim.2
fviz_cos2(res.ca, choice = "row", axes = 1:2)
#all row points except Official are well represented by the first two dimensions
#This implies that the position of the point corresponding the item Official on the scatter plot should be interpreted with some caution. A higher dimensional solution is probably necessary for the item Official.

#example
#Contributions of rows to the dimensions
head(row$contrib)
#The row variables with the larger value, contribute the most to the definition of the dimensions
#Rows that contribute the most to Dim.1 and Dim.2 are the most important in explaining the variability in the data set.
#Rows that do not contribute much to any dimension or that contribute to the last dimensions are less important.
library("corrplot")
row$contrib
corrplot(row$contrib, is.corr=FALSE)
# Contributions of rows to dimension 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
# Contributions of rows to dimension 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)

# Total contribution to dimension 1 and 2
fviz_contrib(res.ca, choice = "row", axes = 1:2, top = 10)
#The red dashed line on the graph above indicates the expected average value
#the row items Repairs, Laundry, Main_meal and Driving are the most important in the definition of the first dimension
#the row items Holidays and Repairs contribute the most to the dimension 2
#------------------------------------------------
col <- get_ca_col(res.ca)
col
# Coordinates of column points
head(col$coord)
# Quality of representation
head(col$cos2)
col$cos2
corrplot(col$cos2, is.corr=FALSE)
# Contributions
head(col$contrib)
col$contrib
fviz_ca_col(res.ca)
fviz_cos2(res.ca, choice = "col", axes = 1:2)
#only the column item Alternating is not very well displayed on the first two dimensions. The position of this item must be interpreted with caution in the space formed by dimensions 1 and 2.

fviz_contrib(res.ca, choice = "col", axes = 1:2)

##---------------------------------------------------------------------------------Asymmetric biplot
#rows (or columns) points are plotted from the standard co-ordinates (S)
#the profiles of the columns (or the rows) are plotted from the principle coordinates (P)
#P = sqrt(eigenvalue) X S where
#P: the principal coordinate of a row (or a column) on the axis
#eigenvalue: the eigenvalue of the axis

#plot a standard asymetric biplot
fviz_ca_biplot(res.ca,
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)
#the argument arrows, which is a vector of two logicals specifying if the plot should contain points (FALSE, default) or arrows (TRUE).
#The first value sets the rows and the second value sets the columns.
#If the angle between two arrows is acute, then their is a strong association between the corresponding row and column.
#To interpret the distance between rows and and a column you should perpendicularly project row points on the column arrow.
#The closer an arrow is (in terms of angular distance) to an axis the greater is the contribution of the row category on that axis relative to the other axis. If the arrow is halfway between the two, its row category contributes to the two axes to the same extent.
#for example, angle between insurance and jointly is similar to angle between husband and insurance. Insurance is associated to husband as much as jointly
#shopping and dishes are very close


#------------------------------------------------------------------------Dimension description
#identify row and column points that are the most associated with the principal dimensions
# Dimension description
res.desc <- dimdesc(res.ca, axes = c(1,2))
# Description of dimension 1 by row points
head(res.desc[[1]]$row, 4)

# Description of dimension 1 by column points #Husband and wife are in opposite position
head(res.desc[[1]]$col, 4)

# Description of dimension 2 by row points
res.desc[[2]]$row
# Description of dimension 1 by column points
res.desc[[2]]$col
