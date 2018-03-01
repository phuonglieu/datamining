#View sngame data set after importing
View(sngame)

#Attach data set to simplify variable names
attach(sngame)

#verify data set and variables
names(sngame)
str(sngame)

#summary of the data set
summary(sngame)

#describe data set to get value number of valid case, missing value, standard deviation, median absolute deviation (mad),mean, median, min, max, range and standard error
library(Hmisc)
describe(sngame)

#Multivariate non-graphical statistics for categorical variables including gender and education
#create frequency table and proportion table for gender and edu
edu.table <-table(edu)
edu.table

prop.table(edu.table)

table(gender, edu)

#create the data frame containing only numerical variables: age, salary, game.min, game.purchase
sngame_num <- sngame[c(2,4,7,8)]

#calculates the correlation for all the pairs of variables
cor(sngame_num)

#testing the correlation of all numerical variables and proability values of this correlation, particularly to see p-value of two pairs (game.min& game.purchase and game.min&age)which have high correlation as we found earlier 
library(psych)
corr.test(sngame_num, use="complete")
cor.test(game.min,game.purchase)
cor.test(age,game.min)
cor.test(salary,game.purchase)

#t-test to understand if there is difference between two groups

t.test(game.min~gender)

t.test(game.purchase~gender)

#Anova
(aov1 <- aov(game.min~edu*gender)
summary(aov1)


#Graphical analysis of age, salary, game.min and game.purchase to identify the peak value of each variable
hist(age)
hist(salary)
plot(density(game.min))
plot(density(game.purchase))

#graphical analysis of education variable
barplot(table(edu))

#defind linear relationship between 2 pairs of variables
plot(game.min~game.purchase)
plot(age~game.min)
plot(salary~game.purchase)


#REGRESSION MODEL
#create linear regression model and detail of regression model of pair of variable (age and game.min) 
#and mutiple independent variables to predict the money spend on game depend on salary and how much time user spend on game

sngame_fit <- lm(game.min~age)
summary(sngame_fit) 

sngame_fit1 <-lm(game.purchase~salary+game.min)
summary(sngame_fit1)

# We use the linear model to estimate the time and money spending on the game of the future user for a new observation 
# using the predict function. For example, in the future group of user, the predicted time spent for the user age 40
predict(sngame_fit, data.frame(age=40))
predict(sngame_fit1,list(salary=6000, game.min=30))

# We can also calculate the confidence interval for the coefficients
confint(sngame_fit, level=0.95)
confint(sngame_fit1, level=0.95)

# Finally, we need to check whether the regression assumptions are fulfilled
plot(game.min~age)

# Linear rlationship
abline(sngame_fit)

library("car")

# Checking normality
qqPlot(sngame_fit)
qqPlot(sngame_fit1)


#CLUSTERING
#run "car" package
library("car")

#recode three-level variables
University <- recode(sngame$edu, "'University'=1; 'High School'=0; 'Elementary School'=0", as.factor.result=FALSE)
HighSchool <-recode(sngame$edu, "'University'=0; 'High School'=1; 'Elementary School'=0", as.factor.result=FALSE)
sngame <- cbind(sngame,University)
sngame <- cbind(sngame,HighSchool)

View(sngame)

sngame <- sngame[c(2,4,7:10)]

#rescale the dataset to be able to create a meaningful distance matrix
sngame_Scale <- scale(sngame)

#Hierarchical clustering
#create the distance matrix based on the Euclidean distance.
d <- dist(sngame_Scale, method = "euclidean")

#Ward Hierarchical Clustering method
#determine number of clusters
library(NbClust)
NbClust(sngame_Scale, distance = "euclidean", min.nc=2, max.nc=6, method = "ward.D", index = "all", alphaBeale = 0.1)  
fit <- hclust(d, method="ward.D")

#11 proposed 3 as the best number of clusters
#Create dendogram based on the created clustering
plot(fit)

# Check for 3 clusters

groups <- cutree(fit, k=3)

aggregate(sngame,by=list(groups),FUN=mean)
#seperate 3 clusters with yellow seperators
rect.hclust(fit, k=3, border="yellow")


#K-means clustering method
#determine number of clusters
wss <- (nrow(sngame_Scale)-1)*sum(apply(sngame_Scale,2,var)) 
for (i in 2:3) wss[i] <- sum(kmeans(sngame_Scale,centers=i)$withinss)

plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis

fit <- kmeans(sngame_Scale, 3) 

# Calculate cluster means

aggregate(sngame,by=list(fit$cluster),FUN=mean)

# Cluster Plot 

install.packages("cluster")
library(cluster)
clusplot(sngame_Scale, fit$cluster, color=TRUE, shade=TRUE,labels=5, lines=0)

# Centroid Plot 

install.packages("fpc")
library(fpc)
plotcluster(sngame_Scale, fit$cluster) 
