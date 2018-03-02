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

 
 
 ### 0. Initialization

## Set working directory


### 1. Text statistics

# Read file
doc = "transaction_management_system.txt"

# Function for loading text data
tokenize = function(file){
  # Read file
  text = Reduce(paste, readLines(file))
  tokens = unlist(lapply(strsplit( # Tokenize (split into words)
    gsub("\\.|,|!|\\?|:|;|\\(|\\)|\\[|\\]|\\-\\-", "", text), " "), # Filter special characters
    tolower)) # Make lowercase
  return(tokens)
}
  
## 1.1 Term statistics

tokens = tokenize(doc)

# Calculate term frequencies
term_freqs = table(tokens)

# Plot histogram of term frequencies
hist(term_freqs, breaks=500, xlab="Term Frequency (TF)", ylab="TF count")

# Plot histogram zoomed in on infrequent terms 
hist(term_freqs, breaks=500, xlab="Term Frequency (TF)", ylab="TF count", xlim=c(0,20))


# Print most frequent terms
term_freqs[order(term_freqs, decreasing=TRUE)][1:30]


## 1.2 N-gram statistics

# Make bigrams
bigrams = unlist(Map(function(i) paste(tokens[i],tokens[i+1]), 2:length(tokens)-1))

# Calculate bigram frequencies
bigram_freqs = table(bigrams)

# Print most frequent bigrams
bigram_freqs[order(bigram_freqs, decreasing=TRUE)][1:30]


## 1.3 Keyword extraction

# Read list of input files in data directory
files = list.files(".", "*.txt")

# Shuffle the alphabetical order of documents
set.seed(1) # Fixing the random seed will give consistent results between runs
files = sample(files)

## Loop through all files, and process the text
## Note: Run the entire for loop at once
for (file in files[1:100]){
	print(file)
	# Load file and split into terms
  tokens = tokenize(file)
	# Calculate term frequencies (TF) for document
	TFtable = data.frame(table(tokens))
	TF = data.frame(t(data.frame(TFtable$Freq, row.names=TFtable$tokens)))

	# Combine term frequencies for each document into data frame allTF
	if (file == files[1]){
		allTF = TF
	} else {
		# Pad TF with columns
		# terms in allTF and not in TF: add 0 to TF
		for(newterm in setdiff(names(allTF), names(TF))){
			TF[[newterm]] = 0
		}

		# Pad allTF with columns
		# terms in TF and not in allTF: add 0 vector to allTF
		for(newterm in setdiff(names(TF), names(allTF))){
			allTF[[newterm]] = rep(0, nrow(TF))
		}

		# Append TF to allTF
		allTF = rbind(allTF, TF)
	}
} ## End of for-loop


# Caluclate document frequencies (number of documents containing term)
DF = data.frame(
	Map(function(col) 
		Reduce(function(a,b) a+b, 
			Map(function(f) f > 0, col)
		), 
	allTF)
)


# Calculate TF-IDF score matrix
scores = allTF*matrix(ncol=1, data=log(nrow(allTF)/DF))


# Get top keywords for each document
keywords = Map(function(x) unlist(scores[x,order(scores[x,], decreasing=TRUE)][1:20]), 1:5)
names(keywords) = files[1:5]
keywords


## 2. Document clustering
## 2.1 Similarity

# Define cosine normalization function
cnorm = function(v) 1/sqrt(sum(unlist(Map(function(x) x^2, v))))
# Define cosine similarity function
sim = function(v1, v2) sum(array(unlist(v1))*array(unlist(v2)))*(cnorm(array(unlist(v1)))*cnorm(array(unlist(v2))))

# Calculate document similarity matrix
ndocs=30
simmx = matrix(ncol=ndocs, nrow=ndocs)
for(i in 1:ndocs){
      writeLines(paste("Comparing document",i))
      for(j in 1:ndocs){
      	    simmx[i,j] = sim(scores[i,], scores[j,])
	    }
}


## 2.2 Hierarchical clustering

# Calculate distance matrix (inverse of similarity/adjacency matrix)
distances = as.dist(1-simmx)

# Generate labels for documents (50 first characters of title)
labs=Map(function(f){substr(f, 1, 50)}, files[1:ndocs])

# Perform clustering
model = hclust(distances, method="ward.D2")
model1= hclust(distances, method="single")
model2= hclust(distances, method="complete")


# Plot dendrogram
plot(model)

# Plot dendrogram with filename labels
plot(model,labels=labs, cex=0.7)
plot(model1,labels=labs, cex=0.7)
plot(model2,labels=labs, cex=0.7)


## 2.3 Partitioning
# Partition hierarchy into k number of non-overlapping clusters
rect.hclust(model, k=6, border="blue") 


### Bonus code: Alternative visualization (not needed for assignment)
# Load/install package
if(!require(MASS)){ install.packages("MASS"); library(MASS) }

# Project data onto 2 dimensions using Sammon's mapping
projection = sammon(distances,k=2)$points

# Visualize projection
plot(projection,pch=19)
text(projection,labels=labs,cex=0.7,adj=c(0.5,2))
### 


### 3. Topic Modeling
if(!require(ggplot2)){ install.packages("ggplot2"); library(ggplot2) }
if(!require(reshape2)){ install.packages("reshape2"); library(reshape2) }
if(!require(lda)){ install.packages("lda"); library(lda) }

# Read all files and filter special characters
documents = concatenate.documents(Map(function(f){
  gsub("\\.|,|!|\\?|:|;|\\(|\\)|\\[|\\]|\\-\\-|\"|\'", "", readLines(f))
  }, files))

# Convert corpus for LDA
corpus = lexicalize(documents)

# Number of topics
K = 10
K = 6


# Train topic model
result = lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab, 25, alpha=0.1, eta=0.1)

# Get the top words for topics
top.words = top.topic.words(result$topics, 8, by.score=TRUE)
top.words

# Get topic proportions
result$topic_sums/sum(result$topic_sums)

x

# Number of documents to visualize
N = 20
# Get topic distributions for documents 1 to 20 (shows number of words)
result$document_sums[,1:20]

# Or, get topic probability distributions for documents 1 to 20 (divides each column by its sum to get proportions)
result$document_sums[,1:20] / apply(result$document_sums[,1:20], 2, sum)



topic.proportions = t(result$document_sums) / colSums(result$document_sums)
topic.proportions = topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] = 1/K
colnames(topic.proportions) = apply(top.words, 2, paste, collapse=" ")
topic.proportions.df = melt(cbind(data.frame(topic.proportions), document=factor(1:N)),
                            variable.name="topic", id.vars = "document")  

# Plot topic distributions for sample documents
qplot(topic, value, fill=document, ylab="proportion", data=topic.proportions.df, geom="bar") +
      opts(axis.text.x = theme_text(angle=90, hjust=1)) + coord_flip() + facet_wrap(~ document, ncol=5)

qplot(topic, value, fill=document, ylab="proportion", data=topic.proportions.df, geom="bar") +
   coord_flip() + facet_wrap(~ document, ncol=5)


## Data Mining and Text Mining - Assignment 3 - Code extension
## Alternative topic term reranking method (inspired by TF*IDF)
# The following code can be used in parallel with top.topic.words
# It uses the LDA corpus and model


# Get corpus term counts
term_counts = data.frame(table(term=unlist(corpus$documents)))
term_counts$term = corpus$vocab

# Count number of topics each term occurs in (similar to document frequency (DF))
topic_count_per_term = 0
for(i in 1:K){
  topic_count_per_term = topic_count_per_term + (result$topics[i,]>0)*1
}

# Function for reranking and getting top terms
top_terms = function(topicN){  
  # Count terms in topicN
  term_counts_in_topic = data.frame(cnt=result$topics[topicN,])  
  # Calculate reweighted score (Term Frequency in topic * Inverted Topic Frequency)
  term_scores = term_counts_in_topic * log(K/(topic_count_per_term))
  # Sort and extract results
  term_indices = order(term_scores, decreasing=T)
  top_terms = row.names(term_scores)[term_indices]
  top_scores = term_scores[term_indices,]
  return(head(top_terms, termN))
  # Alt: include weighting score
  #return(head(paste(top_terms,round(top_scores,2)), termN))
}

# Number of top terms to list
termN=8

# Get top terms for each topic 1:K
sapply(1:K, top_terms)

# Save results for use in plot
top.words = sapply(1:K, top_terms)

## Alternative topic term reranking method (inspired by TF*IDF)
# The following code can be used in parallel with top.topic.words
# It uses the LDA corpus and model


# Get corpus term counts
term_counts = data.frame(table(term=unlist(corpus$documents)))
term_counts$term = corpus$vocab

# Count number of topics each term occurs in (similar to document frequency (DF))
topic_count_per_term = 0
for(i in 1:K){
  topic_count_per_term = topic_count_per_term + (result$topics[i,]>0)*1
}

# Function for reranking and getting top terms
top_terms = function(topicN){  
  # Count terms in topicN
  term_counts_in_topic = data.frame(cnt=result$topics[topicN,])  
  # Calculate reweighted score (Term Frequency in topic * Inverted Topic Frequency)
  term_scores = term_counts_in_topic * log(K/(topic_count_per_term))
  # Sort and extract results
  term_indices = order(term_scores, decreasing=T)
  top_terms = row.names(term_scores)[term_indices]
  top_scores = term_scores[term_indices,]
  return(head(top_terms, termN))
  # Alt: include weighting score
  #return(head(paste(top_terms,round(top_scores,2)), termN))
}

# Number of top terms to list
termN=8

# Get top terms for each topic 1:K
sapply(1:K, top_terms)

# Save results for use in plot
top.words = sapply(1:K, top_terms)
