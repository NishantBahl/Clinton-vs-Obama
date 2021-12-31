########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)
str(election_data)
##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)

###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
### 
library(ggplot2)
library(dplyr)
library(ggthemes)
library(MASS)
library(scales)


ggplot(election_data_train, aes(MalesPer100Females, Obama_margin_percent, color = Black, ylab("Obama Vote Percent Margin")))+
  geom_point(alpha = 0.3)+
  scale_fill_discrete()+
  scale_x_log10()+
  scale_y_continuous(breaks=seq(-100,100,100))+
  facet_wrap(~State)+
  scale_y_continuous(name = "Obama Margin Percent")+
  scale_x_continuous(name = "Males Per 100 Females: Log Scale")+
  geom_smooth(method = lm, color = "red", alpha = 0.5, size = 0.2)+
  ggtitle("Obama Vote Margin vs. Proportion of Male and Black")
###
### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###


#creating model
election_data_train_interactions <- election_data_train

#region variables
election_data_train_interactions$RegionMidwest <- c(election_data_train$Region == "Midwest")*1
election_data_train_interactions$RegionNortheast <- c(election_data_train$Region == "Northeast")*1
election_data_train_interactions$RegionWest <- c(election_data_train$Region == "West")*1

#election type variables
election_data_train_interactions$Etype <- c(election_data_train$ElectionType == "Caucuses")*1



installpkg("randomForest")
library(randomForest)

n <- nrow(election_data_train)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(linear=rep(NA,nfold),linear1=rep(NA,nfold),random_node_6 = rep(NA,nfold),random_node_5 = rep(NA,nfold)) 

election_data_train_interactions$Obama_prob <- election_data_train_interactions$Obama_margin_percent / 100

#data for linear regression
keep <- c("MalesPer100Females","AgeBelow35","Age35to65","Age65andAbove","White","Black","Asian",               
          "AmericanIndian",       "Hawaiian" ,            "Hispanic"  ,           "HighSchool"  ,        
          "Bachelors"       ,     "Poverty"        ,      "IncomeAbove75K"   ,    "MedianIncome"   ,     
          "AverageIncome"  ,      "UnemployRate"    ,     "ManfEmploy"        ,   "SpeakingNonEnglish"  ,
          "Medicare"     ,        "MedicareRate"     ,    "SocialSecurity"    ,   "SocialSecurityRate"  ,
          "RetiredWorkers"    ,   "Disabilities"      ,   "DisabilitiesRate"    , "Homeowner"  ,         
          "SameHouse1995and2000" ,"Pop"   ,               "PopDensity"       ,    "LandArea" ,           
          "FarmArea"  ,"RegionMidwest"      ,  "RegionNortheast"    ,  "RegionWest"         ,  "Etype","Obama_margin_percent")
clean_election_data <- election_data_train_interactions[keep]


#data for second linear regression
keep_check <- c("White","Asian",               
          "AmericanIndian",       "Hawaiian" ,            "Hispanic" ,        
          "Bachelors"       ,     "Poverty"        ,     
               "UnemployRate"    ,     "ManfEmploy"        ,   "SpeakingNonEnglish"  ,
                   "MedicareRate"     ,      "Disabilities"      ,   "DisabilitiesRate"    ,        
          "SameHouse1995and2000" ,"Pop"   ,               "PopDensity"       ,    "LandArea" ,           
          "FarmArea"  ,"RegionMidwest"      ,  "RegionNortheast"    ,  "RegionWest"         ,  "Etype","Obama_margin_percent")
clean_election_data_check <- clean_election_data[keep_check]
### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit the two regressions and null model
  
  model.linear <-glm(Obama_margin_percent ~ ., data=clean_election_data, subset=train)
  model.linear1 <-glm(Obama_margin_percent ~ ., data=clean_election_data_check, subset=train)
  
  #model.tree <- tree(factor(Obama_wins)~ ., subset=train, data=election_data_train) 
  model.random_node_6 <- randomForest(Obama_margin_percent ~ .-County -State -Region -FIPS -ElectionDate -ElectionType -Clinton -Obama
                               -Obama_margin -Obama_wins, data=election_data_train_interactions, subset=train,nodesize=6, ntree = 500, mtry = 6)
  model.random_node_5 <- randomForest(Obama_margin_percent ~ .-County -State -Region -FIPS -ElectionDate -ElectionType -Clinton -Obama
                                -Obama_margin -Obama_wins, data=election_data_train_interactions, subset=train,nodesize=5, ntree = 500, mtry = 4)
  
  ## get predictions: type=response so we have probabilities
  
  pred.linear <- predict(model.linear, newdata=clean_election_data[-train, ], type="response")
  pred.linear1 <- predict(model.linear1, newdata=clean_election_data_check[-train, ], type="response")
  
  #pred.tree                 <- predict(model.tree, newdata=election_data_train[-train, ], type="vector")
  #pred.tree <- pred.tree[,2]
  pred.null <- predict(model.nulll, newdata=election_data_train[-train, ], type="response")
  pred.random <- predict(model.random_node_6, newdata=election_data_train_interactions[-train, ], type="response")
  pred.random1 <- predict(model.random_node_5, newdata=election_data_train_interactions[-train, ], type="response")
  
  ## calculate and log R2
  # Linear
  OOS$linear[k] <- R2(y=clean_election_data$Obama_margin_percent[-train], pred=pred.linear)
  OOS$linear[k]
  
  OOS$linear1[k] <- R2(y=clean_election_data_check$Obama_margin_percent[-train], pred=pred.linear1)
  OOS$linear1[k]
  
  # Random with nodesize 6
  OOS$random_node_6[k] <- R2(y=election_data_train_interactions$Obama_margin_percent[-train], pred=pred.random)
  OOS$random_node_6[k]
  # Random with nodesize 5
  OOS$random_node_5[k] <- R2(y=election_data_train_interactions$Obama_margin_percent[-train], pred=pred.random1)
  OOS$random_node_5[k]
  
  print(paste("Iteration",k,"of",nfold,"(tLet the magic happen"))
}


colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

model.random <- randomForest(Obama_margin_percent ~ ., data=clean_election_data, subset=train,nodesize=6, ntree = 500, mtry = 6)


#creating test data
election_data_test_interactions <- election_data_test

#region variables
election_data_test_interactions$RegionMidwest <- c(election_data_test$Region == "Midwest")*1
election_data_test_interactions$RegionNortheast <- c(election_data_test$Region == "Northeast")*1
election_data_test_interactions$RegionWest <- c(election_data_test$Region == "West")*1

#election type variables
election_data_test_interactions$Etype <- c(election_data_test$ElectionType == "Caucuses")*1

#Run this code to generate the predictions for the winning spread.
pred.random <- predict(model.random, newdata=election_data_test_interactions, type="response")
count <- sum(pred.random > 0)
count
write.csv(pred.random, "electionDataTestPredictions.csv")

count <- sum(pred.random > 0)

#########
#END OF QUESTION 2
#########


installpkg("tree")
library(tree)
installpkg("partykit")
library(partykit)
installpkg("randomForest")
library(randomForest)
install.packages("dplyr")


election_data_train_numeric_var <- sapply(election_data_train_numeric, var, na.rm=TRUE)
election_data_train_numeric_var ==0
election_data_train_numeric <- election_data_train %>% select(MalesPer100Females:Obama_margin_percent)
xdata <- model.matrix(Obama_margin_percent~., data = election_data_train_numeric)
pca.data <- prcomp(xdata, scale=FALSE)

par(mar=c(4,4,4,4)+0.3)
plot(pca.data,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
loadings <- pca.data$rotation[,1:3]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(xdata)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

###
### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###
xdata <- election_data_train_numeric
pca.data <- prcomp(xdata, scale=TRUE)
### Lets plot the variance that each component explains
par(mar=c(4,4,4,4)+0.3)
plot(pca.data,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

loadings <- pca.data$rotation[,1:4]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:ncol(xdata)],1]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### Looking at which are large positive and large negative
#### Fisrt factor is Luxurious/Status/High-Tech Cars
####
#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:ncol(xdata)],2]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### This is convenient for suburb families
####
#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:ncol(xdata)],3]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 4
v<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:ncol(xdata)],4]
loadingfit <- lapply(1:ncol(xdata), function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]





###
### Question 4(a) impact of changing hispanic demographic
###
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

#4a----------------------------------------------------------------------------------------------------------------

#outcome
y <- election_data_train$Obama_margin_percent
#controls
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
#treatment
d <- election_data_train$Hispanic

### DML with Split Sampling
#### Note that there is some randomness in RF
#### therefore the precise numbers are likely to differ
installpkg("randomForest")
library(randomForest)
nfold <- 2 
n <- nrow(x) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
datarf <- data.frame(x,y)
predy <- rep(0,n)
for ( id in 1:nfold ){
  rf <- randomForest( y ~ ., data=datarf[which(foldid!=id),])
  predy[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
datarf <- data.frame(x,d)
predd <- rep(0,n)
for ( id in 1:nfold ){
  rf <- randomForest( d ~ ., data=datarf[which(foldid!=id),])
  predd[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
V <- y-predy
W <- d-predd
summary(glm( V ~ W ))$coef["W",]

# Low p-value (0.00821) == statistically significant
# Negative estimate means if Hispanic pop increase, Obama margin percentage would decrease with coefficient of -0.223
# For 5% increase in Hispanic pop, avg Obama margin %age decrease would be 5*(-)0.222747459 = 1.114


####END OF 4A


### Question 4(b) impact of changing black demographic
####
#### Model with 1771 controls to measure the impact of 10% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####


#4b----------------------------------------------------------------------------------------------------------------

y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black

### DML with Split Sampling
#### Note that there is some randomness in RF
#### therefore the precise numbers are likely to differ
nfold <- 2 
n <- nrow(x) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
datarf <- data.frame(x,y)
predy <- rep(0,n)
for ( id in 1:nfold ){
  rf <- randomForest( y ~ ., data=datarf[which(foldid!=id),])
  predy[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
datarf <- data.frame(x,d)
predd <- rep(0,n)
for ( id in 1:nfold ){
  rf <- randomForest( d ~ ., data=datarf[which(foldid!=id),])
  predd[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
V <- y-predy
W <- d-predd
summary(glm( V ~ W ))$coef["W",]

# Low p-value (4.390116e-26) == statistically significant
# Positive estimate means if Black pop increase, Obama margin percentage would increase with coefficient of 1.092468e+00
# For 5% increase in Black pop, avg Obama margin %age increase would be 5*1.092468e+00 = 5.462

####END OF 4B

####
#### Question 5: No additional R code. Keep in mind that you can build upon your previous 
#### analysis or develop new analysis.
####

