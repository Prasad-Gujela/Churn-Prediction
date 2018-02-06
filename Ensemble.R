## Q5
library(RODBC)
channel = odbcConnect(dsn = "PostgreSQL", uid = 'postgres', pwd = "Prasam@17")
odbcSetAutoCommit(channel, autoCommit = TRUE)
customerstats <- sqlQuery(channel, "select * from tr")
close(channel)

## Q6
customerstats <- subset(customerstats, select = -c(msno,registration_init_time,methother,planother,pricother,sub_churn))

## Q7
customerstats_sub <- subset(customerstats, customerstats$useforpredictions == "keep")
customerstats_sub <- subset(customerstats_sub, select = -useforpredictions)

# Q8
customerstats_rem <- na.omit(customerstats_sub)
write.csv(customerstats_rem, file = "MyData.csv")

# Q9
is.factor(customerstats_rem$gender)
is.factor(customerstats_rem$is_churn)
is.factor(customerstats_rem$city)
is.factor(customerstats_rem$registered_via)
customerstats_rem$is_churn = as.factor(customerstats_rem$is_churn)
customerstats_rem$city = as.factor(customerstats_rem$city)
customerstats_rem$registered_via = as.factor(customerstats_rem$registered_via)

## Q10
library(caret)
dmy <- caret::dummyVars("is_churn ~ .", data=customerstats_rem)
(dumb <- data.frame(predict(dmy, newdata = customerstats_rem)))
names(dumb) <- gsub("\\.", "", names(dumb))
dumb <- cbind(customerstats_rem$is_churn, dumb)

## Q11
filteredcust <- subset(dumb, select = -c(nearZeroVar(dumb)))
#filteredcust <- cbind(customerstats_rem$is_churn, filteredcust)

## Q12
correlations <- cor(filteredcust)
highCorr <- findCorrelation(correlations, cutoff = .9)
filteredcust <- filteredcust[,-highCorr]

## Q13
linearcust <- caret::findLinearCombos(filteredcust)

## Q14 
filteredcust$city4 <- as.factor(filteredcust$city4)
filteredcust$city5 <- as.factor(filteredcust$city5)
filteredcust$city6 <- as.factor(filteredcust$city6)
filteredcust$city13 <- as.factor(filteredcust$city13)
filteredcust$city15 <- as.factor(filteredcust$city15)
filteredcust$city22 <- as.factor(filteredcust$city22)
filteredcust$genderfemale <- as.factor(filteredcust$genderfemale)
filteredcust$registered_via3 <- as.factor(filteredcust$registered_via3)
filteredcust$registered_via4 <- as.factor(filteredcust$registered_via4)
filteredcust$registered_via7 <- as.factor(filteredcust$registered_via7)
filteredcust$registered_via9 <- as.factor(filteredcust$registered_via9)
filteredcust$latest_renew <- as.factor(filteredcust$latest_renew)
filteredcust$meth36 <- as.factor(filteredcust$meth36)
filteredcust$meth37 <- as.factor(filteredcust$meth37)
filteredcust$meth38 <- as.factor(filteredcust$meth38)
filteredcust$meth40 <- as.factor(filteredcust$meth40)
filteredcust$plan7 <- as.factor(filteredcust$plan7)
filteredcust$plan31 <- as.factor(filteredcust$plan31)
filteredcust$plan0 <- as.factor(filteredcust$plan0)
filtereddep <- cbind(customerstats_rem$is_churn, filteredcust)

## Q15
# preprocess values by centering and scaling (typical standardization)
preProcValues <- preProcess(filteredcust, method = c("center", "scale"))

# predict() actually transforms the variables
transformed <- predict(preProcValues, filteredcust)

## Q16
library("plyr")
library('caret')
library('rpart')
library('treebag')
transformed <- cbind(customerstats_rem$is_churn, transformed)
names <- colnames(transformed)
colnames(transformed) <- make.names(names, unique = TRUE)
transformed <- subset(transformed, select = -c(id))
colnames(transformed)[1] <- "is_churn"

set.seed(1000)
labelName <- 'is_churn'
predictors <- names(transformed)[names(transformed) != labelName]
myControl<- trainControl(method="cv", number=5, savePredictions = TRUE)
model_gbm <- train(transformed[,predictors], transformed[,labelName], method='gbm', trControl=myControl)

model_rpart <- train(transformed[,predictors], transformed[,labelName], method='rpart', trControl=myControl)

model_treebag <- train(transformed[,predictors], transformed[,labelName], method='treebag', trControl=myControl)




library(RODBC)
library(caret)
channel = odbcConnect(dsn = "PostgreSQL", uid = 'postgres', pwd = "Prasam@17")
odbcSetAutoCommit(channel, autoCommit = TRUE)
te_clean <- sqlQuery(channel, "select * from te_clean")
close(channel)
te_clmsno <- te_clean
te_clean$city4 <- as.factor(te_clean$city4)
te_clean$city5 <- as.factor(te_clean$city5)
te_clean$city6 <- as.factor(te_clean$city6)
te_clean$city13 <- as.factor(te_clean$city13)
te_clean$city15 <- as.factor(te_clean$city15)
te_clean$city22 <- as.factor(te_clean$city22)
te_clean$genderfemale <- as.factor(te_clean$genderfemale)
te_clean$registered_via3 <- as.factor(te_clean$registered_via3)
te_clean$registered_via4 <- as.factor(te_clean$registered_via4)
te_clean$registered_via7 <- as.factor(te_clean$registered_via7)
te_clean$registered_via9 <- as.factor(te_clean$registered_via9)
te_clean$latest_renew <- as.factor(te_clean$latest_renew)
te_clean$meth36 <- as.factor(te_clean$meth36)
te_clean$meth37 <- as.factor(te_clean$meth37)
te_clean$meth38 <- as.factor(te_clean$meth38)
te_clean$meth40 <- as.factor(te_clean$meth40)
te_clean$plan7 <- as.factor(te_clean$plan7)
te_clean$plan31 <- as.factor(te_clean$plan31)
te_clean$plan0 <- as.factor(te_clean$plan0)
te_clean <- subset(te_clean, select = -c(msno))
te_clean <- subset(te_clean, select = -c(is_churn))


## Q26
preProcValues <- preProcess(te_clean, method = c("center", "scale"))
transformed2 <- predict(preProcValues, te_clean)
#colnames(transformed2)[1] <- "customerstats_rem.is_churn"

transformed$gbm_PROB <- predict(object=model_gbm, transformed[,predictors])
transformed$rf_PROB <- predict(object=model_rpart, transformed[,predictors])
transformed$treebag_PROB <- predict(object=model_treebag, transformed[,predictors])
labelName <- 'is_churn'
predictors <- names(transformed)[names(transformed) != labelName]
final_ens_model <- train(transformed[,predictors], transformed[,labelName], method='gbm', trControl=myControl)


## Q27
predict_clean<- predict(final_ens_model,transformed2, type='prob')
dfp = data.frame(te_clmsno$msno, predict_clean) 
#a = cbind(te_clmsno$msno, predict_clean)
write.table(dfp, 'Prasad9.csv', col.names=NA)