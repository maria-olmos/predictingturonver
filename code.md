---
title: "Individual Project"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
library(ggplot2)
library(dplyr)
```

```{r cars}
#Load Data Set
HR <- read.csv("HR-Employee-Attrition.csv")
summary(HR)
```
Exploratory Data Anlysis 

```{r}
# Attrition Count
AttritionCount <- ggplot(HR, aes(x=Attrition, fill = Attrition)) + geom_bar() + labs(title = "Attrition Count") +theme(plot.title=element_text(hjust=0.5))
AttritionCount
```

```{r}
# Percentage of Attrition 

proportion <- HR %>%
group_by(Attrition) %>% tally() %>% mutate(percentage = n / sum(n))


AttritionPct = ggplot(proportion, aes(Attrition, percentage, fill = Attrition)) + geom_bar(stat = "identity", color = "black")+geom_text(aes(label=percentage), vjust=-.5)+  labs(title ="Percentage of Attrition") +
  theme(plot.title = element_text(hjust = 0.5))
AttritionPct
```


```{r}
# Attrition By Gender 
AttritionGender <- ggplot(HR, aes(x=Gender, fill = Attrition)) + geom_bar(position = "fill") + labs(title = "Attrition By Gender") + theme(plot.title=element_text(hjust=0.5)) 
AttritionGender
```
```{r}
# Attrition By Role

AttritionRole <- ggplot(HR, aes(x=JobRole, fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Role") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=40, vjust=1, hjust=1))
AttritionRole
```
```{r}
# Attrition By Travel Frequency
AttritionTravel <- ggplot(HR, aes(x=BusinessTravel, fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Travel Frequency") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
AttritionTravel
```
```{r}
# Attrition By Number of Companies Worked

AttritionNumComp <- ggplot(HR, aes(x=NumCompaniesWorked, fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Number of Companies Worked") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
AttritionNumComp
```

```{r}
# Attrition By Total Working Years 

AttritionWorkingYears <- ggplot(HR, aes(TotalWorkingYears, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5)  + labs(title ="Attrition by Total Working Years") +
  theme(plot.title = element_text(hjust = 0.5))
AttritionWorkingYears
```

```{r}
#Job Satisfaction By Gender

ggplot(HR, aes(x=Gender, y=JobSatisfaction, fill=Attrition)) +
  geom_boxplot() + 
  labs(title ="Job Satisfaction vs Gender") +
  theme(plot.title = element_text(hjust = 0.5))
```
```{r}
# Pay Distribution Accross Roles

PayDitributionRole <- ggplot(HR, aes(x=JobRole, y=MonthlyIncome, fill=JobRole)) +
  geom_boxplot() +
  theme(legend.position="none", 
        legend.background=element_blank(),
        axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        plot.title=element_text(hjust=0.5)) +
  labs(title="Pay Distribution Across Roles", x="Job Role")
PayDitributionRole
```

```{r}
# PAY DISTRIBUTION ACROSS EDUCATION LEVEL 

#Education
#1 'Below College'
#2 'College'
#3 'Bachelor'
#4 'Master'
#5 'Doctor'

ggplot(HR, aes(x=factor(Education), y=MonthlyIncome, fill=Education)) +
  geom_boxplot() +
  theme(legend.position="none", 
        legend.background=element_blank(),
        axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        plot.title=element_text(hjust=0.5)) +
  labs(title="Pay distribution across Education Level", x="Education Level")
```

```{r}
# Pay Distribution Across Job Satisfaction
# Job Satisfaction 
#1 'Low'
#2 'Medium'
#3 'High'
#4 'Very High'

IncomeJobSatisfaction <- ggplot(HR, aes(x=factor(JobSatisfaction), y=MonthlyIncome, fill=Education)) +
  geom_boxplot() +
  theme(legend.position="none", 
        legend.background=element_blank(),
        axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        plot.title=element_text(hjust=0.5)) +
  labs(title="Pay distribution across Job Satisfaction", x="Job Satisfaction") 
IncomeJobSatisfaction
```

```{r}
#Attrition By Job Satisfaction

JobSatisfactionAttrition <- ggplot(HR, aes(x=factor(JobSatisfaction), fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Job Satisfaction") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
JobSatisfactionAttrition
```

```{r}
#Attrition By Environment Satisfaction

#EnvironmentSatisfaction
#1 'Low'
#2 'Medium'
#3 'High'
#4 'Very High'


EnviroSatisfactionAttrition <- ggplot(HR, aes(x=factor(EnvironmentSatisfaction), fill = Attrition)) + geom_bar(position = "fill",color = "black") + labs(title = "Attrition By Environment Satisfaction") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
EnviroSatisfactionAttrition
```

```{r}
# Work Life balance

#WorkLifeBalance 
#1 'Bad'
#2 'Good'
#3 'Better'
#4 'Best'

WorklifeBalance <- ggplot(HR, aes(x=factor(WorkLifeBalance), fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Work Life Balance") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
WorklifeBalance
```

```{r}
# Job Involvement Attrition
# JobInvolvement 
#1 'Low'
#2 'Medium'
#3 'High'
#4 'Very High'

AttritionJobInvolvement <- ggplot(HR, aes(x=factor(JobInvolvement), fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Job Involvement") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
AttritionJobInvolvement
```
```{r}
# Pay Distribution Across Education Field

EducationFieldIncome <- ggplot(HR, aes(x=factor(EducationField), y=MonthlyIncome, fill=EducationField)) +
  geom_boxplot() +
  theme(legend.position="none", 
        legend.background=element_blank(),
        axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        plot.title=element_text(hjust=0.5)) +
  labs(title="Pay distribution across Education Field", x="Job Role")
EducationFieldIncome
```
```{r}
#Attrition By Over Time 

OverTimeAttrition<- ggplot(HR, aes(x=factor(OverTime), fill = Attrition)) + geom_bar(position = "fill", color="black") + labs(title = "Attrition By Over Time") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
OverTimeAttrition
```

```{r}
# Attrition By Age Group 

AttritionByAge<-  ggplot(HR, aes(Age, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5)+ labs(title="Attrition by Age Group")+theme(plot.title=element_text(hjust=0.5))
AttritionByAge
```
```{r}
#Attrition by Hourly Rate

AttritionByHourly<-  ggplot(HR, aes(HourlyRate, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5)+labs(title="Attrition by Hourly Rate")+theme(plot.title=element_text(hjust=0.5))
AttritionByHourly
```


```{r}
# Distribution of Over Time By Job Role

OverTimeJob<- ggplot(HR, aes(x=factor(JobRole), fill = OverTime)) + geom_bar() + labs(title = "Over Time and Job Role", color = "black") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
OverTimeJob
```
```{r}
# Attrition by Performance Rating

PerformanceRatingAttrition<- ggplot(HR, aes(x=factor(PerformanceRating), fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Performance Rating") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
PerformanceRatingAttrition
```
```{r}
#Attrition By Relationship Satisfaction

#RelationshipSatisfaction 
#1 'Low'
#2 'Medium'
#3 'High'
#4 'Very High'

OverTimeAttrition<- ggplot(HR, aes(x=factor(RelationshipSatisfaction), fill = Attrition)) + geom_bar(position = "fill", color = "black") + labs(title = "Attrition By Relationship Satisfaction") + theme(plot.title=element_text(hjust=0.5),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
OverTimeAttrition
```
```{r}
# Attrition By Years at Company

YearsAtComapanyAttrition<-  ggplot(HR, aes(YearsAtCompany, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5) +  labs(title = "Attrition By Years Worked at Company") + theme(plot.title=element_text(hjust=0.5))     
YearsAtComapanyAttrition
```
```{r}
# Attrition By Years in Current Role

YearsCurrentRoleAttrition<-  ggplot(HR, aes(YearsInCurrentRole, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5) + labs(title = "Attrition By Years in Current Role") + theme(plot.title=element_text(hjust=0.5))
YearsCurrentRoleAttrition
```
```{r}
#Attrition By Years Since Last Promotion 
YearsLastPromotion <- ggplot(HR, aes(YearsSinceLastPromotion, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5)+ labs(title = "Attrition By Years Since Last Promotion ") + theme(plot.title=element_text(hjust=0.5))
YearsLastPromotion
```
```{r}
#Attrition by Years With Current Manager
YearsWithCurrentManager <-ggplot(HR, aes(YearsWithCurrManager, fill=Attrition)) + geom_histogram(stat="bin", color = "black", binwidth= 5)+  labs(title = "Attrition by Years With Current Manager") + theme(plot.title=element_text(hjust=0.5))
YearsWithCurrentManager
```


```{r}
# CORRELATION MATRIX 
library(ggcorrplot)
Corrdf <- data.frame(MonthlyIncome = HR$MonthlyIncome, 
                    DistanceFromHomwe = HR$DistanceFromHome,
                    Age = HR$Age, 
                    YearsatCompany = HR$YearsAtCompany,
                    YearsCurrentRole = HR$YearsInCurrentRole,
                    YearsLastPromotion = HR$YearsSinceLastPromotion,
                    YearsCurrentManager = HR$YearsWithCurrManager,
                    TotalWorkingYears = HR$TotalWorkingYears,
                    NumberCompaniesWorked= HR$NumCompaniesWorked)
ggcorrplot(cor(Corrdf), lab=TRUE)
```

TEST SPLIT DATA 

```{r}
set.seed(7032) # a seed makes the analysis reproducible 
# so everyone will get the same results 
ndata <- nrow(HR)
# Randomly choose 0.8n indices between 1 and n 
train_index <- sample(1:ndata, size=0.8*ndata, rep=FALSE)
test_index <- setdiff(1:ndata, train_index)

# obtain the train dataset and test dataset 
train <- HR[train_index,]
test <- HR[test_index, ]
dim(train); dim(test)
```



MODELING 

```{r}
# First model WITH ALL VARIABLES 
 
logmodel1 <- glm(factor(Attrition)~ Age+ factor(BusinessTravel)+ DailyRate+ factor(Department)+ DistanceFromHome+factor(Education)+ factor(EducationField)+ factor(EnvironmentSatisfaction)+factor(Gender) + HourlyRate+ factor(JobInvolvement)+ factor(JobLevel) +  factor(JobRole)+ factor(JobSatisfaction)+ factor(MaritalStatus) + MonthlyIncome+ MonthlyRate+ NumCompaniesWorked+factor(OverTime)+ PercentSalaryHike+factor(RelationshipSatisfaction)+TotalWorkingYears+ YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+ YearsWithCurrManager+ factor(WorkLifeBalance)+TrainingTimesLastYear+ factor(StockOptionLevel), data=train, family="binomial")
summary(logmodel1)
```
```{r}
# MODEL AFTER MANUALLY PICKING VARIABLES 
logmodel2 <- glm(factor(Attrition)~  Age+ DistanceFromHome+YearsInCurrentRole+DailyRate+ NumCompaniesWorked+factor(OverTime)+factor(RelationshipSatisfaction) + factor(WorkLifeBalance)+ factor(JobRole)+factor(BusinessTravel)+factor(JobInvolvement)+factor(JobSatisfaction)+factor(Gender)+factor(MaritalStatus), data=train, family="binomial")
summary(logmodel2)
```
```{r}
Rsq <- 1 - logmodel2$deviance/logmodel2$null.deviance
Rsq
```


```{r}
#LASSO REGRESSION METHOD 
#PART 1
x_matrix <- model.matrix(Attrition ~  Age+ factor(BusinessTravel)+ DailyRate+ factor(Department)+ DistanceFromHome+factor(Education)+ factor(EducationField)+ factor(EnvironmentSatisfaction)+factor(Gender) + HourlyRate+ factor(JobInvolvement)+ factor(JobLevel) +  factor(JobRole)+ factor(JobSatisfaction)+ factor(MaritalStatus) + MonthlyIncome+ MonthlyRate+ NumCompaniesWorked+factor(OverTime)+ PercentSalaryHike+factor(RelationshipSatisfaction)+TotalWorkingYears+ YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+ YearsWithCurrManager+ factor(WorkLifeBalance)+TrainingTimesLastYear+ factor(StockOptionLevel), data=train)[,-1]
y_matrix <- train$Attrition == "Yes"

num.cols <- ncol(x_matrix)
num.rows <- nrow(x_matrix)
num.attrition <- sum(y_matrix)
w <- (num.attrition/num.rows)*(1-(num.attrition/num.rows))

lambda.theory <- sqrt(w*log(num.cols/0.05)/num.rows)
lambda.theory
```


```{r}
library(glmnet)
cv <- cv.glmnet(x= as.matrix(x_matrix), y=y_matrix, alpha = 1)
cv
```

```{r}
plot(cv)
```
```{r}
as.matrix(coef(cv, lambda.theory))
```
```{r}
# MODEL 3 USING LASSO (FINAL MODEL FOR PREDICTING ATTRITION)

logmodel3 <- glm(factor(Attrition)~  Age+factor(BusinessTravel)+DistanceFromHome+factor(JobLevel)+factor(JobRole)+factor(JobSatisfaction)+factor(MaritalStatus)+MonthlyIncome+NumCompaniesWorked+factor(OverTime)+YearsInCurrentRole+factor(WorkLifeBalance)+factor(StockOptionLevel), data=train, family="binomial")
summary(logmodel3)
```



```{r}
#Classification Tree
# Decision Tree with All Variables 
library(tree)
tree3<-tree(factor(Attrition)~ .,data=HR)  
plot(tree3)
text(tree3, label="yval")
```


```{r}
# Classification Tree 2 - Lasso Variables
library(tree)
LasoAttritionTree<-tree(factor(Attrition)~ Age+BusinessTravel+DistanceFromHome+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+NumCompaniesWorked+OverTime+YearsInCurrentRole+WorkLifeBalance+StockOptionLevel,data=train)
plot(LasoAttritionTree)
text(LasoAttritionTree, label="yval")
```

MODEL EVALUATION
```{r}
# MODEL 2 - Manually Picked Variables
library(pROC)
LogModel2Prediction <- predict(logmodel2, test, type = "response")
model2ROC <- roc(response = test$Attrition, predictor = as.numeric(LogModel2Prediction), plot=TRUE,  print.auc=TRUE)
model2ROC
```

```{r}
# ROC Model 3 - Lasso Regression Variables
LogModel3Prediction <- predict(logmodel3, test, type = "response")
model3ROC <- roc(response = test$Attrition, predictor = as.numeric(LogModel3Prediction), plot=TRUE,  print.auc=TRUE)
model3ROC
```



```{r}
# ROC Tree Laso Variables
LasoTreePrediction<- predict(LasoAttritionTree, test, type = "class")
LasoTreeROC <- roc(response = test$Attrition, predictor = as.numeric(LasoTreePrediction), plot=TRUE,  print.auc=TRUE)
LasoTreeROC
```


```{r}
# ROC of Decision Tree with ALL VARIABLES
TreePrediction3<- predict(tree3, test, type = "class")
TreeROC3 <- roc(response = test$Attrition, predictor = as.numeric(TreePrediction3), plot=TRUE,  print.auc=TRUE)
TreeROC3
```

```{r}
# All Models ROC

plot(model2ROC, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(LasoTreeROC, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)
plot(TreeROC3, col = "red" , add = TRUE, print.auc.y = 0.74, print.auc = TRUE)
plot(model3ROC, col = "Green" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)

legend("bottom", c("Logistic", "Lasso Regression", "Decision Tree","Decision Tree Lasso Variables" ),
       lty = c(1,1), lwd = c(1, 2), col = c( "black", "Green","blue", "red"), cex = 0.5)
```

