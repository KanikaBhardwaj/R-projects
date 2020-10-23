
# Here we are using the data insurance.csv for risk assessment in case of Health Insurance
# We have created two models namely Decision Tree and Random Forest
# Our accuracy of Random Forest is 1% more than Decision Tree.
# Both the models have accuracy more than 90%

#------------------ Loading packages ---------------------
install.packages("FSelector") # Entropy, Info gain are all parts of this package
install.packages("rpart") # For partitioning of the decision tree
install.packages("caret", dependencies = TRUE) # For splitting the data into train and test
install.packages("dplyr") # For filtering the data, EDA process
install.packages("rpart.plot") # plot the tree
install.packages("data.tree") # Displaying the data

library(FSelector)
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(data.tree)

#------------------------- Load the data insurance.csv ------------------------

insurance <- read.csv(file.choose(), header = TRUE)
View(insurance)

# ------------ Selecting only the meaningful columns for prediction (EDA process) ---------

insurance_new <- insurance[-6] # Only removing region column
View(insurance_new)
summary(insurance_new)
str(insurance_new)
any(is.na(insurance))
insurance_new$bmi <- as.integer(insurance$bmi)

# -------------- Converting Charges into categorical variable -------------------

Risk = ifelse(insurance_new$charges>=15000,"1","0") # if charges are greater than 15K then '1' in col Risk else '0'
insurance_new = data.frame(insurance_new, Risk)
head(insurance_new)

insurance_new <- insurance_new[-6] # Assumed that insurance of above 15k are risky i.e. 1 
head(insurance_new)
str(insurance_new)

# ------------------- Visualising the data -------------------

library(psych)
library(tidyverse)
library(corrplot)
library(knitr)
library(gridExtra)


plot(insurance_new)
insurance_boxplot<-insurance %>%
  select(c(1,3)) %>%
  gather()
boxplot<-ggplot(insurance_boxplot, aes(x=key, y=value)) +
  labs(x= "variable", title= "Insurance Daata Boxplot") +
  geom_boxplot(outlier.colour= "red", fill= "white", outlier.shape = 2)
boxplot
insurance_hist<- insurance %>%
  select(c(1,3,7)) %>%
  gather()
hist<- ggplot(data= insurance_hist, mapping = aes(x= value)) +
  geom_histogram(bins= 10, colour= "Orange", fill= "Red")+
  facet_wrap(~key,scales = 'free_x')
hist

#correlation between age, BMI, children and charges
pairs.panels(insurance[c("age","bmi","children","charges")])

#mutate_all() function in R creates new columns for all the available columns. Positive correlations are displayed in blue and negative correlations in red color.
Corr_ins<- mutate_all(insurance,
                      funs(as.numeric))
corrplot(cor(Corr_ins),method = "color", 
         type = "lower")

# We can see that the correlation between the indepandent variable is very less.
# This will give us good results in our RandomForest model. Low corr high accuracy

#-------------------- Splitting into training and testing data --------------------------

set.seed(123)

index_train <- sample(1:nrow(insurance_new), 2/3 * nrow(insurance_new)) #2/3 of dataset

insurance_train <- insurance_new[index_train, ]
insurance_test <- insurance_new[-index_train, ]

# ----------------- Decision Tree Model ------------------

# Training the Decision Tree Classifier

tree <- rpart(Risk ~.,data = insurance_train)

# Predictions

tree.risk.predicted <- predict(tree, insurance_test, type = 'class')

# Confusion matrix for evaluating the model

confusionMatrix(tree.risk.predicted, insurance_test$Risk) # accuracy = 92.38

# Visualizing the Decision Tree

prp(tree)


# ----------------- Random Forest Model ------------------

library(randomForest)

rf <- randomForest(Risk ~., data = insurance_train, ntree = 2000, importance = TRUE)

rf 
# accuracy
((641+192)/(641+192+54+5))*100 # 93.39%

plot(rf)

result <- data.frame(insurance_test$Risk, predict(rf,insurance_test[,1:6], type = 'response'))
result # here we can see out predicted v/s actual value of Risk 
plot(result)

# ------------------ Following are the acuuracy of our models ---------------------

# Decision Tree: 92.38%
# Random Forest: 93.39%