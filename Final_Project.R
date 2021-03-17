library(tidyverse)
library(skimr)
library(GGally)
library(readxl)
library(dplyr)
library(memisc)
library(psych)
library(lmtest)
library(sjPlot)
library(sgof)
library(foreign)
library(ggplot2)
library(hexbin)
library(vcd)
library(devtools)
library(knitr)
library(car)

my_data = read_excel("Final_Data.xlsx", col_names = TRUE)
names(my_data) = c("Date", "GDP", "SP", "FiveSp", "OneSp", "BC", "CU", "UR", "BCf", "HP", "EAI", "AS", "CPI", "CIL", "MT")

pct_change = function(x) {x/lag(x)-1}
my_data = mutate_each(my_data, funs(pct_change), GDPG = "GDP", SP_pct = "SP", CPI_pct = "CPI", EAI_pct ="EAI", BCf_pct = "BCf", AS_pct = "AS", MT_pct = "MT", CIL_pct = "CIL")

my_data = na.omit(my_data)
glimpse(my_data)
str(my_data)

qplot(my_data$GDPG)
ggplot(my_data, aes(x=GDPG)) + geom_density()
ggpairs(my_data)

# first approach of Step down regression, where we remove most insignificant independent variable (highest p value/smallest absolute t value) in each iteration untill all the independent variables are significant.
my_data = subset(my_data, select = -c(GDP, Date))

#scaling the data
my_data =  as.data.frame(scale(my_data))
my_data_2 = my_data
glimpse(my_data)

# Univariate analysis------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
univariate_model_0 = lm(data = my_data, GDPG~SP)
summary(univariate_model_0)
dwt(univariate_model_0)

univariate_model_1 = lm(data = my_data, GDPG~HP)
summary(univariate_model_1)
dwt(univariate_model_1)

univariate_model_2 = lm(data = my_data, GDPG~MT)
summary(univariate_model_2)
dwt(univariate_model_2)

univariate_model_3 = lm(data = my_data, GDPG~AS)
summary(univariate_model_3)
dwt(univariate_model_3)

univariate_model_4 = lm(data = my_data, GDPG~CPI)
summary(univariate_model_4)
dwt(univariate_model_4)

univariate_model_5 = lm(data = my_data, GDPG~FiveSp)
summary(univariate_model_5)
dwt(univariate_model_5)

univariate_model_6 = lm(data = my_data, GDPG~OneSp)
summary(univariate_model_6)
dwt(univariate_model_6)

univariate_model_7 = lm(data = my_data, GDPG~BC)
summary(univariate_model_7)
dwt(univariate_model_7)

univariate_model_8 = lm(data = my_data, GDPG~CIL)
summary(univariate_model_8)
dwt(univariate_model_8)

univariate_model_9 = lm(data = my_data, GDPG~CU)
summary(univariate_model_9)
dwt(univariate_model_9)

univariate_model_10 = lm(data = my_data, GDPG~UR)
summary(univariate_model_10)
dwt(univariate_model_10)

univariate_model_11 = lm(data = my_data, GDPG~BCf)
summary(univariate_model_11)
dwt(univariate_model_11)

univariate_model_12 = lm(data = my_data, GDPG~EAI)
summary(univariate_model_12)
dwt(univariate_model_12)

univariate_model_13 = lm(data = my_data, GDPG~SP_pct)
summary(univariate_model_13)
dwt(univariate_model_13)

univariate_model_14 = lm(data = my_data, GDPG~CPI_pct)
summary(univariate_model_14)
dwt(univariate_model_14)

univariate_model_15 = lm(data = my_data, GDPG~EAI_pct)
summary(univariate_model_15)
dwt(univariate_model_15)

univariate_model_16 = lm(data = my_data, GDPG~BCf_pct)
summary(univariate_model_16)
dwt(univariate_model_16)

univariate_model_17 = lm(data = my_data, GDPG~AS_pct)
summary(univariate_model_17)
dwt(univariate_model_17)

univariate_model_18 = lm(data = my_data, GDPG~MT_pct)
summary(univariate_model_18)
dwt(univariate_model_18)

univariate_model_19 = lm(data = my_data, GDPG~CIL_pct)
summary(univariate_model_19)
dwt(univariate_model_19)

comparison = mtable(univariate_model_0,univariate_model_1,univariate_model_2,univariate_model_3,univariate_model_4,univariate_model_5,univariate_model_6,univariate_model_7,univariate_model_8,univariate_model_9,univariate_model_10,univariate_model_11,univariate_model_12,univariate_model_13,univariate_model_14,univariate_model_15,univariate_model_16,univariate_model_17,univariate_model_18,univariate_model_19)
comparison

#Initial step of Step-down regression approach-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_0 = lm(data = my_data, GDPG~.)
summary(model_0)
dwt(model_0)

#we remove CIL_pct
my_data = subset(my_data, select = -c(CIL_pct))
model_1 = lm(data = my_data, GDPG~.)
summary(model_1)
dwt(model_1)

#we remove OneSp
my_data = subset(my_data, select = -c(OneSp))
model_2 = lm(data = my_data, GDPG~.)
summary(model_2)
dwt(model_2)

#we remove SP
my_data = subset(my_data, select = -c(SP))
model_3 = lm(data = my_data, GDPG~.)
summary(model_3)
dwt(model_3)

#we remove CU
my_data = subset(my_data, select = -c(CU))
model_4 = lm(data = my_data, GDPG~.)
summary(model_4)
dwt(model_4)

#we remove SP_pct
my_data = subset(my_data, select = -c(SP_pct))
model_5 = lm(data = my_data, GDPG~.)
summary(model_5)
dwt(model_5)

#we remove EAI
my_data = subset(my_data, select = -c(EAI))
model_5 = lm(data = my_data, GDPG~.)
summary(model_5)
dwt(model_5)

#we remove MT_pct
my_data = subset(my_data, select = -c(MT_pct))
model_6 = lm(data = my_data, GDPG~.)
summary(model_6)
dwt(model_6)

#we remove AS
my_data = subset(my_data, select = -c(AS))
model_7 = lm(data = my_data, GDPG~.)
summary(model_7)
dwt(model_7)

#we remove BC
my_data = subset(my_data, select = -c(BC))
model_8 = lm(data = my_data, GDPG~.)
summary(model_8)
dwt(model_8)

#we remove HP
my_data = subset(my_data, select = -c(HP))
model_9 = lm(data = my_data, GDPG~.)
summary(model_9)
dwt(model_9)
#good model @5$ significance level. Named as model A in presentation

#we remove FiveSp  @0.1% significance level
my_data = subset(my_data, select = -c(FiveSp)) 
model_10 = lm(data = my_data, GDPG~.)
summary(model_10)
dwt(model_10)

#we remove BCf
my_data = subset(my_data, select = -c(BCf))
model_11 = lm(data = my_data, GDPG~.)
summary(model_11)
dwt(model_11)   #named as model B in presentation

#custom made model from step-up regression 
model_12 = lm(data = my_data_2, GDPG ~ MT + CPI_pct + EAI_pct + BCf_pct + AS_pct)
summary(model_12)
dwt(model_12)   #named as model C in presentation

#Waldtest to decide the best model---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

waldtest(model_12, model_11)
waldtest(model_9, model_11)
waldtest(model_12, model_9)
mtable(model_9, model_11, model_12)

#model_11 is the best among the 3 models using waldtests (also model_11 is preferred as it has less variables than model_9)

#Backtesting the models using r2 scores by splitting the data into train_data and test_data (3:1 ratio)
train_data = my_data_2[1:125,]
glimpse(train_data)
test_data = my_data_2[125:166,]
glimpse(test_data)

#function to claculate r2 score between original and predicted values
rsq <- function(x, y) summary(lm(y~x))$r.squared


#model_9 (model A)
train_model_9 = lm(data = train_data, GDPG ~ FiveSp + UR + BCf + CPI + CIL + MT + CPI_pct + EAI_pct + BCf_pct + AS_pct)
predicted_values_9 = predict(train_model_9, test_data)
model_9_r2_score = rsq(test_data$GDPG, predicted_values_9)
model_9_r2_score
plot_model(model_9)

#model_11 (model B)
train_model_11 = lm(data = train_data, GDPG ~ UR + CPI + CIL + MT + CPI_pct + EAI_pct + BCf_pct + AS_pct)
predicted_values_11 = predict(train_model_11, test_data)
model_11_r2_score = rsq(test_data$GDPG, predicted_values_11)
model_11_r2_score
plot_model(model_11)

#model_12 (model C)
train_model_12 = lm(data = train_data, GDPG ~ MT + CPI_pct + EAI_pct + BCf_pct + AS_pct)
predicted_values_12 = predict(train_model_12, test_data)
model_12_r2_score = rsq(test_data$GDPG, predicted_values_12)
model_12_r2_score
plot_model(model_12)
#model_12 consistently provides better r2 scores than model_9 and model_11 (for multiple test-train splits)
#However, using Waldtest -> model 11 (model B) is better


