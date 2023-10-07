#tai cac packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")

#tai cac libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(haven)

#doc file regression
df <- read_dta("Regression.dta")
View(df)

#phan tich tuong quan
cov(df$c38, df$wage, method ="pearson")
cor(df$c38, df$wage, method ="pearson")
plot(df$c38, df$wage, xlab = "c38", ylab = "wage")
abline(lm(wage~c38, data= df), col = "Red")

#linear regression 1 sample
reg1 <- lm(wage ~ c38, data = df)
reg1
model1 <- lm(df$wage ~ df$c38)
summary(model1)

#linear reg for multiple 
dt = sort(sample(nrow(regression),nrow(regression)*.5))
df1 <- regression[dt,]
df2 <- regression[-dt,]

dim(df1)
dim(df2)

model2 <- lm(df1$wage ~ df1$c38)
model3 <- lm(df2$wage ~ df2$c38)
plot(df1$c38, df1$wage, xlab = "c38", ylab = "wage")
abline(lm(wage~c38, data= df1), col = "Blue")
plot(df2$c38, df2$wage, xlab = "c38", ylab = "wage")
abline(lm(wage~c38, data= df2), col = "Green")