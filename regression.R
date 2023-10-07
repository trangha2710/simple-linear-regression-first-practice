#tai cac packages
install.packages("ggplot2")
install.packages("dplyr")

#tai cac libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)

#doc file regression
df <- read_dta("Regression.dta")
View(df)

#phan tich tuong quan
cov(df$c38, df$wage, method ="pearson")
cor(df$c38, df$wage, method ="pearson")


#linear regression 1 sample
reg1 <- lm(wage ~ c38, data = df)
summary(reg1)
plot(df$c38, df$wage, xlab = "c38", ylab = "wage", col = "blue")
abline(reg1, col = "Red")

#phan tich hoi quy cho tung mau
dt = sort(sample(nrow(df),nrow(df)*.5))
df1 <- df[dt,]
df2 <- df[-dt,]

dim(df1)
dim(df2)

reg2 <- lm(df1$wage ~ df1$c38)
reg3 <- lm(df2$wage ~ df2$c38)

#danh gia cac tham so cho tung mo hinh hoi quy mau
summary(reg2)
summary(reg3)

plot(df$c38, df$wage, xlab = "c38", ylab = "wage", col = "blue")
abline(reg1, col = "Red")
abline(reg2, col = "Yellow")
abline(reg3, col = "Green")
