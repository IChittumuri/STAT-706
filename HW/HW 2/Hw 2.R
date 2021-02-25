library(faraway)
library(tidyverse)

data("swiss")
?swiss
swiss <- swiss[complete.cases(swiss),]
lmod <- lm(Fertility ~ Agriculture, swiss)

## 1 
coef(lmod)
# beta_0 is 60.3043752, beta_1 is .01942017

a <- sum(((swiss$Agriculture) - mean(swiss$Agriculture)) * 
           ((swiss$Fertility) - mean(swiss$Fertility)))
b <- sum((mean(swiss$Agriculture) - (swiss$Agriculture))^2)
b_1 <- a/b; b_1

b_0 <- (mean(swiss$Fertility) - b_1*mean(swiss$Agriculture)); b_0

## 3
plot(lmod, which = 2) 

ggplot(swiss, aes(Agriculture, Fertility)) + geom_point() + geom_smooth(method = lm)

# 4
confint(lmod, level = 0.89)
 
#5 
lmod1 <- lm(Fertility ~ Examination, swiss)
lmod2 <- lm(Fertility ~ Education, swiss)
coef(lmod1)
coef(lmod2)

#Q-Q plot
plot(lmod1, which = 2)
plot(lmod2, which = 2)

# R^2
lmodsum <- summary(lmod)
lmodsum$r.squared
lmodsum1 <- summary(lmod1)
lmodsum1$r.squared
lmodsum2 <- summary(lmod2)
lmodsum2$r.squared

# AIC
AIC(lmod)
AIC(lmod1)
AIC(lmod2)



