str(mtcars)
mtcars <- mtcars[c("mpg", "hp","wt","disp","drat")]

head(mtcars)

summary(mtcars)
cor(mtcars)

# 중위수가 평균보다 오른쪽에 있는 경우 대체로 왼쪽으로 꼬리가 긴 분포 

library(car)

scatterplotMatrix(mtcars, pch=19, col = "royalblue", cex=1.2,
                  regLine = list(method=lm, lty=1, lwd=3, col='salmon') ,
                  smooth =list(smoother=loessLine, spread=FALSE,
                               lty.smooth=1,
                               lwd.smooth=3,
                               col.smooth="forestgreen"),
                  main = " Car Performance")

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)

#Multiple R-squared:  0.8376,	Adjusted R-squared:  0.8136 
# Adjusted R-squared:  0.8136  >> 수정된 R-squared 
# : 모델이 포함된 예측변수의 갯수를 포함. 
# 모델이 포함된 예측변수의 개수만큼 보정. 과적합 방지

# 변수의 표준화 

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)

mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) +
                  scale(drat), data=mtcars) 
summary(mtcars.lm)


install.packages("QuantPsyc")
library(QuantPsyc)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
lm.beta(mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data= mtcars))
