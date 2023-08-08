library(MASS)
str(cats)

# 고양이의 몸무게와 심장무게 산점도로 관계 파악하기 

plot(cats$Hwt ~ cats$Bwt,
     col="forestgreen", pch=19,
     xlab='Body Weight (kg)', ylab="Heart Weight(g)",
     main = "Body Weight and Heart Weight of Cats")

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))

?cor

cor.test(cats$Bwt, cats$Hwt)

cor.test(cats$Bwt, cats$Hwt, alternative = "greater", conf.level = 0.99)

cor.test(~ Bwt + Hwt , data=cats) # formula 형식을 이용하면 subset별로 함수를 적용할 수 있는 장점 

cor.test(~ Bwt + Hwt , data=cats, subset = (Sex=="F"))


str(iris)
cor(iris[-5])

iris.cor <- cor(iris[-5])
iris.cor["Petal.width", 'Petal.Length']

# 2개를 초과하는 상관계수 
library(psych)
corr.test(iris[-5])

print(corr.test(iris[-5]), short=FALSE)


str(state.x77)

cor(state.x77)

pairs.panels(state.x77, pch=21, bg="red", hist.col="gold",
             main="Correlation Plot of US States")


library(corrgram)

corrgram(state.x77, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt,
         order=TRUE, main = "Corrgram of US States")



# 편상관계수 

str(mtcars)

mtcars2 <- mtcars[, c('mpg', 'cyl','hp','wt')]
mtcars2

cor(mtcars2)

library(ggm)

pcor(c(1,3,2,4), cov(mtcars2))

library(ppcor)
pcor(mtcars2)

pcor.test(mtcars2['mpg'], mtcars2['hp'])
