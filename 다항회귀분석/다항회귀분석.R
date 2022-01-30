library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
summary(Prestige.lm)

plot(Prestige$income ~ Prestige$education,
     col = 'cornflowerblue', pch=19,
     xlab="Education (year)", ylab='Income(dollars)',
     main="Education and Income")

abline(Prestige.lm, col="salmon", lwd=2)

lm(income ~ education, data=Prestige,
   subset=(education > mean(Prestige$education)))
# 교육기간이 평균보다 긴 집단 회귀모델

lm(income ~ education, data=Prestige,
   subset=(education <= mean(Prestige$education)))
# 교육기간이 평균보다 짧은 집단 희귀모델 

scatterplot(income ~ education, data=Prestige,
            pch=19, col="orangered", cex=1.0,
            regLine=list(method=lm, lty=2, lwd=3, col="royalblue"),
            smooth=list(smoother=loessLine, spread=FALSE,
                        lty.smooth=1, lwd.smooth=3, col.smooth='green3'),
            xlab="Education (year)", ylab='Income(dollars)',
            main="Education and Income")

Prestige.poly <- lm(income ~ education + I(education^2), data=Prestige)
summary(Prestige.poly)
# I 함수는 ()안에있는 수치를 있는 그대로 이용

# RSE 값은 작을수록 좋고 R-square 값은 클수록 좋음.
# 단항회귀 분석보다 다항회귀 분석의 서능이 더 높은 것을 확인.

plot(Prestige$income ~ Prestige$education,
     col = 'darkorange', pch=19,
     xlab="Education (year)", ylab='Income(dollars)',
     main="Education and Income")
library(dplyr)

lines(arrange(data.frame(Prestige$education, fitted(Prestige.poly)),
              Prestige$education), col="cornflowerblue") 

str(faithful)

scatterplot(eruptions ~ waiting, data=faithful,
            pch=19, col="deepskyblue", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="blueviolet"),
            smooth=list(smoother=loessLine, spread=FALSE,
                        lty.smooth=1, lwd.smooth=3, col.smooth='green3'),
            xlab="Waiting (minutes)", ylab='Eruptions(minutues)',
            main="Waiting Time Between Eruptions and the Duration of the Eruptions")

faithful.poly <- lm(eruptions ~ waiting + I(waiting^2)+ I(waiting^3), data=faithful)
summary(faithful.poly)

faithful.lm <- lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm)
 

