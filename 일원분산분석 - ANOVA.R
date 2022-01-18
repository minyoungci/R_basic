# 일원분산분석 (one-way ANOVA)

# 집단을 구분하는 독립변수가 한 개일때 모집단 간 평균의 동일성을 검정 
# 귀무가설 : 집단 간 평균은 모두 동일하다 
# 대립가설 : 집단 간 평균은 모두 동일하지 않다. 
# 어느 한 집다닝라도 다른 집단과 통계적으로 유의한 차이를 보인다면 귀무가설은 기각됌

str(InsectSprays)
InsectSprays

tapply(InsectSprays$count, InsectSprays$spray, length) #갯수 
tapply(InsectSprays$count, InsectSprays$spray, mean) # 평균
tapply(InsectSprays$count, InsectSprays$spray, sd) # 표준편차

# step1. 평균과 표준편차의 크기가 큼 

install.packages("windows")
library(gplots)


window(width=12, height=8)
plotmeans(count ~ spray, data=InsectSprays)
plotmeans(count ~ spray, data=InsectSprays, 
          barcol="tomato", barwidth=3,
          col = "cornflowerblue", lwd = 2,
          xlab="Type of Sprays", ylab="Insect Count",
          main = "performance of Insect Sprays")

boxplot(count ~ spray, data=InsectSprays, col="tomato", 
        xlab="Type of Sprays", ylab="Insect Count",
        main = "performance of Insect Sprays")


sprays.aov <- aov(count ~ spray, data=InsectSprays)
sprays.aov

summary(sprays.aov)
# 자유도, 제곱합 , 집단간 분산 (밑)집단내 분산, F값, F값보다 큰 확률
# 귀무가설 기각 

model.tables(sprays.aov, type="mean")
model.tables(sprays.aov, type="effects") # 각 집단평균과 전체평균의 차이
# c가 살충효과 베스트, f가 살충효과 워스트 

sprays.compare<-TukeyHSD(sprays.aov)
sprays.compare
str(sprays.compare)
sprays.compare$spray['D-C',]
# p 값은 D와 C값의 평균차이 , 0.05보다 크기 때문에 통계적으로 무의미
# 살충 효과 xx 

plot(sprays.compare)
plot(sprays.compare, col="blue", las=1)
# 그래프에서 신뢰구간이 0을 포함하지 않는다면 유의미 
# 신뢰구간 0을 포함 : 0이 될 수도 있다. > 두 집단간의 평균이 같다.

install.packages("multcomp")
library(multcomp) 

tuk.hsd <- glht(model=sprays.aov, linfct=mcp(spray="Tukey"))
cld(tuk.hsd, level=0.05) # 같은 알파벳 = 평균이 같음
# A=B=F, C=D=E

plot(cld(tuk.hsd, level=0.05), col="orange", las=1)




# 분산 분석 가정 
# 정규성(normality) : 종속변수는 정규분포를 함.
# 등분산성(homoscedasticity, equality of variance) : 각 집단의 분포는 
# 모두 동일한 분산을 가짐. 

library(car)
qqplot(InsectSprays$count,id=FALSE, pch=20, col="deepskyblue",
       xlab="Theretical Quantiles", ylab="Empirical Quantitles",
       main="Q-Q plot")

shapiro.test(InsectSprays$count)

outlierTest(sprays.aov)

leveneTest(count ~ spray, data=InsectSprays)
bartlett.test(count ~ spray, data = InsectSprays)

oneway.test(count ~ spray, data=InsectSprays)
summary(aov(count ~ spray, data=InsectSprays))

oneway.test(count ~ spray, data=InsectSprays,
            var.equal = TRUE)

 