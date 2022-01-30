str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data=InsectSprays)
summary(sprays.lm)

# A가 기준범주 ( 첫번쨰)

contrasts(InsectSprays$spray)
# dummy변수의 구조 파악

# ex. sprayC 의 회귀계수는 sprayC 의 평균 생존 수 - sprayA의 평균 생존 수 

sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov)
TukeyHSD(sprays.aov)

respray <- relevel(InsectSprays$spray, ref=6)
contrasts(respray)

sprays.lm <- lm(count ~ spray, data=InsectSprays)
summary(sprays.lm)


install.packages("fastDummies")
