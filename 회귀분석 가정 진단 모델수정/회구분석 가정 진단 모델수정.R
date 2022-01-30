str(mtcars)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
plot(mtcars.lm)

# 종속변수가 독립변수와 선형 관계를 가지려면 잔차와 예측값 간에는 
# 어떠한 체계적 관계도 존재 x
# 예측 가능한 패턴 관측 x
# 회귀모델은 데이터에 존재하는 모든 체계적 변동성 포착 
# 모델에 의해서 설명이 안되는 무작위적인 오차만 남아야함.

library(car)
vif(mtcars.lm)

powerTransform(mtcars$mpg) #mpg 변수의 정규화 ( 람다화 )
summary(powerTransform(mtcars$mpg))

boxTidwell(mpg ~ hp + wt, data=mtcars)

spreadLevelPlot(lm(mpg ~ hp + wt, data=mtcars)) # 람다값의 추정치 = 0.585

