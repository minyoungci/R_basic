# 조절효과분석 

str(mtcars)

mtcars.lm <- lm(mpg ~ hp+ wt+ hp:wt, data=mtcars) 
# hp:wt > 마력과 무게의 상호작용항 

summary(mtcars.lm)
# hp:wt 유의수준 0.05 에서 통꼐적으로 유의 - 연비와 마력간의 관계 패턴은 무게에
# 따라서 변동 

round(mean(mtcars$wt),1)
round(sd(mtcars$wt),1)

library(effects)

m <- round(mean(mtcars$wt),1) # 평균
s <- round(sd(mtcars$wt),1) # 표준편차 

m;s

plot(effect(term ="hp:wt", mod=mtcars.lm , 
            xlevels=list(wt=c(m-s, m, m+s))),
     lines = list(multiline=TRUE, lwd=2,
                  lty=c(3, 2, 1),
                  col=c("royalblue", "violet", "maroon")),
     main="Interaction Plot for Horsepower and Weight")

# term = 모델에서 시각화 할 부분의 포뮬러 지정 

library(rockchalk)

plotSlopes(model=mtcars.lm, plotx="hp", modx="wt",
           modxVals = "std.dev.", col=rainbow(3),
           main="Interaction Plot for Horsepower and Weight")
