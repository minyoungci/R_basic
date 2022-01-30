str(mtcars)

# 변속기의 유형에 따라서 배기량과 연비간의 자동차 무게에 대한 매개효과가 달라지는지?
# 매개효과와 조절효과 동시 분석 

model.M <- lm(wt ~ disp * am, data=mtcars) #매개변수를 종속변수로, am=상호작용항
model.M <- lm(wt ~ disp + am + disp:am , data=mtcars) # 상호작용을 나타내는 ':' 연산자 이용
model.Y <- lm(mpg ~ disp * am + wt * am, data=mtcars)
model.Y <- lm(mpg ~ disp + am + wt * disp:am + wt:am, data=mtcars)

library(mediation)
set.seed(12)

# 자동 변속 기어 
model.med1 <- mediate(model.m = model.M,
                      model.y = model.Y,
                      covariates = list(am=0),
                      treat = "disp",
                      mediator = "wt",
                      boot = TRUE, sims= 500)
summary(model.med1)

# 수동 변속 기어 
model.med2 <- mediate(model.m = model.M,
                      model.y = model.Y,
                      covariates = list(am=1),
                      treat = "disp",
                      mediator = "wt",
                      boot = TRUE, sims= 500)
summary(model.med2)

set.seed(12)
model.med <- mediate(model.m=model.M,
                     model.y= model.Y,
                     treat="disp", mediator="wt",
                     boot = TRUE, sims=500)
set.seed(12)
test.modmed(object=model.med,
            covariates.1=list(am=0), covariates.2 =(am=1),
            sims=500)

