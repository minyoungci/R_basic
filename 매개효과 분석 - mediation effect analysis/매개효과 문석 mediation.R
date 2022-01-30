str(mtcars)

# 매개분석 1단계 
model.total <- lm(mpg ~ disp, data=mtcars)
summary(model.total)
# 유의수준 0.05에 비해 매우 낮으므로 배기량은 연비에 유의한 의미 = 독립변수가 통계적으로 유의
# 배기량이 연비에 미치는 총효과는 독립변수의 회귀계수인 -0.04 

# 매개분석 2 단계 
model.M <- lm(wt ~ disp, data=mtcars)
summary(model.M)
# 유의수준 0.05에 비해 매우 낮으므로 무게는 연비에 유의한 의미 = 독립변수가 통계적으로 유의

# 3단계 - 독립변수를 통제한 상테에서 매개변수와 종속변수간의 영향관계 검정 
model.Y <- lm(mpg ~ disp+wt, data=mtcars)
summary(model.Y)
# wt - 유의확률 0.007 통계적 유의
# disp - 유의확률 0.06 통계적으로 유의 x 
# 배기량과 연비간의 직접 효과는 모집단에서 존재 x 

# 1단계 모델에서는 배기량과 연비간의 관계는 통꼐적으로 유의, 총효과 존재 
# 3단계는 모델에서는 무게(wt)라는 매개변수의 투입으로 배기량과 연비간의 직접효과가 사라짐.
# 자동차 무게는 배기량과 연비를 완전 매개한다고 해석 가능 

# 간접효과(배기량과 연비) - 독립변수와 매개변수간의 회귀계수와 매개변수와 종속변수간의 회귀계수의 
# 곱으로 가능 

# 2단계 모델 - 종속변수(자동차 무게), 독립변수(배기량)의 회귀꼐수 = 0.007 
# 3단계 모델 - 매개변수(연비),  종속변수(자동차 무게)의 회귀계수 = -3.351

0.007 * ( -3.351)

# 통계적 유의성 검정 - 소벨검정, 부트스트레핑 검정 

# 소벨 검정 - 매개변수 m이 존재할 때 독립변수 x가 종속변수y에 미치는 영향이 
# 통계적으로 유의하게 감소하는지 검정 


 library(multilevel)

model.sob <- sobel(pred=mtcars$disp , med =mtcars$wt ,out = mtcars$mpg)
model.sob
# z 값을 이용하여 간접효과에 대한 p값 계산 

pnorm(abs(model.sob$z.value), lower.tail = FALSE) # 오른쪽 
pnorm(abs(model.sob$z.value), lower.tail = FALSE)* 2 # 오른쪽 + 왼쪽 


library(bda)
  
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

# 부트스트래핑 분석 - 매개분석 


library(mediation)

# 두 개의 회귀모델 - 매개변수 모델, 종속변수 모델 
# 매개변수 모델 - 매개변수를 종속변수로 사용 
# 종속변수 모델 - 기존의 종속변수를 종속변수로 사용하고 독립변수와 매개변수를 독립변수로 사용

model.M <- lm(wt ~ disp, data=mtcars)
model.Y <- lm(mpg ~ disp+wt, data=mtcars)
set.seed(123) # 동일한 결과를 위한 시드번호 

model.mediation <- mediate(model.m=model.M, 
                           model.y=model.Y,
                           treat="disp",
                           mediator = "wt",
                           boot=TRUE, 
                           sims = 500)
# 독립변수 = treat , 매개변수 = mediator
# sims = 추출할 표본의 개수 

summary(model.mediation)
# total effect - 총효과 - 바론앤캐니 1단계 회귀 모델에서 독립변수가 종속변수에 
# 미치는 영향을 나타냄 

# ADE - average direct effect (직접효과) - 바론앤캐니 3단계 회귀모델에서 종속변수에 대한 독립변수의
# 영향을 나타냄 .

# ACME - average closer mediation effect ( 간접효과, 매개효과)
# 총효과에서 직접 효과를 빼서 계산   

# 매개효과분석의 목표 - 간접효과를 추정해서 이에 대한 통꼐적 유의성 검정.
# 

plot(model.mediation, cex = 1.2, col="royalblue", lwd=2,
     main = "Mediation Effect Analysis")

