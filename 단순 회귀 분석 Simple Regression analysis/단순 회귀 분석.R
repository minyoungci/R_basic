library(car)
str(Prestige)

head(Prestige)

# 회귀분석 

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm) # lm 객체 
Prestige.lm

# 절편 = -2853.6 기울기 = 898.8 

plot(Prestige$income ~ Prestige$education,
     col = 'cornflowerblue', pch=19, 
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")

# 회귀식을 산점도에 나타내기
abline(Prestige.lm, col="salmon", lwd=2)

# 기울기 > 독립변수인 교육이 1년 증가할 때마다 소득 898.8 증가
# 절편은 독립변수가 값이 0 일때, 종속변수의 값 

summary(Prestige.lm)

#Residuals : 잔차의 값 = 관측값과 예측값의 차 , 잔차의 평균 = 0
# Coefficients : 회귀계수에 대한 유의성 검정 결과 
# Estimate = 추정치 , std.error = 표준오차 , t value = t 값 , pr = 유의확률(p값)

# Residual standard error = 잔차 표준오차 (RSE) = 독립변수로부터 종속변수 예측할 때 발생하는 잔차의 표준ㅊ편차 
# RSE = 회귀선을 중심으로 상하로 변동하는 관측값의 평균 변동성  , 모델에 의해서 표현되지 않는 값을 보여줌
# 작은 값을 가질수록 모델의적합도가 좋음 
# Multiple R-square = 회귀모델의 설명력 , R-square 값이 클수록 설명력 높음 
# F-statistic = 회귀식의 유의성을 검정결과 확인. 
# p-value = 매우 작은 값은 통계적으로 매우 유의 ( 0.05 보다 낮음) 

# 회귀계수 부분에 대한 결과를 테이블 형식으로 출력 
coef(summary(Prestige.lm))

# 회귀모델에 대한 분산분석표 
anova(Prestige.lm)

coef(Prestige.lm)

confint(Prestige.lm) # 신뢰구간 ( 디폴트 95% )
confint(Prestige.lm, level =0.99) # 신뢰구간 ( 99% )

# 회귀식에 대한 예측값 벡터로 반환 
fitted(Prestige.lm)[1:3]
resid(Prestige.lm)[1:3]

Prestige$income[1:3] # 실제 관측값 

# resid 함수 = 실제 관측값과 예측값(fitted 함수) 간의 차이 

Prestige.new <- data.frame(education=c(5, 10, 15))
Predict(Prestige.lm, newdata=Prestige.new) # 예측
Predict(Prestige.lm, newdata=Prestige.new, interval = "confidence") # 95% 신뢰구간 

mean(Prestige$education)

lm(income ~ education, data=Prestige, 
   subset = (education > mean(Prestige$education)))
# 교육기관이 평균보다 큰 집단만 회귀 분석 수행 

lm(income ~ education, data=Prestige, 
   subset = (education <= mean(Prestige$education)))
# 교육기관이 평균보다 작은 집단만 회귀 분석 수행  


mydata <- read.csv("https://www.theissaclee.com/ko/courses/rstat101/examscore.csv", header = TRUE)

with(mydata,
     plot(midterm, final, asp=1,
          xlab="중간고사",
          ylab="기말고사",
          main="시험점수 분포"))

result <- lm(final ~ midterm, data=mydata)
attributes(result)

result$coefficients

plot(mydata$midterm, mydata$final, asp=1,
     xlab="midterm",
     ylab='final',
     main="plotting test")

abline(result$coefficients)
summary(result)






