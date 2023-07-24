# 가설 검정과 확률 분포

# 이항분포의 확률밀도함수
# 사건이 발생할 횟수(숫자면이 나오는 경우) , 시행횟수 , 확률
dbinom(7, size=10, 0.5) 

# 동전을 10번 던졌을 때 숫자가 7번 이하가 나타날 누적 확률 계산
pbinom(4, size = 10, prob = 0.5)
sum(dbinom(0:7, size=10, 0.5))

dbinom(10, 10, 0.5)

# 누적확률의 반대 
pbinom(7, size = 10, prob = 0.5, lower.tail = FALSE
  )

rbinom(3, size = 10, 0.5)

# 정규분포의 확률분포 함수

pnorm(110, mean = 100, sd = 10) # 110 미만일 확률 
pnorm(110, mean = 100, sd = 10, lower.tail = FALSE) # 110을 초과할 확률

pnorm(0) 
pnorm(0, mean=0, sd=1)

pnorm(110, mean=100, sd=10) - pnorm(90, mean=100, sd=10)

qnorm(0.05, mean=100, sd=10) 
# 누적확률 5%이하에 대응되는 관측값 (IQ가 83이하인 사람들이 전체에서 5%정도 차지하고 있다는 의미 )

qnorm(c(0.05, 0.95), mean=100, sd=10)
# qnorm ( 정규분포의 백분위수 함수)는 신뢰구간을 구할 때 사용할 수 있다. 

# 평균 0 표준편차 1인 표준정규분포에서 표준 점수의 95% 신뢰구간을 계산
qnorm(0.025)
qnorm(0.975)
# -1.95 ~ 1.95 가 신뢰구간 

# 정규분포로부터 난수 생성
set.seed(1)
rnorm(1 , mean=100, sd=10)
rnorm(5 , mean=100, sd=10)

rnorm(6,mean=c(-10, 0,10), sd=1 )

set.seed(123)

shapiro.test(rnorm(100, mean=100, sd=10)) # 정규분포로부터 추출한 데이터 100개 
shapiro.test(runif(100, min=2, max=4)) # 이량분포로부터 추출한 데이터 100개


set.seed(123)
qqnorm(runif(100, min=2, max=4),
       col="red",
       main = "Simple from Normal Distribution")

qqline(runif(100, min=2, max=4))
 