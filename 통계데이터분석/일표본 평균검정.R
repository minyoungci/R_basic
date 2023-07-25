# 일표본 평균검정 

library(MASS)
str(cats)

# 고양이의 성별, 몸무게(kg), 심장무게(g)

# 고양이의 몸무게 평균을 2.6kg으로 가정 , 대립 가설 : 고양이의 몸무게는 2.6kg이 아니다.

# 일표본 평균 
t.test(x=cats$Bwt, mu=2.6) # 고양이의 몸무게 , 검정하고자 하는 고양이 무게의 평균 

# 144개의 관측값으로부터 관측된 고양이 몸무게의 평균 : mean of x = 2.7
# 표본 평균에 대응되는 t값은 3.0565, 자유도인 df = 143, p-value=0.002673
# 귀무가설은 2.6kg이 아니다 이므로 양측검정을 수행해야함. 
# = t값은 -3.05보다 작거나 , 3.05보다 커야함. 
# alternative hypothesis: true mean is not equal to 2.6 (대립가설에 해당)
# 95 percent confidence interval: 2.643669 2.803553 ( 95% 신뢰구간 )
# 귀무가설 기각 , 대립가설 채택 

t.test(x=cats$Bwt, mu=2.7)
# 평균을 2.7로 변경할 경우 귀무가설을 채택함 

# t.test 의 파라미터를 살펴보면 
# alternative 옵션을 통해 단측, 양측 검정을 설정할 수 있다. 


cats.t <- t.test(x=cats$Bwt, mu=2.6) # 가설검정 결과를 객체로 저장 
str(cats.t) # 결과는 리스트로 저장됌 

# 리스트로 저장되기 때문에 인덱싱으로 원하는 값을 얻을 수 있다.
cats.t$p.value
cats.t$conf.int

t.test(x=cats$Bwt, mu=2.6, conf.level = 0.99)

prop.test(x=18, n=30, p=0.5, alternative = 'greater')
