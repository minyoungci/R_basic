library(psych)
str(sat.act) # dataset , 학습 능력을 평가하는 지표(ACT, SATV, SATQ)

# ACT - 전체 모든 점수 
# SATV , SATQ - 언어점수, 수학점수 

ability <- sat.act[c("ACT", "SATV", "SATQ")]
head(ability)

# alpha함수를 이용하여 크론바흐 알파계수 구하자
# alpha의 데이터 프레임 혹은 행렬을 인수로 제공
alpha(ability)

alpha(ability)
alpha(ability[c(2,3)])            
