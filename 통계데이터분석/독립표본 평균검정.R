library(MASS)
str(cats)

t.test(formula=Bwt ~ Sex, data=cats) 
# formula = 검정하고자 하는 종속변수 ~ 집단을 나타내는 독립변수 
# 독립변수는 영향을 주는 변수 , 종속 변수는 영향을 받는 변수 
# 즉 위의 코드는 Sex에 따른 Bwt의 변화 


Bwt.f <- cats$Bwt[cats$Sex=="F"] # 암컷 고양이의 몸무게 
Bwt.m <- cats$Bwt[cats$Sex=="M"] # 수컷 고양이의 몸무게 

mean(Bwt.f)
mean(Bwt.m)

t.test(Bwt.f, Bwt.m)


# 집단간의 비율이 동일한지 ? 

# 예를 들어 폐질환자 대비 흡연자의 비율이 

patients <- c(86, 93, 136, 82)
smokers <- c(83, 90, 129, 70)

smokers/patients 

prop.test(x=smokers, n=patients) # 사건 발생할 횟수 , 전체 
