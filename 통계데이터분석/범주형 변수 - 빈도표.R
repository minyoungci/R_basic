# 빈도표 frequency table , 교차표 cross-tabulation , 다차원 테이블 

library(MASS)
str(survey) # 12개의 변수 , Factor type은 범주형 변수 의미 

levels(survey$Smoke) 

frqtab <- table(survey$Smoke) 
frqtab
class(frqtab)

frqtab[2]

frqtab == max(frqtab)
frqtab[frqtab==max(frqtab)]

names(frqtab[frqtab==max(frqtab)])

which.max(frqtab)
names(frqtab[which.max(frqtab)])

frqtab.prop <- prop.table(frqtab)
frqtab.prop['Never'] * 100

a <- survey$Smoke == 'Never' # 담배를 피우지 않는 사람 
mean(a, na.rm=TRUE)

head(anorexia)
anorexia$Postwt > anorexia$Prewt
mean(anorexia$Postwt > anorexia$Prewt)

head(mammals)

abs(mammals$brain - mean(mammals$brain))

head(SP500)
str(SP500)

diff(SP500) # diff 함수는 주어진 벡터로부터 연속된 두 숫자의 차이 계산
diff(SP500) > 0
mean(diff(SP500) > 0)

# 교차표 

install.packages("vcd")
library(vcd)
str(Arthritis)

levels(Arthritis$Treatment)
levels(Arthritis$Improved)

crosstab <- table(Arthritis$Improved, Arthritis$Treatment)
crosstab
crosstab["Marked", "Treated"]

