# 콕스회귀분석 

library(survival)
str(lung)


lung$sex <- factor(lung$sex, levels = c(1,2), 
                   labels=c("male", "female"))

# 생존함수를 위한 서브객체 생성 

# 생존시간, 사건발생상태, 
Surv(time=lung$time, event = lung$status)

class(Surv(time=lung$time, event = lung$status)) # 서브객체 
# 중도절단의 경우 +기호가 붙음. 

cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog , data=lung)
cox

summary(cox)


library(survminer)

ggforest(cox, data=lung)


cox.fit <- survfit(cox, data=lung)
cox.fit

ggsurvplot(cox.fit, palette = "cornflowerblue", ggtheme=theme_minimal(),
           legend="none", xlab="Days", ylab="Overall Survival Probability")


# 성별이 생존확률에 미치는 영향 구하기 

sex.df <- with(lung, 
               data.frame(sex=c("male", "female"), 
                          age=rep(mean(age,na.rm=TRUE)),
                          ph.ecog=rep(mean(ph.ecog,na.rm=TRUE))
               
               