library(MASS)
str(Boston)

# 주택가격 예측 

# dataset 분할 > caret 패키지

library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv , p=0.7, list = FALSE)
# y에는 결과변수 , p = 훈련데이터 비율(0.7) , 0.3은 테스트 데이터 
# list = 디폴트 TRUE = 
head(train)

Boston.train <- Boston[train, ] #훈련 데이터 
Boston.test <- Boston[-train, ] #테스트 데이터 
nrow(Boston.train)
nrow(Boston.test)
# 데이터 갯수 

install.packages("glmnet")
library(glmnet)
?glmnet
# x,y,family,alpha,lamda 필수 입력 

x <- model.matrix(medv ~ ., Boston.train) #model에 투입할 예측될 행렬 변수 
head(x)
x <- model.matrix(medv ~ ., Boston.train)[,-1] #첫번째 행 삭제 
y <- Boston.train$medv

# ridge regression analysis
set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family='gaussian', alpha=0) #k-묶음 교차검증 > 최적의 람다값 검출 

plot(Boston.cv)
# 가장 왼쪽에 있는 점선 = 최적의 람다값. > 예측오차 최소화 

str(Boston.cv)
#$ lambda.min: num 0.708

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

Boston.gnet <- glmnet(x=x, y=y, family='gaussian', alpha=0, lambda=Boston.cv$lambda.min)
# ridge 예측 모델 
# gaussian = 결과 변수가 연속형 변수인 표준적인 선형회귀분석 

coef(Boston.gnet) # ridge 회귀모델의 회귀계수 
# 1개의 절편과 13개의 회귀계수 

# test 데이터의 예측 변수 행렬 생성 
Boston.test.x <- model.matrix(medv ~., Boston.test)[,-1]
# predict 함수의 첫번째 모델은 ridge 함수 , newx = 예측변수 함수 
Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
# 예측 결과 확인 
head(Boston.pred)

# 예측 모델의 성능 평가 
postResample(pred=Boston.pred, obs=Boston.test$medv)
# RMSE 와 MAE는 오차를 나타내기 때문에 작을수록 good

# lasso regression analysis 

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family='gaussian', alpha=1)

plot(Boston.cv)
# 가장 왼쪽에 있는 점선 = 최적의 람다값. > 예측오차 최소화  
# lasso 회귀분석에서는 중요하지 않은 예측변수의 회귀변수는 0

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

# 패널티 회귀분석은 정확도를 크게하면서 과적합을 줄이는 것이 목표 
# 즉, 모델의 예측 정확도와 간명도의 균형을 찾는 것. 

Boston.cv$lambda.1se
log(Boston.cv$lambda.1se)

coef(Boston.cv, Boston.cv$lambda.min)
coef(Boston.cv, Boston.cv$lambda.1se) 
# 회귀계수 제거. > 간명한 모델 

# lambda.min 과 lambda.1se 간의 성능 비교 

Boston.gnet1 <- glmnet(x=x, y=y, family='gaussian', 
                       alpha=1, lambda=Boston.cv$lambda.min)

Boston.pred1 <- predict(Boston.gnet1, newx = Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv)

# lambda.1se 
Boston.gnet2 <- glmnet(x=x, y=y, family='gaussian', 
                       alpha=1, lambda=Boston.cv$lambda.1se)

Boston.pred2 <- predict(Boston.gnet2, newx = Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv)
 
# 성능지표 바탕으로 예측 성능은 lambda.min 이 우수 
# lambda.1se 는 예측 정확도가 낮은 대신 정확도가 크고 과적합 가능성이 낮음 .




# elasticnet regression analysis 
# ridge + lasso 의 형태 

set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train, 
                   method = "glmnet", 
                   trControl=trainControl(method="cv",
                                          number=10), 
                   tuneLength=10)

# tuneLength = alpha, lambda 값을 각각 10개의 서로 다른 값을 이용하여 이들의 조화를 바타응로
# 교차 검증 실시 

Boston.cv$bestTune #최적의 alpha and lambda

Boston.gnet <- glmnet(x=x, y=y, family='gaussian', 
                       alpha=Boston.cv$bestTune$alpha,
                      lambda=Boston.cv$bestTune$lambda)

coef(Boston.gnet)

Boston.pred <- predict(Boston.gnet, newx = Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

# comparison

# 탐색할 lambda 의 범위 

lambda <- 10^seq(-5, 5, length=100)
lambda

# ridge
set.seed(123)
ridge <- train(form=medv ~ ., data=Boston.train, 
                method = "glmnet", 
               trControl=trainControl(method="cv",
                                          number=10), 
                tuneGrid = expand.grid(alpha=0, lambda=lambda))

coef(ridge$finalModel, ridge$bestTune$lamda)
# coef(최종 모델 , 그 떄의 lambda 값 )
ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)

# lasso

set.seed(123)
lasso <- train(form=medv ~ ., data=Boston.train, 
               method = "glmnet", 
               trControl=trainControl(method="cv",
                                      number=10), 
               tuneGrid = expand.grid(alpha=1, lambda=lambda))

coef(lasso$finalModel, lasso$bestTune$lamda)
# coef(최종 모델 , 그 떄의 lambda 값 )
lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)

#  elastic

set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train, 
               method = "glmnet", 
               trControl=trainControl(method="cv",
                                      number=10), 
               tuneLength = 10)

coef(elastic$finalModel, elastic$bestTune$lamda)
# coef(최종 모델 , 그 떄의 lambda 값 )
elastic.pred <- predict(lasso, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)

library(caret)

models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
models

summary(resamples(models))
summary(resamples(models), metric = "RMSE") #RMSE만 뽑아줘

summary(diff(resamples(models), metric = "RMSE")) # 통계적으로 유의한지 검증

# elastic 모델과 lasso 모델은 ridge 모델보다 간명하기 때문에 elastic or lasso
# 사용이 바람직함 ( 간명함 )
