# 팩터 - factors 

c("Vegetables", "Fruits", "Vegetables", "Grains","Fruits",
  "Vegetables", "Dairy", "Fruits", "Proteins", "Fruits")

food <- factor(c("Vegetables", "Fruits", "Vegetables", "Grains","Fruits",
                 "Vegetables", "Dairy", "Fruits", "Proteins", "Fruits"))
food

install.packages('forcats')
library(forcats)
fct_inorder(food)

fct_infreq(food) # 출연빈도가 많은 순서

fct_relevel(food, "Fruits", "Vegetables", "Grains", "Proteins", 
            "Dairy") # level 순서를 사용자가 지정

fct_relevel(food, "Proteins")
fct_relevel(food, "Dairy")

fct_relevel(food, "Proteins", after=2) # after=2는 2번째 위치 뒤 
fct_relevel(food, "Proteins", after=Inf)

value <- c(1000, 1500, 1200, 700, 2000,
           2000, 1350, 2500, 15000, 3000)

food

fct_reorder(food, .x=value) # .x에는 level의 순서를 정할 기준
fct_reorder(food, .x=value, .fun=mean) # .fun에 mean 적용 

fct_reorder(food, .x=value, .desc = TRUE)

fct_recode(food, Fats="Proteins", Fats="Dairy") # 기존이름 변경 가능.
