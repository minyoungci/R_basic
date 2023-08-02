str(InsectSprays)
InsectSprays

tapply(InsectSprays$count, InsectSprays$spray, length)
tapply(InsectSprays$count, InsectSprays$spray, mean) 
tapply(InsectSprays$count, InsectSprays$spray, sd) 

library(gplots)

plotmeans(count ~ spray, data=InsectSprays)

plotmeans(count ~ spray, data=InsectSprays,
          barcol = "tomato", barwidth = 3,
          col="cornflowerblue", lwd=2,
          xlab="Type of Sprays", ylab="Insect Count",
          main = "Performance of Insect Sprays")

# 상자도표 

boxplot(count ~ spray, data=InsectSprays, col="tomato",
        xlab="Type of Sprays", ylab="Insect Count",
        main = "Performance of Insect Sprays")


sprays.aov <- aov(count ~ spray , data=InsectSprays)
sprays.aov

summary(sprays.aov)

model.tables(sprays.aov, type="mean")
model.tables(sprays.aov, type="effects")

# 다중비교 
sprays.compare <- TukeyHSD(sprays.aov)
sparays.compare

plot(sprays.compare)


library(multcomp)

# 다중비교 방식을 상대그래프로 그리기 


tuk.hsd <- glht(model=sprays.aov, linfct=mcp(spray="Tukey"))
tuk.hsd

cld(tuk.hsd, levek=0.5)
plot(cld(tuk.hsd, levek=0.5))

#### 

str(InsectSprays)

sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov)

library(car)
qqPlot(sprays.aov$residuals, distribution="norm",
       pch=20, col="tomato", id=FALSE,
       main="Q-Q plot", 
       xlab="Theoretical Quantiles", ylab="Residual")


shapiro.test(residuals(sprays.aov))


# 등분산성 

leveneTest(count ~ spray, data=InsectSprays)
bartlett.test(count ~ spray, data=InsectSprays)



