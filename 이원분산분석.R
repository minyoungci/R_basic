str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose,
                           levels=c(0.5, 1.0, 2.0),
                           labels=c("low","mid","high"))

str(ToothGrowth)

ToothGrowth

with(ToothGrowth, tapply(len, list(supp, dose), length))
with(ToothGrowth, tapply(len, list(supp, dose), mean))
with(ToothGrowth, tapply(len, list(supp, dose), sd))

#이원분산분석

ToothGrowth.aov <- aov(len ~ supp * dose, data=ToothGrowth)
ToothGrowth.aov <- aov(len ~ supp + dose + supp:dose, data=ToothGrowth)

summary(ToothGrowth.aov)

model.tables(ToothGrowth.aov, type="means")

boxplot(len ~ supp * dose, data=ToothGrowth)
boxplot(len ~ supp * dose, data=ToothGrowth,
        col=c("deeppink","yellowgreen"), las=1,
        xlab="Vitamin C Type", ylab="Tooth Growth",
        main="Effects of Vitamin C on Tooth Growth of Guninea Pigs")

interaction.plot(x.factor=ToothGrowth$dose,
                 trace.factor = ToothGrowth$supp,
                 response = ToothGrowth$len,
                 trace.label = "Supplement",
                 las=1, type="b", pch=c(1, 19),
                 col=c("blue", "red"),
                 xlab="Dose Level", ylab="Tooth Length",
                 main="Interaction plot for Tooth Growth of Guinea pigs")

library(gplots)
interaction(ToothGrowth$supp, ToothGrowth$dose, sep=" ")

plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth,
          col=c("red", "green3"),
          xlab="Supplement and Dose Combination",
          ylab="Tooth Growth",
          main="Means plot for Tooth Growth of guinea pigs")

plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth,
          connect = list(c(1,3,5), c(2,4,6)),
          col=c("red", "green3"),
          xlab="Supplement and Dose Combination",
          ylab="Tooth Growth",
          main="Means plot for Tooth Growth of guinea pigs")

coplot(len ~ dose | supp, data=ToothGrowth,
       col="steelblue", pch=19)

coplot(len ~ dose | supp, data=ToothGrowth,
       col="steelblue", pch=19,
       panel = panel.smooth, lwd=2, col.smooth="darkorange",
       xlab="Doae Level", ylab="Tooth Growth")

library(dplyr)

install.packages("HH")
library(HH)

interaction2wt(len ~ supp * dose, data=ToothGrowth)

TukeyHSD(ToothGrowth.aov)

TukeyHSD(ToothGrowth.aov, which = c("dose"), conf.level=0.99)
# 별도 추출 = which 사용 


