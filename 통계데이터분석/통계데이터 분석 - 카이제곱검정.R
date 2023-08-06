survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol=2)

dimnames(survivors) <- list(Status=c("minor injury",
                                     "major injury",
                                     "dead"),
                            Seatbelt=c("with seatbelt",
                                       "without seatbelt"))

survivors

addmargins(survivors)

addmargins(survivors, 2) # 열의 합만 계산 

# 비율의 교차표를 만들기 위해 prop.table()함수를 사용합니다. 
# 열의 합계가 100%가 되도록 하기 위해서 margin인수에 2를 지정합니다. 
prop.table(addmargins(survivors,2), 2)

addmargins(prop.table(addmargins(survivors,2), 2),1)

barplot(survivors, ylim=c(0,2500), las=1,
        col=c("yellowgreen", 'lightsalmon', "orange" ),
        ylab="Frequency", main="Frequency of Survivors")

survivors.porp = prop.table(survivors, 2)
barplot(survivors.porp*100, las=1,
        col=c("yellowgreen", 'lightsalmon', "orange" ),
        ylab="Percentage", main="Percentage of Survivors")

pchisq(45.91, df=(3-1)*(2-1), lower.tail = FALSE) 

qchisq(0.05, df=(3-1)*(2-1), lower.tail = FALSE)
