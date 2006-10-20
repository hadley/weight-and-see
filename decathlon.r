# Decathlon scores
# first variable is reported total
# second variable is sum of individual event totals

library(ggplot)

d <- read.table("decathlon-errors.txt",header=TRUE)
qplot(total, sum, data=d, size=total-sum)

scsize(qplot(total, sum-total, data=d, size=abs(total-sum)), to=c(0, 3))

qplot(total, sum-total, data=d, size=abs(total-sum), weight=abs(total-sum), type=c("point","smooth"))

qplot(total, sum-total, data=d, type=c("point","smooth"))

qplot(as.numeric(as.character(Var1)), Freq, data=as.data.frame(table(d$sum - 
d$total)), ylim=c(0,1000))

qplot(as.numeric(as.character(Var1)), Freq, data=as.data.frame(table(d$sum - d$total)), log="y")