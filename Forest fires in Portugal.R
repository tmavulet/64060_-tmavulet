Forest.fires.in.Portugal.
DF=Forest.fires.in.Portugal.
mean(DF$Wind)
mean(DF$Rel..humadity)
sd(DF$Temperature)
sd(DF$ISI.index)
var(DF$FFMC.index)
var(DF$DMC.index)
max(DF$Wind)
min(DF$Wind)
DF$Temperature_ztransformed=(DF$Temperature-mean(DF$Temperature))/sd(DF$Temperature)
View(DF)
plot(DF$Temperature, DF$Wind)