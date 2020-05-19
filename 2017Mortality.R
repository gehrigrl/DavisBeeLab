setwd("~/ABC Analysis/2017 Analysis/Excel Data")
mort=read.csv("2017mortality.csv")
mort$Group=factor(mort$Group,c("Mustard","Typical_Control","Wildflower","Rip_Control"))
mort=mort[order(mort$Group),]
library(survival)

mustmort=mort[1:16,]
wildmort=mort[17:32,]

survobj = with(mort, Surv(DAYS,Alive))
fitwild=survfit(survobj~Group,data=mort)



plot(fitwild, xlab="Survival Time in Days",
     ylab="% Surviving", yscale=100, col=c("red","blue","black","orange"),
     main="Survival Distributions by Treatment")
legend("bottomright", title="Treatment", c("Wildflower", "Rip_Control","Mustard","Typ_Control"),
       fill=c("red", "blue","black","orange"))

wildtest=survdiff(survobj~Group, data=wildmort)
musttest=survdiff(survobj~Group, data=mustmort)

test=survdiff(survobj~Group, data=mort)

