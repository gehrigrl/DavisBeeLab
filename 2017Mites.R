setwd("~/ABC Analysis/2017 Analysis/Excel Data")
Mites=read.csv("2017MiteData.csv")
Mites[,3]=factor(Mites[,3],levels=c("Mustard","Typical_Control","Wildflower","Rip_Control"))
Mites=Mites[order(Mites$Group),]

#This code creates an empty data frame to be filled with mite percentage data
PercentData=data.frame("PreBloom"=rep(0,32),"Bloom1"=rep(0,32),"Bloom2"=rep(0,32),
                        "PostBloom"=rep(0,32),"May5"=rep(0,32),"June6"=rep(0,32),"July7"=rep(0,32),
                         "Aug10"=rep(0,32),"Sep12"=rep(0,32),"Oct17"=rep(0,32))

#This code converts the original excel data into percentages
for (i in 1:10) {PercentData[i]= Mites[2*i+2]/(Mites[2*i+2]+Mites[2*i+3])}

PercentData=cbind(Mites$ABC,Mites$Location,Mites$Group,PercentData)
names(PercentData)[1:3]=c("ABC","Location","Group")


MustPercent=PercentData[1:16,]
        MustPercent$Sep12[6]=NA
        MustPercent$Oct17[13]=NA
WildPercent=PercentData[17:32,]

library(tidyverse)
library(ggpubr)
library(rstatix)

boxplot=function(inData){
  inData <- inData %>%    #orders data to prepare it for anova function
    gather(key = "Stage", value = "PercentMites", PreBloom, Bloom1, Bloom2, PostBloom,
           May5, June6, July7, Aug10, Sep12, Oct17) %>%
    convert_as_factor(ABC, Stage)
  
  bxp <- ggboxplot(                      #makes boxplot of treatment vs. control
    inData, x = "Stage", y = "PercentMites",
    color = "Group", palette = "jco", order = c("PreBloom", "Bloom1", "Bloom2", "PostBloom",
                                                "May5","June6","July7","Aug10","Sep12","Oct17")
  )
  return(bxp)  }

MustMites=Mites[1:16,]
WildMites=Mites[17:32,]


#This code runs the mite analysis model with a random effect for location
attach(WildMites)
library(MASS)
fit1.glmm <- glmmPQL(cbind(PreMites,PreNoMites)~Group, random=~+1|Location, binomial,data=Mites)
summary(fit1.glmm)

