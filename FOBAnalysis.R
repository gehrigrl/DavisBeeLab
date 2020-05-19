setwd("~/ABC Analysis/2018 Analysis/Excel Data")
BeeData=read.csv("2018FOBData.csv")
BeeData[,2]=factor(BeeData[,2], levels=c("Mustard","Typical Control","Wildflower","Rip Control"))
BeeData=BeeData[order(BeeData$Group),]  # orders dataframe by associated treatments
names(BeeData)[3:7]=c("Feb5","Feb12","Feb26","Mar9","Apr4")
library(tidyverse)
library(ggpubr)
library(rstatix)

MustFOB=BeeData[1:32,]
WildFOB=BeeData[33:64,]

AVG=data.frame("Feb5"=rep(0,4), "Feb12"=rep(0,4), "Feb26"=rep(0,4), "Mar9"=rep(0,4), "Apr4"=rep(0,4))
for (i in 2:6)  {
  AVG[,i-1]=c(mean(BeeData[,i][1:16]), mean(BeeData[,i][17:32]),
              mean(BeeData[,i][33:48],na.rm=TRUE),mean(BeeData[,i][49:64],na.rm=TRUE))
}
Group=c("Mustard","Mustard Control","Wildflower","Wildflower Control")
BeesAVG=cbind(Group,AVG)
library(xlsx)
write.csv(BeesAVG, "2018BeeData.csv")
AVGData=read.csv("2018BeeData.csv")
AVGData=AVGData[,1:3]
AVGData[,2]=factor(AVGData$Stage,levels=c("5-Feb","12-Feb","26-Feb","9-Mar","4-Apr"))
library(ggplot2)
ggplot(data=AVGData, aes(x=Stage, y=FOB, group=Group,color=Group)) +
  geom_line() +
  geom_point() +
  ggtitle("Frames of Bees in 2018 Almond Bloom") +
  labs(y="Frames of Bees")

shapiro=function(inData){
  inData <- inData %>%    #orders data to prepare it for anova function
    gather(key = "Stage", value = "FOB", Feb5, Feb12, Feb26, Mar9, Apr4) %>% #The stages need to be extended to Oct13 for Adult
    convert_as_factor(Stage)                                            #Bees since they were measured for longer.
  inData %>%
    group_by(Stage, Group) %>%
    shapiro_test(FOB)   }

levene=function(inData){
  inData <- inData %>%
    gather(key = "Stage", value = "FOB", Feb5, Feb12, Feb26, Mar9, Apr4) %>%
    convert_as_factor(Stage)  
  inData %>%
    group_by(Stage) %>%
    levene_test(FOB ~ Group)}

mixed=function(inData){
  inData <- inData %>%    #orders data to prepare it for anova function
    gather(key = "Stage", value = "FOB", Feb5, Feb12, Feb26, Mar9, Apr4) %>%
    convert_as_factor(ABC, Stage)  
  
  res.aov <- anova_test(
    data = inData, dv = FOB, wid = ABC,
    between = Group, within = Stage)
  get_anova_table(res.aov) }

one=function(inData){
  inData <- inData %>%    #orders data to prepare it for anova function
    gather(key = "Stage", value = "FOB", Feb5, Feb12, Feb26, Mar9, Apr4) %>%
    convert_as_factor(ABC, Stage)  
  
  one.way <- inData %>%
    group_by(Stage) %>%
    anova_test(dv = FOB, wid = ABC, between = Group) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  one.way }
