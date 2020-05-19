setwd("~/ABC Analysis/2017 Analysis/Excel Data")
DF=read.csv("2017FullData.csv")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(WRS2)

DF=DF[,-c(2,4,5)]  #removes weight data

DF[,2]=factor(DF$Group,levels=c("Mustard","Typical_Control","Wildflower","Rip_Control"))
FullData=DF[order(DF$Group),]  # orders dataframe by associated treatments

FirstDep=3 #first column with dep variable
CreateDepData=function(posit){      #posit is column # of dep var you're using 
       interv=5 #distance between columns of same dep variable
       NewDep=FullData[c(1,2,posit, posit + interv, posit + interv*2, posit + interv*3,
                         posit + interv*4, posit + interv*5, posit + interv*6, posit + interv*7)]
       names(NewDep)[FirstDep]="PreBloom"   
       names(NewDep)[FirstDep+1]="Bloom1"     
       names(NewDep)[FirstDep+2]="Bloom2"
       names(NewDep)[FirstDep+3]="PostBloom"
       names(NewDep)[FirstDep+4]="May5"
       names(NewDep)[FirstDep+5]="June6"
       names(NewDep)[FirstDep+6]="July5"
       names(NewDep)[FirstDep+7]="Aug10"
       return(NewDep)
      }
BeesData=CreateDepData(FirstDep)#these lines evaluate function for each dep var
       BeesData[ncol(BeesData)+1]=FullData$Adult.Bees.8   #adds extra Adult Bees Data
       names(BeesData)[ncol(BeesData)]="Sep12"
       BeesData[ncol(BeesData)+1]=FullData$Adult.Bees.9
       names(BeesData)[ncol(BeesData)]="Oct13"
BroodData=CreateDepData(FirstDep+1)
HoneyData=CreateDepData(FirstDep+2)
NectarData=CreateDepData(FirstDep+3)
PollenData=CreateDepData(FirstDep+4)

MustBeesData=BeesData[1:16,]
MustBroodData=BroodData[1:16,]
MustHoneyData=HoneyData[1:16,]
MustNectarData=NectarData[1:16,]
MustPollenData=PollenData[1:16,]
        
WildBeesData=BeesData[17:32,]
WildBroodData=BroodData[17:32,]
WildHoneyData=HoneyData[17:32,]
WildNectarData=NectarData[17:32,]
WildPollenData=PollenData[17:32,]

#makes a function that creates boxplots from the above data
boxplot=function(inData){
    inData <- inData %>%    #orders data to prepare it for anova function
        gather(key = "Stage", value = "Pollen", PreBloom, Bloom1, Bloom2, PostBloom,
               May5, June6, July5, Aug10) %>%
        convert_as_factor(ABC, Stage)
    
    bxp <- ggboxplot(                      #makes boxplot of treatment vs. control
      inData, x = "Stage", y = "Pollen",
      color = "Group", palette = "jco", order = c("PreBloom", "Bloom1", "Bloom2", "PostBloom",
                                                  "May5","June6","July5","Aug10")
    )
    return(bxp)  }
    
shapiro=function(inData){
      inData <- inData %>%    #orders data to prepare it for anova function
        gather(key = "Stage", value = "Nectar", PreBloom, Bloom1, Bloom2, PostBloom,
               May5, June6, July5, Aug10) %>% #The stages need to be extended to Oct13 for Adult
              convert_as_factor(ABC, Stage)   #Bees since they were measured for longer.
      inData %>%
        group_by(Stage, Group) %>%
        shapiro_test(Nectar)   }

levene=function(inData){
      inData <- inData %>%
        gather(key = "Stage", value = "Nectar", PreBloom, Bloom1, Bloom2, PostBloom,
               May5, June6, July5, Aug10) %>%
              convert_as_factor(ABC, Stage)  
      inData %>%
        group_by(Stage) %>%
        levene_test(Nectar ~ Group)}


#this functions runs the mixed ANOVA test
mixed=function(inData){
      inData <- inData %>%    #orders data to prepare it for anova function
           gather(key = "Stage", value = "Nectar", PreBloom, Bloom1, Bloom2, PostBloom,
           May5, June6, July5, Aug10) %>%
           convert_as_factor(ABC, Stage)  
  
      res.aov <- anova_test(
          data = inData, dv = Nectar, wid = ABC,
          between = Group, within = Stage)
  
   get_anova_table(res.aov) }

#This code applies the sqrt transform to the non normal data. Input whatever data needs to be transformed
for (i in 3:ncol(NectarData)) {            
  NectarData[i]=sqrt(NectarData[i])
}

#this function runs the robust version of mixed ANOVA
robust=function(inData){
  inData <- inData %>%    
    gather(key = "Stage", value = "Honey", PreBloom, Bloom1, Bloom2, PostBloom
           , May5, June6, July5, Aug10) %>%
    convert_as_factor(ABC, Stage) 
  bwtrim(Honey~Group*Stage, id = Stage, data=inData)}

#this function runs one way anova at each stage 
one=function(inData){
  inData <- inData %>%    #orders data to prepare it for anova function
    gather(key = "Stage", value = "Nectar", PreBloom, Bloom1, Bloom2, PostBloom,
           May5, June6, July5, Aug10) %>%
    convert_as_factor(ABC, Stage)  
  
  one.way <- inData %>%
    group_by(Stage) %>%
    anova_test(dv = Nectar, wid = ABC, between = Group) %>%
    get_anova_table() %>%
    adjust_pvalue(method = "bonferroni")
  one.way }

#This code runs the wilcoxon rank sum test on whatever data is specified in the function
wilcox.test(Aug10~Group, data=MustHoneyData)


#This section of the code makes the graph that was submitted to the Blue Diamond Almond Co.
#that shows change in average adult bee counts over time.

#Get Adult Bees Average
AVG=data.frame("Prebloom"=rep(0,4), "Bloom1"=rep(0,4), "Bloom2"=rep(0,4), "PostBloom"=rep(0,4))
for (i in 3:6)  {
AVG[,i-2]=c(mean(BeesData[,i][1:8]), mean(BeesData[,i][9:16]),
          mean(BeesData[,i][17:24]),mean(BeesData[,i][25:32]))
}
Group=c("Mustard","Typical_Control","Wildflower","Rip_Control")
BeesAVG=cbind(Group,AVG)

LineData=read.csv("2017BeeDataforLineChart.csv")
#Makes LineGraph
LineData[,2]=factor(LineData$Stage,levels=c("PreBloom","Bloom1","Bloom2","PostBloom"))
library(ggplot2)
ggplot(data=LineData, aes(x=Stage, y=AdultBees, group=Group,color=Group)) +
       geom_line() +
       geom_point() +
       ggtitle("Adult Bee Counts in 2017 Almond Bloom") +
       labs(y="Adult Bees")

