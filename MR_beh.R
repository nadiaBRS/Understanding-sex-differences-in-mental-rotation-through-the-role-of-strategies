library(nlme)
library(emmeans)
library(ggplot2)
library(psych)
library(stats)
library(reshape2)
library(dplyr)
library(ggpubr)
library(FSA)
library(effsize)
library(lsr)

getwd()

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs2.txt",header = T)


library(data.table)

data_fem <- subset(data, Gender == 'F')
summary(data_fem)
sd(data_fem$Age)
data_males <- subset(data, Gender == 'M')
summary(data_males)
sd(data_males$Age)



MR_ACCwide=subset(data, select= c(1:18))  # subset for just acc
MR_ACCwide$Sub <- factor(MR_ACCwide$Sub)  # transforming sub as factor

MR_RTwide=subset(data, select= c(1:6, 19:30)) # same
MR_RTwide$Sub <- factor(MR_RTwide$Sub) #same


MR_ACClong_temp<- melt(setDT(MR_ACCwide),
                       measure = patterns("Control", "OBS", "EBS"), 
                       value.name = c("Control", "OBS", "EBS"), 
                       variable.name = "Block")

# putting in rows

MR_ACClong<- melt(MR_ACClong_temp,
                  id.vars=c("Sub", "Version","Age","Gender","TST", "Digit", "Block"),
                  measure.vars=c("Control", "OBS", "EBS"),
                  variable.name="Cond",
                  value.name="Accuracy")



MR_RTlong_temp<- melt(setDT(MR_RTwide),
                      measure = patterns("Control", "OBS", "EBS"), 
                      value.name = c("Control", "OBS", "EBS"), 
                      variable.name = "Block")


MR_RTlong<- melt(MR_RTlong_temp,
                 id.vars=c("Sub", "Version","Age","Gender","TST", "Digit", "Block"),
                 measure.vars=c("Control", "OBS", "EBS"),
                 variable.name="Cond",
                 value.name="RT"
)



MR_ACClong$Gender <- as.factor(MR_ACClong$Gender)
MR_RTlong$Gender <- as.factor(MR_RTlong$Gender)


MR_ACC<-lme(Accuracy~Cond*Gender*Block,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACClong)
anova(MR_ACC, test = "F")
summary(MR_ACC)
mod <- car::Anova(MR_ACC)
mod
etaSquared(MR_ACC)
effectsize::eta_squared(MR_ACC)

MR_ACC2<-lm(Accuracy~Cond*Gender*Block, MR_ACClong)
anova(MR_ACC2, test = 'F')
etaSquared(MR_ACC2)

MR_ACC2_carr <- car:: Anova(MR_ACC2)
MR_ACC2_carr

a <- ggplot(MR_ACClong,aes(Cond, Accuracy, color=Gender))+
  geom_boxplot() + theme(panel.background = element_blank())+
  ggtitle("Interaction condition*gender")
a

b <- ggplot(MR_ACClong,aes(Cond, Accuracy, fill=Gender))+
  geom_boxplot() + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Condition") + ylab("Accuracy ")

b

b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                        axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))



c <- ggplot(MR_ACClong,aes(Block, Accuracy, color=Cond))+
  facet_wrap(~Cond)+
  stat_summary(aes(group=Cond, color=Cond), fun="mean", geom="point", position= position_dodge(0.5)) +
  stat_summary(aes(group=Cond, color = Cond), fun = "mean", geom="line",position= position_dodge(0.5)) +              
  stat_summary(aes(group=Cond, color=Cond), fun.data = "mean_cl_normal",                  
               geom="errorbar", width=0.1,position= position_dodge(0.5)) +
  theme_bw()+
  ggtitle("Interaction block*condition")

c

c <- ggplot(MR_ACClong,aes(Block, Accuracy, color=Cond))+
  facet_wrap(~Cond)+
  stat_summary(aes(group=Cond, color=Cond), fun="mean", geom="point", position= position_dodge(0.5)) +
  stat_summary(aes(group=Cond, color = Cond), fun = "mean", geom="line",position= position_dodge(0.5)) +              
  stat_summary(aes(group=Cond, color=Cond), fun.data = "mean_cl_normal",                  
               geom="errorbar", width=0.1,position= position_dodge(0.5))+
  theme_bw() + theme( panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                      axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

c + coord_cartesian(ylim = c(0.70, 0.85)) + ggtitle("Interaction version*condition")




####### INVESTIGATION TO SEE IF ANY JUMP IS SIGNIFICANT ---> NO THEY ARE NOT #####################

MR_ACC_Ctrl <- subset(MR_ACClong, Cond == 'Control')
MR_ACC_Ctrl1 <- subset(MR_ACC_Ctrl, Block == '1')
MR_ACC_Ctrl2 <- subset(MR_ACC_Ctrl, Block == '2')
MR_ACC_Ctrl3 <- subset(MR_ACC_Ctrl, Block == '3')

t.test(MR_ACC_Ctrl1$Accuracy,MR_ACC_Ctrl2$Accuracy) 
t.test(MR_ACC_Ctrl2$Accuracy,MR_ACC_Ctrl3$Accuracy) 
t.test(MR_ACC_Ctrl1$Accuracy,MR_ACC_Ctrl3$Accuracy) 

MR_ACC_Allo <- subset(MR_ACClong, Cond == 'OB')
MR_ACC_Allo1 <- subset(MR_ACC_Allo, Block == '1')
MR_ACC_Allo2 <- subset(MR_ACC_Allo, Block == '2')
MR_ACC_Allo3 <- subset(MR_ACC_Allo, Block == '3')

t.test(MR_ACC_Allo1$Accuracy,MR_ACC_Allo2$Accuracy) 
t.test(MR_ACC_Allo2$Accuracy,MR_ACC_Allo3$Accuracy) 


library(emmeans)

#Planned comparisons is when you plan them ahead, before collecting your data, 
#based on you hypothesis. While post-hocs are comparisons that you do based on 
# the results that you obtain from an ANOVA. And since I had hypothesis from 
# the beggining ... I could do that right ?

e1<- emmeans(MR_ACC, list(pairwise ~ Gender|Cond), adjust="tukey")
e1
cohen.d(MR_ACClong$Accuracy ~MR_ACClong$Gender)

data_ctrl<- subset(MR_ACClong,Cond=="Control")
cohen.d(data_ctrl$Accuracy ~data_ctrl$Gender)

data_ego<- subset(MR_ACClong,Cond=="EB")
cohen.d(data_ego$Accuracy ~data_ego$Gender)

data_allo<- subset(MR_ACClong,Cond=="OB")
cohen.d(data_allo$Accuracy ~data_allo$Gender)

eff_size(e1, sigma= sigma(MR_ACC2), edf=df.residual(MR_ACC2))

emmeans(MR_ACC, list(pairwise ~ Cond|Gender), adjust="Tukey")
emmeans(MR_ACC, list(pairwise ~ Cond|Gender), adjust="None")

emmeans(MR_ACC, list(pairwise ~ Gender|Cond), adjust="None")

MR_ACC_long_Fem<- subset(data_ctrl,Gender=="F")
data_ego_fem<- subset(data_ego,Gender=="F")
cohen.d(MR_ACC_long_Fem$Accuracy ~data_ego_fem$Accuracy)

# The first compares males vs females in each condition, while the second 
# compares conditions (eg. allo vs ego) within each gender

emmeans(MR_ACC, list(pairwise ~ Cond|Block), adjust="tukey")
emmeans(MR_ACC, list(pairwise ~ Block|Cond), adjust="tukey")
emmeans(MR_ACC, list(pairwise ~ Cond:Block), adjust="tukey")

t.test(MR_ACC_Ctrl1$Accuracy)

# The first compares the condition in each block, while the second 
# compares blocks within each condition

emmeans(MR_ACC, list(pairwise ~ Gender|Cond), adjust="tukey") %>%
  confint() 


MR_ACC_long_Fem = subset(MR_ACClong,Gender =="F")
MR_ACC_fem<-lme(Accuracy~Cond*Block,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Fem)
anova(MR_ACC_fem, test = "F")
car::Anova(MR_ACC_fem)


emmeans(MR_ACC_fem, list(pairwise ~ Cond), adjust="tukey")

#or

ctrl_fem <- subset(MR_ACC_long_Fem, Cond=="Control")
allo_fem <- subset(MR_ACC_long_Fem, Cond=="OB")
ego_fem <- subset(MR_ACC_long_Fem, Cond=="EB")

t.test(ctrl_fem$Accuracy,allo_fem$Accuracy) 
t.test(ctrl_fem$Accuracy, ego_fem$Accuracy) 
t.test(allo_fem$Accuracy,ego_fem$Accuracy) 


dunnTest(Accuracy ~ Cond,
         data=MR_ACC_long_Fem,
         method="bh") 
boxplot(Accuracy ~ Cond,
        data=MR_ACC_long_Fem,
        col=c("#F1948A", "#58D68D", "#5DADE2"),
        frame=F,
        ylab="Accuracy",
        xlab="Condition")



library(lme4)
MR_ACClong$TST<-as.numeric(MR_ACClong$TST)
MR_ACC<-lme(Accuracy~Cond*Gender*TST, random=~1|Sub, method='ML', na.action = na.omit, data = MR_ACClong)
anova(MR_ACC, test = "F")

p<-ggplot(MR_ACClong,aes(TST, Accuracy, color=Cond, group=Cond))+
  geom_smooth(method='lm', formula=y~x)+  geom_point() +
  theme_bw()
p + facet_wrap(~Gender)

MR_ACC_long_Male = subset(MR_ACClong,Gender =="M")
MR_ACC_Male<-lme(Accuracy~Cond*TST,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Male)
anova(MR_ACC_Male, test = "F")

MR_ACC_long_Fem = subset(MR_ACClong,Gender =="F")
MR_ACC_Fem<-lme(Accuracy~Cond*TST,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Fem)
anova(MR_ACC_Fem, test = "F")

ggplot(MR_ACC_long_Fem,aes(TST, Accuracy, color=Cond, group=Cond))+
  geom_smooth(method='lm', formula=y~x)+  geom_point() +
  theme_bw()

MR_ACC_long_Fem[-c(2, 31,), ] 

# The problem here is that we have these outliers females that really drive the slope down. 

#MR_ACC_long_Fem = subset(MR_ACClong2,Gender =="F")
#MR_ACC_Fem<-lme(Acc~Cond*TST,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Fem)
#anova(MR_ACC_Fem, test = "F")

#MR_ACC_long_Fem_ctrl = subset(MR_ACC_long_Fem, Cond == 'Ctrl')
#MR_ACC_long_Fem_ego = subset(MR_ACC_long_Fem, Cond == 'Ego')
#MR_ACC_long_Fem_allo = subset(MR_ACC_long_Fem, Cond == 'Allo')
#t.test(subset(MR_ACC_long_Fem_ctrl)$ACC, subset(MR_ACC_long_Fem_ctrl)$TST)
#t.test(subset(MR_ACC_long_Fem_ctrl)$TST, subset(MR_ACC_long_Fem_allo)$TST)
#t.test(subset(MR_ACC_long_Fem_allo, Gender == "F")$Acc, subset(MR_ACC_long_Fem_ego, Gender == "F")$Acc)



MR_RT<-lme(RT~Cond*Gender*Block,random=~1|Sub,method="ML",na.action = na.omit, data = MR_RTlong)
anova_rt <- anova(MR_RT, test = "F")
anova_rt
car:: Anova(MR_RT)
effectsize::eta_squared(MR_RT)

library(effsize)
eta_sq(anova_rt, partial = FALSE, ci.lvl = NULL, n = 1000, method = c("dist", "quantile"))

MR_RT2<-lm(RT~Cond + Gender +Block + Gender + Cond:Gender + Cond:Block + Gender+Block, data = MR_RTlong)
Anova(MR_RT2, test = 'F')
car:: Anova(MR_RT2)


ggplot(MR_RTlong,aes(Cond, RT, color=Block))+
  geom_boxplot() + theme_bw() +
  ggtitle("Interaction block*condition")

ggplot(MR_RTlong,aes(Cond, RT))+
  geom_boxplot() + theme_bw() +
  ggtitle("RT according to condition")

t.test(data$ctrl_rt,data$ego_rt)
t.test(data$ctrl_rt,data$allo_rt)
cohen.d(data$ctrl_rt ~data$ego_rt)

emmeans(MR_RT, list(pairwise ~ Cond|Block), adjust="tukey")
emmeans(MR_RT, list(pairwise ~ Block|Cond), adjust="tukey")

MR_RT_Ctrl <- subset(MR_RTlong, Cond == 'Control')
MR_RT_Ctrl1 <- subset(MR_RT_Ctrl, Block == '1')
MR_RT_Ctrl2 <- subset(MR_RT_Ctrl, Block == '2')
MR_RT_Ctrl3 <- subset(MR_RT_Ctrl, Block == '3')

t.test(MR_RT_Ctrl1$RT,MR_RT_Ctrl2$RT) 
t.test(MR_RT_Ctrl2$RT,MR_RT_Ctrl3$RT) 
t.test(MR_RT_Ctrl1$RT,MR_RT_Ctrl3$RT) 

MR_RT_Allo <- subset(MR_RTlong, Cond == 'OB')
MR_RT_Allo1 <- subset(MR_RT_Allo, Block == '1')
MR_RT_Allo2 <- subset(MR_RT_Allo, Block == '2')
MR_RT_Allo3 <- subset(MR_RT_Allo, Block == '3')

t.test(MR_RT_Allo1$RT,MR_RT_Allo2$RT) 
t.test(MR_RT_Allo2$RT,MR_RT_Allo3$RT) 

MR_RT_block1 <- subset(MR_RTlong, Block == '1')
MR_RT_block2 <- subset(MR_RTlong, Block == '2')
MR_RT_block3 <- subset(MR_RTlong, Block == '3')

t.test(MR_RT_block1$RT, MR_RT_block2$RT)
t.test(MR_RT_block1$RT, MR_RT_block3$RT)
t.test(MR_RT_block2$RT, MR_RT_block3$RT)


e <- ggplot(MR_RTlong,aes(Cond, RT, fill=Cond))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))+ ggtitle("Interaction Gender*condition")

e + xlab("Condition")

emmeans(MR_RT, list(pairwise ~ Cond|Gender), adjust="tukey")
emmeans(MR_RT, list(pairwise ~ Gender|Cond), adjust="tukey")


ggplot(MR_RTlong,aes(Block, RT, color=Cond))+
  facet_wrap(~Cond)+
  stat_summary(aes(group=Cond, color=Cond), fun="mean", geom="point", position= position_dodge(0.5)) +
  stat_summary(aes(group=Cond, color = Cond), fun = "mean", geom="line",position= position_dodge(0.5)) +              
  stat_summary(aes(group=Cond, color=Cond), fun.data = "mean_cl_normal",                  
               geom="errorbar", width=0.1,position= position_dodge(0.5)) +
  theme_bw()+
  ggtitle("Interaction block*condition")


d <- ggplot(MR_RTlong,aes(Block, RT, color=Cond))+
  facet_wrap(~Cond)+
  stat_summary(aes(group=Cond, color=Cond), fun="mean", geom="point", position= position_dodge(0.5)) +
  stat_summary(aes(group=Cond, color = Cond), fun = "mean", geom="line",position= position_dodge(0.5)) +              
  stat_summary(aes(group=Cond, color=Cond), fun.data = "mean_cl_normal",                  
               geom="errorbar", width=0.1,position= position_dodge(0.5))+
  theme_bw() + theme( panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                      axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

d + ggtitle("Interaction block*condition")





dunnTest(RT ~ Cond,
         data=MR_RTlong,
         method="bh") 
boxplot(RT ~ Cond,
        data=MR_RTlong,
        col=c("#F1948A", "#58D68D", "#5DADE2"),
        frame=F,
        ylab="RT",
        xlab="Condition")



# --> check here if the allo effect of block 2 is not caused by some outliers


library(tidyverse)

## subsetting 
grouped_ctrl <- subset(MR_RTlong, Cond == "Ctrl")

grouped_ctrl1 <- grouped_ctrl %>% group_by(Cond, RT) %>% 
  dplyr:: summarise(mean_RT = mean(RT, na.rm = TRUE), sd_RT=sd(RT, na.rm=TRUE))

grouped_cond <- MR_RTlong %>% group_by(Cond, RT) %>% 
  dplyr:: summarise(mean_RT = mean(RT, na.rm = TRUE), sd_RT=sd(RT, na.rm=TRUE))

library(plyr)

grouped_cond1 <- ddply(MR_RTlong, ~ Cond*RT, summarise, mean_RT = mean(RT, na.rm = TRUE),
                       sd_RT=sd(RT, na.rm=TRUE))


grouped_cond <- MR_RTlong %>% group_by(Cond, RT) %>% 
  dplyr::summarise(n =n(), mean_RT = mean(RT, na.rm = TRUE), sd_RT=sd(RT, na.rm=TRUE))

library(ggpubr)


ggdensity(MR_ACClong$Accuracy, 
          main = "dist",
          xlab = "Accuracy")
ggqqplot(MR_ACClong$Accuracy)
shapiro.test(MR_ACClong$Accuracy)
ks.test(MR_ACClong$Accuracy, "pnorm", mean=mean(MR_ACClong$Accuracy), sd=sd(MR_ACClong$Accuracy))

skewness(MR_ACClong$Accuracy)
kurtosis(MR_ACClong$Accuracy)

ggdensity(MR_RTlong$RT, 
          main = "dist",
          xlab = "RT")
ggqqplot(MR_RTlong$RT)
shapiro.test(MR_RTlong$RT)
ks.test(MR_RTlong$RT, "pnorm", mean=mean(MR_RTlong$RT), sd=sd(MR_RTlong$RT))

skewness(MR_RTlong$RT)
kurtosis(MR_RTlong$RT)

# another thing we could compute is the squeudness and kurtosis. If value is below .5 it's ok. between .5 and 1 its meh. And then above
# 1 it's not so good. 


skewness(MR_RTlong$RT)
kurtosis(MR_RTlong$RT)


