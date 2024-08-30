# videogames 
# Acc= gender*videogames

library(rio)
library(nlme)
library(plyr)
library(car)
library(dbplyr)
library(dplyr)
library(dtplyr)
library(effects)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(grid)
library(lattice)
library(magrittr)
library(multcomp)
library(PairedData)
library(plyr)
library(rio)
library(rstatix)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lme4)
library(car)
library(MuMIn)
library(multimode)


getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)



data <-data %>% gather(key = "condition", value = "Mean_RT" , "ctrl_rt", "ego_rt", "allo_rt")
data$Mean_RT<- gsub(",",".",data$Mean_RT)
data$Mean_RT<-as.numeric(data$Mean_RT)
str(data$Mean_RT)
mod0<-lme(Mean_RT~condition*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data)
anova(mod0, test = "F")
car:: Anova(mod0)
effectsize::eta_squared(mod0)

p <- ggplot(data, aes(x = vghours, y = Mean_RT)) + geom_point()
p + aes(color = Gender) + geom_smooth(method = "lm", se = FALSE)
    

data_women<- subset(data,Gender=="F")
data_men<- subset(data,Gender=="M")

mod1 <- lm(Mean_RT ~ vghours*condition, data_women)
anova(mod1)

mod1<-lme(Mean_RT~condition*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data_women)
anova(mod1, test = "F")
car:: Anova(mod1)
effectsize::eta_squared(mod1)

mod2<-lme(Mean_RT~condition*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data_men)
anova(mod1, test = "F")
car:: Anova(mod2)
effectsize::eta_squared(mod1)

data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)

mod3 <- lm(ctrl_rt~ vghours + Gender + vghours:Gender, data)
anova(mod3)

mod3.1 <- lm(allo_rt~ vghours + Gender + vghours:Gender, data)
anova(mod3.1)

mod3.2 <- lm(ego_rt~ vghours + Gender + vghours:Gender, data)
anova(mod3.2)

mod4 <- lm(allo_rt~ vghours, data)
anova(mod4)

mod5 <- lm(ego_rt~ vghours, data)
anova(mod5)

data_women<- subset(data,Gender=="F")
data_men<- subset(data,Gender=="M")


mod6 <- lm(ctrl_rt~ vghours, data_women)
anova(mod6)

mod7 <- lm(allo_rt~ vghours, data_women)
anova(mod7)

mod8 <- lm(ego_rt~ vghours, data_women)
anova(mod8)

mod9 <- lm(ctrl_rt~ vghours, data_men)
anova(mod9)

mod10 <- lm(allo_rt~ vghours, data_men)
anova(mod10)

mod11 <- lm(ego_rt~ vghours, data_men)
anova(mod11)

###################### DOING THE SAME FOR ACCURACY #################################


getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)



data <-data %>% gather(key = "condition", value = "accuracy" , "ctrl_acc", "ego_acc", "allo_acc")
data$accuracy<- gsub(",",".",data$accuracy)
data$accuracy<-as.numeric(data$accuracy)
str(data$accuracy)
mod0<-lme(accuracy~condition*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data)
anova(mod0, test = "F")
car:: Anova(mod0)
effectsize::eta_squared(mod0)

data_women<- subset(data,Gender=="F")
data_men<- subset(data,Gender=="M")

mod1 <- lm(accuracy ~ vghours, data_women)
anova(mod1)

mod2 <- lm(accuracy ~ vghours, data_men)
anova(mod2)

data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)

mod3 <- lm(ctrl_acc~ vghours + Gender + vghours:Gender, data)
anova(mod3)

mod3.1 <- lm(allo_acc~ vghours + Gender + vghours:Gender, data)
anova(mod3.1)

mod3.2 <- lm(ego_acc~ vghours + Gender + vghours:Gender, data)
anova(mod3.2)

mod4 <- lm(allo_acc~ vghours, data)
anova(mod4)

mod5 <- lm(ego_acc~ vghours, data)
anova(mod5)

data_women<- subset(data,Gender=="F")
data_men<- subset(data,Gender=="M")


mod6 <- lm(ctrl_acc~ vghours, data_women)
anova(mod6)

mod7 <- lm(allo_acc~ vghours, data_women)
anova(mod7)

mod8 <- lm(ego_acc~ vghours, data_women)
anova(mod8)

mod9 <- lm(ctrl_acc~ vghours, data_men)
anova(mod9)

mod10 <- lm(allo_acc~ vghours, data_men)
anova(mod10)

mod11 <- lm(egos_acc~ vghours, data_men)
anova(mod11)



MR_ACCwide=subset(data, select= c(1:4, 9:21))  # subset for just acc
MR_ACCwide$Sub <- factor(MR_ACCwide$Sub)  # transforming sub as factor

MR_RTwide=subset(data, select= c(1:4, 9, 22:33)) # same
MR_RTwide$Sub <- factor(MR_RTwide$Sub) #same

MR_ACClong_temp<- melt(setDT(MR_ACCwide),
                       measure = patterns("Ctrl", "Allo", "Ego"), 
                       value.name = c("Ctrl", "Allo", "Ego"))
                      
# putting in rows

MR_ACClong<- melt(MR_ACClong_temp,
                  id.vars=c("Sub", "Version","Age","Gender","vghours"),
                  measure.vars=c("Ctrl", "Allo", "Ego"),
                  variable.name="Cond",
                  value.name="Acc")


MR_RTlong_temp<- melt(setDT(MR_RTwide),
                      measure = patterns("Ctrl", "Allo", "Ego"), 
                      value.name = c("Ctrl", "Allo", "Ego"), 
                      variable.name = "Block")

MR_RTlong<- melt(MR_RTlong_temp,
                 id.vars=c("Sub", "Version","Age","Gender","vghours","Block"),
                 measure.vars=c("Ctrl", "Allo", "Ego"),
                 variable.name="Cond",
                 value.name="RT")

MR_ACClong$Gender <- as.factor(MR_ACClong$Gender)
MR_RTlong$Gender <- as.factor(MR_ACClong$Gender)          
str(MR_ACClong)


library(lme4)
MR_ACClong$vghours<-as.numeric(MR_ACClong$vghours)
MR_ACC<-lme(Acc~Cond*Gender*vghours, random=~1|Sub, method='ML', na.action = na.omit, data = MR_ACClong)
anova(MR_ACC, test = "F")
MR_acc <- lm(Acc~Cond*Gender*vghours, MR_ACClong)
MR_acc

p<-ggplot(MR_ACClong,aes(vghours, Acc, color=Cond, group=Cond))+
  geom_smooth(method='lm', formula=y~x)+  geom_point() +
  theme_bw()
p + facet_wrap(~Gender)

##what the heck here ?

MR_ACC_long_Fem = subset(MR_ACClong,Gender =="F")
MR_ACC_Fem<-lme(Acc~Cond*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Fem)
anova(MR_ACC_Fem, test = "F")

MR_ACC_long_Male = subset(MR_ACClong,Gender =="M")
MR_ACC_Male<-lme(Acc~Cond*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = MR_ACC_long_Male)
anova(MR_ACC_Male, test = "F")

ggplot(MR_ACC_long_Fem,aes(vghours, Acc, color=Cond, group=Cond))+
  geom_smooth(method='lm', formula=y~x)+  geom_point() +
  theme_bw()

#####################################################################

getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)



data <- data%>% gather(key = "condition", value = "score" , "ctrl_acc", "allo_acc", "ego_acc")
data$score<-as.numeric(data$score)
data$Gender <- as.factor(data$Gender)
data %>% group_by(condition)
data %>% get_summary_stats(score, type = "mean_sd")
data$condition <- factor(data$condition)
mod_acc <- lm(score ~ Gender + condition + vghours+ Gender: vghours, data)
Anova(mod_acc)

MOD_acc<-lme(score~condition*Gender*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data)
anova(MOD_acc, test = "F")


# there is no effect of vghours on performance 

ggplot(data=data, aes(x=vghours, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#####################################################################

getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets/MR_beh_filtsubs_vghours.txt",header = T)


data <- data%>% gather(key = "condition", value = "rt" , "ctrl_rt", "allo_rt", "ego_rt")
data$rt<-as.numeric(data$rt)
data$Gender <- as.factor(data$Gender)
data %>% group_by(condition)
data %>% get_summary_stats(score, type = "mean_sd")
data$condition <- factor(data$condition)
mod_rt <- lm(rt ~ Gender + condition + vghours+ Gender: vghours, data)
Anova(mod_rt)

MOD_rt<-lme(rt~condition*Gender*vghours,random=~1|Sub,method="ML",na.action = na.omit, data = data)
anova(MOD_rt, test = "F")


# there is no effect of vghours on performance 

ggplot(data=data, aes(x=vghours, y=rt, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
