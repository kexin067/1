
####load required libraries####
options(digits=2)
library(tidyverse)
#library(dplyr)
library(pander)
library(cowplot)
#library(DescTools)
#library(effectsize)
library(ez)
#library(ggExtra)
library(ggplot2)
library(plyr)
library(psych)
#library(psychReport)
#library(readr)
library(reshape2)
library(Rmisc)
#library(survey)

####COMPILE DATA####

setwd("~/Desktop/results 0718")
mydir <- getwd()
files <- c(list.files(path=mydir, pattern="*.csv", all.files=TRUE, full.names=TRUE))

#create empty participant dataframe   ？？改participant-mindscode 删除了？, delete session
PPT_dat <- read.csv(text="subjectGroup,browser,version,screenWidth,screenHeight,OS,OS_lang,GMT_timestamp,local_timestamp,mindscode,link,calibration,age,gender,First language,FILE") 

#create empty DATA dataframe
ALL_dat <- read.csv(text="rowNo,subjectGroup,type,stim1,stimFormat,stimPos,responseType,head,responseOptions,required,randomBlock,button1,responsePos,pageBreak,spatialCondition,stimPos_actual,ITI_ms,timestamp,response,RT,responseCode,FILE")


for(file in files){
 #read the first two lines of participant info
  PPT_tmp <- read.csv(file, header = TRUE, skip=0, nrows=1)
  PPT_tmp$FILE <- basename(file)
  PPT_dat <- rbind(PPT_dat, PPT_tmp)
  #read the rest of the data
  ALL_tmp <- read.csv(file, header = TRUE, skip=2)
  #add file identifier
  ALL_tmp$FILE <- basename(file)
  #add to ALL_dat
  ALL_dat <- rbind(ALL_dat, ALL_tmp)
}

#Frequency in each group, the data collection is actually not equal with 45/40/39/45 in each group
#count(PPT_dat$subjectGroup)

##participates infor
#gender
#as.factor(PPT_dat$gender)
#table(PPT_dat$gender)

#first language
as.factor(PPT_dat$First.language)
PPT_dat$First.language[PPT_dat$First.language == "english"] <- "English"
PPT_dat$First.language[PPT_dat$First.language == "ENGLISH"] <- "English"
PPT_dat$First.language[PPT_dat$First.language == "English "] <- "English"
PPT_dat$First.language[PPT_dat$First.language == "hindi"] <- "Hindi"
PPT_dat$First.language[PPT_dat$First.language == "GREEK"] <- "Greek"
PPT_dat$First.language[PPT_dat$First.language == "greece"] <- "Greek"
PPT_dat$First.language[PPT_dat$First.language == "italian"] <- "Italian"
PPT_dat$First.language[PPT_dat$First.language == "portuguese"] <- "Portuguese"
PPT_dat$First.language[PPT_dat$First.language == "POrtuguese"] <- "Portuguese"
PPT_dat$First.language[PPT_dat$First.language == "spanish"] <- "Spanish"
PPT_dat$First.language[PPT_dat$First.language == "TELUGU"] <- "Telugu"
PPT_dat$First.language[PPT_dat$First.language == "Telugu "] <- "Telugu"
#count(PPT_dat$First.language)
  

#GET SESSION DURATION PER FILE####
DURATION <- aggregate(timestamp~FILE, min, data=ALL_dat)
names(DURATION)[2] <- "t_min"
DURATION <- merge(DURATION, aggregate(timestamp~FILE, max, data=ALL_dat))
names(DURATION)[3] <- "t_max"
DURATION$DUR <- DURATION$t_max - DURATION$t_min
DURATION$DUR <- DURATION$DUR/60000
#MERGE INTO PPT_DAT
PPT_dat <- merge(PPT_dat, DURATION[c(1,4)])

####COMPILE DEVICE, RESPONSE HAND, Edinburgh Handedness Inventory (EHI)########
#SPLIT OFF EHI DATA
EHI_dat <- ALL_dat[ALL_dat$type == "form", c(1,8,19:22)]
#GET DEVICE
DEVICE <- EHI_dat[EHI_dat$rowNo == 15,  c(6:5)]#(1)Mouse;(2)Touchscreen;(3)Track/Touch Pad;(4)Trackball
names(DEVICE)[2] <- "DEVICE"
#RESPONDING HAND
HAND <- EHI_dat[EHI_dat$rowNo == "15_2",c(6:5)]#(1)Left hand;(2)Right hand
names(HAND)[2] <- "HAND"
PPT_dat <- merge(PPT_dat,merge(DEVICE,HAND))

#lables? 已经尝试了dataframe 不行

#restrict to short form EHI (writing, throwing, toothbrush, spoon)
#Veale, J. F. (2014). Laterality, 19(2), 164–177. doi.org/10.1080/1357650X.2013.783045
EHI_sh <- EHI_dat[EHI_dat$rowNo %in% c(6,"6_3",9,"9_3"), ]
#recode response to percent pref (LQ)
EHI_sh$LQ <- (EHI_sh$responseCode - 3)*50
EHI <- aggregate(LQ~FILE, mean, data=EHI_sh)
names(EHI)[2] <- "LQ_SHORT"
PPT_dat <- merge(PPT_dat, EHI)

#categorise hand dominance
EHI$DOM <- cut(EHI$LQ_SHORT, breaks = c(-101,-60,60,101), labels = c("LEFT","MIXED","RIGHT"))
PPT_dat <- merge(PPT_dat, EHI)

#summary
summary(PPT_dat)

#Continuous
pptsum <- PPT_dat%>% dplyr::summarise(
    var = c("LQ_SHORT","age","DUR"),
    count = c(n(),n(),n()),
    mean = c(mean(LQ_SHORT),mean(age),mean(DUR)),
    sd = c(sd(LQ_SHORT),sd(age),sd(DUR)),
    min = c(min(LQ_SHORT),min(age),min(DUR)),
    max = c(max(LQ_SHORT),max(age),max(DUR)),
  )
pptsum %>% pander

#Categorical: group gender First.language
PPT_dat %>%
  group_by(subjectGroup) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

PPT_dat %>%
  group_by(gender) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

PPT_dat %>%
  group_by(First.language) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

PPT_dat %>%
  group_by(DEVICE) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

PPT_dat %>%
  group_by(HAND) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

PPT_dat %>%
  group_by(DOM) %>%
  dplyr::summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>% 
  pander

#reduce PPT-dat to relevant columns
PPT_dat <- PPT_dat[c(1:3,6:7,12,14:21)]

#encode between-subjects factors
PPT_dat$STIM <- factor(mapvalues(PPT_dat$subjectGroup, from=seq(1,24), to=c(rep("LINE", 12), rep("GAP", 12))))
PPT_dat$BLOCKING <- factor(mapvalues(PPT_dat$subjectGroup, from=seq(1,24), to=c(rep("BLOCKED", 6), rep("MIXED", 6),rep("BLOCKED", 6), rep("MIXED", 6))))

#restrict to bisection data 
LB_dat <- merge(PPT_dat, ALL_dat[ALL_dat$type=="test" & ALL_dat$stim1!="INTERTRIAL", c(22, 4, 18:20)])

#GET THE PIXEL POSITIONS FROM RESPONSES
LB_dat$x <- unlist(lapply(LB_dat$response, function(x) regexpr(x, pattern="x")[1]))
LB_dat$Hpix <- as.numeric(substr(LB_dat$response, LB_dat$x+1, LB_dat$x+4))
#LB_dat$Hpixcal <- LB_dat$Hpix/LB_dat$calibration

#read in STIM file
STIM <- read.csv(paste0(mydir,"/SPACIAL_STIM.tsv"), header = T)
LB_dat <- merge(LB_dat, STIM[2:5])
LB_dat$P <- LB_dat$Hpix

#IDENTIFY RESPONSES OUTSIDE LINE BOUNDARIES
LB_dat$OOB <- (LB_dat$P < LB_dat$L) | (LB_dat$P > LB_dat$R)

#calculate Directional Bisection Error (DBE)
LB_dat$DBE <- LB_dat$P - ((LB_dat$R+LB_dat$L)/2)

#create empty DV_dat
DV_dat <- read.csv(text="FILE,SPACE,rsq,k,dPL,dPR,DBE,LB_DURATION,LB_RESPTIME")

#extract left and right end-point weightings per participant per session
#iterate by participant
LB_dat$FILE <- as.factor(LB_dat$FILE)
LB_dat$SPACE <- as.factor(LB_dat$SPACE)

for(FILE in levels(LB_dat$FILE)){
  for(SPACE in levels(LB_dat$SPACE)){
    #subset
    tmp <- LB_dat[LB_dat$FILE == FILE & LB_dat$SPACE == SPACE, ]
    #regression to extract endpoint weightings
    model <- lm(P~L+R, data=tmp)
    #extract DVs from model
    rsq <- summary(model)$r.squared
    k <-  as.numeric(coefficients(model)[1])
    dPL <- as.numeric(coefficients(model)[2])
    dPR <- as.numeric(coefficients(model)[3])
    #calculate DBE per stimlus line configuration then average
    DBE_means <- aggregate(DBE~stim1, mean, data=tmp)
    DBE <- mean(DBE_means$DBE)
    #duration of block
    LB_DURATION <- (max(tmp$timestamp)-min(tmp$timestamp))/60000#converting ms to minutes
    LB_RESPTIME <- median(tmp$RT)
    #add to DV_dat
    DV_dat <- rbind(DV_dat, cbind.data.frame(FILE,SPACE,rsq,k,dPL,dPR,DBE,LB_DURATION,LB_RESPTIME))
  }
}

#calculate endpoints composite measures (EWB & EWS)
DV_dat$EWB <- DV_dat$dPR - DV_dat$dPL
DV_dat$EWS <- DV_dat$dPL + DV_dat$dPR

#merge with PPT_dat
DV_dat <- merge(PPT_dat, DV_dat)

#apply exclusion criteria
DV_dat$FILTER <- DV_dat$rsq >= .7 & DV_dat$EWS > .5 & abs(DV_dat$EWB) < .5

#visualise exclusions 加 离群点？？
ggplot(DV_dat)+geom_point(aes(x=dPR, y=dPL, shape=FILTER, colour=FILTER), size=2, alpha=.6) +
  scale_shape_manual(values=c(16,1))+
  geom_abline(intercept=0, slope=1)+
  theme_bw() 


#check participant validity 排查参与者有效性
XCLUDE <- dcast(mindsCode~SPACE, value.var = "FILTER", data=DV_dat)
XCLUDE <- XCLUDE[!(XCLUDE$LEFT & XCLUDE$MID& XCLUDE$RIGHT), "mindsCode"]
length(XCLUDE)
XCLUDE

#make exclusions
DV_dat <- DV_dat[!(DV_dat$mindsCode %in% XCLUDE), 1:26]

#drop excluded participant levels
DV_dat$mindsCode <- droplevels(DV_dat$mindsCode)
nlevels(DV_dat$mindsCode)

#how many valid datapoints
table(DV_dat$BLOCKING)/3

#bias值
describe(DV_dat$DBE)
describe(DV_dat$EWB)
describe(DV_dat$EWS)
t.test(DV_dat$DBE, mu = 0)

t.test(DV_dat$EWS, mu = 1)

##EFFECT SIZES
cohen.d.ci(d = mean(DV_dat$DBE, na.rm=TRUE)/sd(DV_dat$DBE, na.rm=TRUE), n1 = sum(!is.na(DV_dat$DBE)))
cohen.d.ci(d = mean(DV_dat$EWB, na.rm=TRUE)/sd(DV_dat$EWB, na.rm=TRUE), n1 = sum(!is.na(DV_dat$EWB)))
#one way t-tests of pseudoneglect per variable
t.test(DV_dat$DBE, mu = 0);
t.test(DV_dat$EWB, mu = 0)
#full reports for CIs etc
cor.test(DV_dat$DBE, DV_dat$EWS)

EWS
t.test(DV_dat$EWS, mu = 1)


sum <-DV_dat %>%
  dplyr::group_by(SPACE) %>%
  dplyr:summarise(
    mean.DBE=mean(DBE, na.rm=TRUE),
    sd.DBE = sd(DBE, na.rm=TRUE),
    mean.EWB=mean(EWB, na.rm=TRUE),
    sd.EWB = sd(EWB, na.rm=TRUE)) 
sum %>% pander

pptsum <- PPT_dat%>% dplyr::summarise(
  var = c("LQ_SHORT","age","DUR"),
  count = c(n(),n(),n()),
  mean = c(mean(LQ_SHORT),mean(age),mean(DUR)),
  sd = c(sd(LQ_SHORT),sd(age),sd(DUR)),
  min = c(min(LQ_SHORT),min(age),min(DUR)),
  max = c(max(LQ_SHORT),max(age),max(DUR)),
)
pptsum %>% pander

aggregate(DV_dat$DBE,by=list(DV_dat$SPACE),
                FUN=mean)
aggregate(DV_dat$EWS,by=list(DV_dat$SPACE),
          FUN=mean)
aggregate(DV_dat$EWB,by=list(DV_dat$SPACE),
          FUN=mean)
          




ggplot(DV_dat, aes(x=DBE))+geom_histogram()+facet_grid(STIM~SPACE)+
  geom_vline(xintercept=0, linetype="dotted")+ theme_bw()
ggplot(DV_dat, aes(x=DBE, fill=SPACE))+geom_density(alpha=.5)+
  geom_vline(xintercept=0, linetype="dotted")+ theme_bw()

ggplot(DV_dat, aes(x=EWB))+geom_histogram()+facet_grid(STIM~SPACE)+
  geom_vline(xintercept=0, linetype="dotted")+ theme_bw()
ggplot(DV_dat, aes(x=EWB, fill=SPACE))+geom_density(alpha=.5)+
  geom_vline(xintercept=0, linetype="dotted")+ theme_bw()



ggplot(DV_dat, aes(x=EWS))+geom_histogram()+facet_grid(STIM~SPACE)+
  geom_vline(xintercept=1, linetype="dotted")+ theme_bw()
ggplot(DV_dat, aes(x=EWS, fill=SPACE))+geom_density(alpha=.5)+
  geom_vline(xintercept=1, linetype="dotted")+ theme_bw()


#summarise means and CIs
DBE_means <- summarySE(data = DV_dat, measurevar = "DBE", groupvars = c("STIM", "BLOCKING", "SPACE"))
ggplot(DBE_means, aes(x=SPACE, y=DBE, colour=BLOCKING, group=BLOCKING))+
  geom_hline(yintercept=0, linetype="dotted")+
  geom_point(size=5, alpha=.7, position=position_dodge(width=.2))+
  geom_line(size=1, position=position_dodge(width=.2))+
  geom_errorbar(aes(ymin=DBE-ci, ymax=DBE+ci), width=.2, position=position_dodge(width=.2))+
  facet_wrap(~STIM)+
  theme_bw()+
  theme(panel.grid = element_blank())

EWB_means <- summarySE(data = DV_dat, measurevar = "EWB", groupvars = c("STIM", "BLOCKING", "SPACE"))
EWB_means$SPACE <- factor(EWB_means$SPACE, levels=c("LEFT", "MID", "RIGHT"))
ggplot(EWB_means, aes(x=SPACE, y=EWB, colour=BLOCKING, group=BLOCKING))+
  geom_hline(yintercept=0, linetype="dotted")+
  geom_point(size=5, alpha=.7, position=position_dodge(width=.2))+
  geom_line(size=1, position=position_dodge(width=.2))+
  geom_errorbar(aes(ymin=EWB-ci, ymax=EWB+ci), width=.2, position=position_dodge(width=.2))+
  facet_wrap(~STIM)+
  theme_bw()+
  theme(panel.grid = element_blank())

EWS_means <- summarySE(data = DV_dat, measurevar = "EWS", groupvars = c("STIM", "BLOCKING", "SPACE"))
EWS_means$SPACE <- factor(EWS_means$SPACE, levels=c("LEFT", "MID", "RIGHT"))
ggplot(EWS_means, aes(x=SPACE, y=EWS, colour=BLOCKING, group=BLOCKING))+
  geom_hline(yintercept=1, linetype="dotted")+
  geom_point(size=5, alpha=.7, position=position_dodge(width=.2))+
  geom_line(size=1, position=position_dodge(width=.2))+
  geom_errorbar(aes(ymin=EWS-ci, ymax=EWS+ci), width=.2, position=position_dodge(width=.2))+
  facet_wrap(~STIM)+
  theme_bw()+
  theme(panel.grid = element_blank())




#ANOVA

DBE_ANOVA <- ezANOVA(data=DV_dat
                     , dv = "DBE"
                     , wid = .("mindsCode")
                     , within = .("SPACE")
                     , between = .("STIM", "BLOCKING")
                     , type = 3
                     , return_aov = T)


print(DBE_ANOVA)

EWB_ANOVA <- ezANOVA(data=DV_dat
                     , dv = "EWB"
                     , wid = .("mindsCode")
                     , within = .("SPACE")
                     , between = .("STIM", "BLOCKING")
                     , type = 3
                     , return_aov = T)

print(EWB_ANOVA)

EWS_ANOVA <- ezANOVA(data=DV_dat
                     , dv = "EWS"
                     , wid = .("mindsCode")
                     , within = .("SPACE")
                     , between = .("STIM", "BLOCKING")
                     , type = 3
                     , return_aov = T)

print(EWS_ANOVA)




##########################################################












###在做一次数据描述#####

summary(DV_dat)

#group gender First.language
DV_dat %>%
  group_by(gender) %>%
  dplyr::summarise(n = n()/3) %>%
  mutate(freq = n / sum(n))%>% 
  pander

DV_dat %>%
  group_by(DEVICE) %>%
  dplyr::summarise(n = n()/3) %>%
  mutate(freq = n / sum(n))%>% 
  pander

DV_dat %>%
  group_by(HAND) %>%
  dplyr::summarise(n = n()/3) %>%
  mutate(freq = n / sum(n))%>% 
  pander

DV_dat %>%
  group_by(DOM) %>%
  dplyr::summarise(n = n()/3) %>%
  mutate(freq = n / sum(n))%>% 
  pander


### 之前的数据集 ###
DBE <- dcast(FILE+SPACE~STIM+BLOCKING, value.var = "DBE", data = DV_dat)
names(DBE)[3:6] <- c("DBE_GB", "DBE_GM","DBE_LB", "DBE_LM")
EWB <- dcast(FILE+SPACE~STIM+BLOCKING, value.var = "EWB", data = DV_dat)
names(EWB)[3:6] <- c("EWB_GB", "EWB_GM","EWB_LB", "EWB_LM")
EWS <- dcast(FILE+SPACE~STIM+BLOCKING, value.var = "EWS", data = DV_dat)
names(EWS)[3:6] <- c("EWS_GB", "EWS_GM","EWS_LB", "EWS_LM")


#merge into REDuced df
RED <- merge(DBE,EWB, all = TRUE)
RED <- merge(RED,EWS, all = TRUE)

#ADD IN THE HAND EXCLUSIONS TO REDUCED DATA
RED <- merge(RED, HAND[1:2])

#REPORT DEVICES
##(1)Mouse;(2)Touchscreen;(3)Track/Touch Pad;(4)Trackball
#table(DV_dat$DEVICE)/nrow(DV_dat)

####write output files if uncommented####
#write.csv(DV_dat,"PSEUDO_data_processed_complete.csv", row.names = FALSE)
#write.csv(RED,"PSEUDO_data_processed_reduced.csv", row.names = FALSE)

#####DEMOGRAPHIC DESCRIPTIVES FOR COMPLETE SAMPLE####
#table(DV_dat[DV_dat$STIM, DV_dat$SPACE,"gender"])
#describe(DV_dat[DV_dat$SESSION=="Session_1", "AGE_YRS"])
#describe(DV_dat[DV_dat$SESSION=="Session_1", "EHI_LQ"])
#table(DV_dat[DV_dat$SESSION=="Session_1", "DOM"])
#table(DV_dat$HAND)
#table(DV_dat$DEVICE)

#####REPORT EXCLUSIONS####


#####TIMING DESCRIPTIVES####
#for full sample

#for valid blocks  ?

#####BIAS ANALYSIS####
describe(RED$DBE_GB);describe(RED$DBE_GM)
describe(RED$DBE_LB);describe(RED$DBE_LM)
describe(RED$EWB_GB);describe(RED$EWB_GM)
describe(RED$EWB_LB);describe(RED$EWB_LB)
#EWS

##EFFECT SIZES
cohen.d.ci(d = mean(RED$DBE_GB, na.rm=TRUE)/sd(RED$DBE_GB, na.rm=TRUE), n1 = sum(!is.na(RED$DBE_GB)))
cohen.d.ci(d = mean(RED$DBE_GM, na.rm=TRUE)/sd(RED$DBE_GM, na.rm=TRUE), n1 = sum(!is.na(RED$DBE_GM)))
cohen.d.ci(d = mean(RED$DBE_LB, na.rm=TRUE)/sd(RED$DBE_LB, na.rm=TRUE), n1 = sum(!is.na(RED$DBE_LB)))
cohen.d.ci(d = mean(RED$DBE_LM, na.rm=TRUE)/sd(RED$DBE_LM, na.rm=TRUE), n1 = sum(!is.na(RED$DBE_LM)))
cohen.d.ci(d = mean(RED$EWB_GB, na.rm=TRUE)/sd(RED$EWB_GB, na.rm=TRUE), n1 = sum(!is.na(RED$EWB_GB)))
cohen.d.ci(d = mean(RED$EWB_GM, na.rm=TRUE)/sd(RED$EWB_GM, na.rm=TRUE), n1 = sum(!is.na(RED$EWB_GM)))
cohen.d.ci(d = mean(RED$EWB_LB, na.rm=TRUE)/sd(RED$EWB_LB, na.rm=TRUE), n1 = sum(!is.na(RED$EWB_LB)))
cohen.d.ci(d = mean(RED$EWB_LM, na.rm=TRUE)/sd(RED$EWB_LM, na.rm=TRUE), n1 = sum(!is.na(RED$EWB_LM)))


#one way t-tests of pseudoneglect per session per variable
t.test(RED$DBE_GB, mu = 0);t.test(RED$DBE_GM, mu = 0)
t.test(RED$DBE_LB, mu = 0);t.test(RED$DBE_LM, mu = 0)
t.test(RED$EWB_GB, mu = 0);t.test(RED$EWB_GM, mu = 0)
t.test(RED$EWB_LB, mu = 0);t.test(RED$EWB_LM, mu = 0)

#test-retest reliability per variable
#cor.test(RED[RED$HAND_FILTER==TRUE, "DBE1"], RED[RED$HAND_FILTER==TRUE, "DBE2"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "EWB1"], RED[RED$HAND_FILTER==TRUE, "EWB2"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "PSE1"], RED[RED$HAND_FILTER==TRUE, "PSE2"])

#test-retest agreement per variable (Lin's Concordance Correlation Coefficient, from DescTools)
#Lin L (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics 45: 255 - 268.
#CCC(x=RED$DBE1, y=RED$DBE2, na.rm = TRUE)$rho.c
#CCC(x=RED$EWB1, y=RED$EWB2, na.rm = TRUE)$rho.c
#CCC(x=RED$PSE1, y=RED$PSE2, na.rm = TRUE)$rho.c
#CCC(x=RED$EWS1, y=RED$EWS2, na.rm = TRUE)$rho.c

#external reliability per session
#cor(RED[c(2,4,6)], use="pairwise.complete.obs")
#cor(RED[c(3,5,7)], use="pairwise.complete.obs")

#full reports for CIs etc
#cor.test(RED[RED$HAND_FILTER==TRUE, "DBE1"], RED[RED$HAND_FILTER==TRUE, "EWB1"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "DBE1"], RED[RED$HAND_FILTER==TRUE, "PSE1"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "EWB1"], RED[RED$HAND_FILTER==TRUE, "PSE1"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "DBE2"], RED[RED$HAND_FILTER==TRUE, "EWB2"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "DBE2"], RED[RED$HAND_FILTER==TRUE, "PSE2"])
#cor.test(RED[RED$HAND_FILTER==TRUE, "EWB2"], RED[RED$HAND_FILTER==TRUE, "PSE2"])

#####EWS ANALYSIS####
describe(RED$EWS_LB);describe(RED$EWS_LM)
t.test(RED$EWS_LB, mu = 1);t.test(RED$EWS_LM, mu = 1)

#cor.test(RED[RED$HAND_FILTER==TRUE, "EWS1"], RED[RED$HAND_FILTER==TRUE, "EWB1"])
#cor(RED[c(2,4,6,8)], use="pairwise.complete.obs")
#cor(RED[c(3,5,7,9)], use="pairwise.complete.obs")

####PLOT FIGURE 1####
df <- RED[RED$HAND_FILTER==TRUE,]

#DBE#
Rlabel <- paste0("(a) DBE\n r = ", round(cor(df$DBE1, df$DBE2, use="pairwise.complete.obs"),2),"\n CCC = ", round(as.numeric(CCC(df$DBE1, df$DBE2, na.rm = TRUE)$rho.c[1]),2))
ggplot(df, aes(x=DBE1,y=DBE2))+
  scale_x_continuous(name="", limits = c(-8,8), breaks=seq(-6,6,4))+
  scale_y_continuous(name="Session 2", limits = c(-8,8), breaks=seq(-6,6,4))+
  geom_abline(intercept=0, slope=1, linetype="dotted", size = 1)+
  geom_hline(yintercept=0, colour="grey50", size = .5)+
  geom_vline(xintercept=0, colour="grey50")+
  geom_point(size=2.5, alpha=.4, colour = 'grey20')+
  geom_smooth(method=lm, colour="black", size=1)+
  annotate("text", x = -8, y = 6, label = Rlabel, hjust = "left", size=6) +
  theme_bw() + 
  theme(aspect.ratio = 1,
        title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank()) -> Fig1a

ggMarginal(Fig1a, type='histogram', fill = 'grey') -> Fig1a


#EWB#
Rlabel <- paste0("(b) EWB\n r = ", round(cor(df$EWB1, df$EWB2, use="pairwise.complete.obs"),2),"\n CCC = ", round(as.numeric(CCC(df$EWB1, df$EWB2, na.rm = TRUE)$rho.c[1]),2))
ggplot(df, aes(x=EWB1,y=EWB2))+
  scale_x_continuous(name="", limits = c(-.15,.15), breaks=seq(-.15,.15,.1))+
  scale_y_continuous(name="", limits = c(-.15,.15), breaks=seq(-.15,.15,.1))+
  geom_abline(intercept=0, slope=1, linetype="dotted", size = 1)+
  geom_hline(yintercept=0, colour="grey50", size = .5)+
  geom_vline(xintercept=0, colour="grey50")+
  geom_point(size=2.5, alpha=.4, colour = 'grey20')+
  geom_smooth(method=lm, colour="black", size=1)+
  annotate("text", x = -.15, y = .11, label = Rlabel, hjust = "left", size=6) +
  theme_bw() + 
  theme(aspect.ratio = 1,
        title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank()) -> Fig1b

ggMarginal(Fig1b, type='histogram', fill = 'grey') -> Fig1b

#PSE#
Rlabel <- paste0("(c) PSE\n r = ", round(cor(df$PSE1, df$PSE2, use="pairwise.complete.obs"),2),"\n CCC = ", round(as.numeric(CCC(df$PSE1, df$PSE2, na.rm = TRUE)$rho.c[1]),2))
ggplot(df, aes(x=PSE1,y=PSE2))+
  scale_x_continuous(name="Session 1", limits = c(-8,8), breaks=seq(-6,6,4))+
  scale_y_continuous(name="Session 2", limits = c(-8,8), breaks=seq(-6,6,4))+
  geom_abline(intercept=0, slope=1, linetype="dotted", size = 1)+
  geom_hline(yintercept=0, colour="grey50", size = .5)+
  geom_vline(xintercept=0, colour="grey50")+
  geom_point(size=2.5, alpha=.4, colour = 'grey20')+
  geom_smooth(method=lm, colour="black", size=1)+
  annotate("text", x = -8, y = 6, label = Rlabel, hjust = "left", size=6) +
  theme_bw() + 
  theme(aspect.ratio = 1,
        title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank()) -> Fig1c

ggMarginal(Fig1c, type='histogram', fill = 'grey') -> Fig1c

#EWS#
Rlabel <- paste0("(d) EWS\n r = ", round(cor(df$EWS1, df$EWS2, use="pairwise.complete.obs"),2),"\n CCC = ", round(as.numeric(CCC(df$EWS1, df$EWS2, na.rm = TRUE)$rho.c[1]),2))
ggplot(df, aes(x=EWS1,y=EWS2))+
  scale_x_continuous(name="Session 1", limits = c(.8,1.3), breaks=seq(.8,1.3,.1))+
  scale_y_continuous(name="", limits = c(.8,1.3), breaks=seq(.8,1.3,.1))+
  geom_abline(intercept=0, slope=1, linetype="dotted", size = 1)+
  geom_hline(yintercept=1, colour="grey50", size = .5)+
  geom_vline(xintercept=1, colour="grey50")+
  geom_point(size=2.5, alpha=.4, colour = 'grey20')+
  geom_smooth(method=lm, colour="black", size=1)+
  annotate("text", x = .8, y = 1.23, label = Rlabel, hjust = "left", size=6) +
  theme_bw() + 
  theme(aspect.ratio = 1,
        title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid = element_blank()) -> Fig1d

ggMarginal(Fig1d, type='histogram', fill = 'grey') -> Fig1d

#output Fig 1#
tiff("PSEUDO_Fig1.tif", units = "in", width = 11.5, height=11.5, res=300)
plot_grid(Fig1a, Fig1b, Fig1c, Fig1d, ncol=2, align="v")
dev.off()

#####EXPLORE EWS and TRIAL_TIME by HALF_BLOCK by SESSION#####
#add in half (1st 40 vs 2nd 40 trials in a block)
LB_dat$HALF <- rep(c(rep(1,40),rep(2,40)), 479)

#empty DV_dat
HALF_dat <- read.csv(text="PPT,SESSION,HALF,rsq,k,sPL,dPR,DBE,LB_RESPTIME")

for(PPT in levels(LB_dat$PPT)){
  for(SESSION in levels(LB_dat$SESSION)){
    for(HALF in c(1,2)){
      #select PPT data
      tmp <- LB_dat[LB_dat$PPT == PPT & LB_dat$SESSION == SESSION & LB_dat$HALF==HALF, ]
      if(nrow(tmp) > 0){
        #regression to extract endpoint weightings
        model <- lm(P~L+R, data=tmp)
        #extract DVs from model
        rsq <- summary(model)$r.squared
        k <-  as.numeric(coefficients(model)[1])
        dPL <- as.numeric(coefficients(model)[2])
        dPR <- as.numeric(coefficients(model)[3])
        #calculate DBE per condition then average
        DBE_means <- aggregate(DBE~stim1, mean, data=tmp)
        DBE <- mean(DBE_means$DBE)
        #median trial duration
        #LB_DURATION <- (max(tmp$timestamp)-min(tmp$timestamp))/60000#converting ms to minutes
        LB_RESPTIME <- median(tmp$RT)
        #add to DV_dat
        HALF_dat <- rbind(HALF_dat, cbind.data.frame(PPT,SESSION,HALF,rsq,k,dPL,dPR,DBE,LB_RESPTIME))
      }
    }
  }
}

#exclude calibration (DPI) filter 
HALF_dat <- merge(HALF_dat, DV_dat[DV_dat$SESSION=="Session_1",c(1,30)])
HALF_dat <- HALF_dat[HALF_dat$DPI_FILTER==TRUE, c(1:9)]

#CREATE COMPOSITE VARIABLES
HALF_dat$SESSION <- factor(HALF_dat$SESSION)
HALF_dat$HALF <- factor(HALF_dat$HALF)
HALF_dat$EWB <- HALF_dat$dPR - HALF_dat$dPL
HALF_dat$EWS <- HALF_dat$dPL + HALF_dat$dPR

#FILTER INADEQUATE FITS
HALF_dat$LB_FILTER <- (HALF_dat$rsq >= .7 & HALF_dat$EWS > .5 & abs(HALF_dat$EWB) < .5)

#keep only participants with complete data at all four timepoints
COMPLETE <- aggregate(LB_FILTER~PPT, sum, data=HALF_dat)
COMPLETE <- as.vector(COMPLETE[COMPLETE$LB_FILTER==4, "PPT"])
HALF_dat <- HALF_dat[HALF_dat$PPT %in% COMPLETE, c(1:11)]

#plot EWS
EWS_plot <- summarySEwithin(data = HALF_dat, measurevar = "EWS", withinvars = c("SESSION", "HALF"), idvar = "PPT")
ggplot(EWS_plot, aes(x=HALF, y=EWS, colour=SESSION, group=SESSION))+
  scale_color_manual(values=c("black", "darkgrey"))+
  geom_point(size=5)+
  geom_line()+
  geom_errorbar(aes(ymin=EWS-ci, ymax=EWS+ci), width=.2)+
  scale_x_discrete(labels=c("first","second"))+
  labs(x="Half-block", y="EWS")+
  ggtitle("(a)")+
  theme_bw() +
  theme(aspect.ratio=1, axis.title = element_text(size=16),axis.text = element_text(size=12), panel.grid = element_blank(), axis.ticks.x = element_blank(),
        title = element_text(size=16), legend.title = element_blank(), legend.position = c(.85,.88), legend.background = element_rect(fill="transparent")) -> Fig2a

#RESPTIME by HALF-SESSION
#visualise duration distributions (positively skewed)
ggplot(HALF_dat, aes(x=LB_RESPTIME, fill=HALF))+geom_histogram(alpha=.5)+facet_wrap(~SESSION)
#log transform and visualise transformed distributions
HALF_dat$lgTT <- log10(HALF_dat$LB_RESPTIME)
ggplot(HALF_dat, aes(x=lgTT, fill=HALF))+geom_histogram(alpha=.5)+facet_wrap(~SESSION)

#plot lgTT
TT_plot <- summarySEwithin(data = HALF_dat, measurevar = "lgTT", withinvars = c("SESSION", "HALF"), idvar = "PPT")
ggplot(TT_plot, aes(x=HALF, y=lgTT, colour=SESSION, group=SESSION))+
  geom_point(size=5)+
  geom_line()+
  geom_errorbar(aes(ymin=lgTT-ci, ymax=lgTT+ci), width=.2)+
  scale_color_manual(values=c("black", "darkgrey"))+
  #adjust scale y to display ticks and labels in seconds
  scale_y_continuous(limits = c(3.230449, 3.342423), breaks=c(3.230449, 3.255273, 3.278754, 3.30103, 3.322219, 3.342423), labels=c("1.7","1.8","1.9","2.0","2.1","2.2"))+
  scale_x_discrete(labels=c("first","second"))+
  labs(x="Half-block", y="Bisection response time (sec)")+
  ggtitle("(b)")+
  theme_bw()+
  theme(aspect.ratio=1, axis.title = element_text(size=16),axis.text = element_text(size=12), panel.grid = element_blank(), axis.ticks.x = element_blank(),
        title = element_text(size=16), legend.title = element_blank(), legend.position = c(.85,.88), legend.background = element_rect(fill="transparent")) -> Fig2b
Fig2b
#output Fig 3#
tiff("PSEUDO_Fig2.tif", units="in", width=10, height=5, res=300)
plot_grid(Fig2a, Fig2b, ncol=2, align = "h")
dev.off()

#output data if uncommented#
write.csv(HALF_dat, "PSEUDO_by_HALF_SESSION.csv", row.names = FALSE)

####ANOVA by HALF-BLOCK AND SESSION####
#EWS#
EWS_ANOVA <- ezANOVA(data=HALF_dat
                     ,dv = .(EWS)
                     ,wid = .(PPT)
                     ,within = .(SESSION, HALF)
                     ,type = 3
                     ,detailed = TRUE
                     ,return_aov = TRUE
)

EWS_ANOVA$ANOVA
#get partial eta squared
aovEWS <- aovEffectSize(ezObj = EWS_ANOVA, effectSize = "pes")
aovDispTable(aovEWS)
#calculate effect sizes for main effects
EWS_SESSION <- aggregate(EWS~SESSION*PPT, mean, data=HALF_dat)
EWS_SESSION <- dcast(PPT~SESSION, value.var = "EWS", data=EWS_SESSION)
EWS_SESSION$D <- EWS_SESSION$Session_1-EWS_SESSION$Session_2
mean(EWS_SESSION$D); sd(EWS_SESSION$D); mean(EWS_SESSION$D)/sd(EWS_SESSION$D)
EWS_HALF <- aggregate(EWS~HALF*PPT, mean, data=HALF_dat)
EWS_HALF <- dcast(PPT~HALF, value.var = "EWS", data=EWS_HALF)
EWS_HALF$D <- EWS_HALF$`1`-EWS_HALF$`2`
mean(EWS_HALF$D); sd(EWS_HALF$D); mean(EWS_HALF$D)/sd(EWS_HALF$D)

#TRIAL TIME#
TT_ANOVA <- ezANOVA(data=HALF_dat
                    ,dv = .(lgTT)
                    ,wid = .(PPT)
                    ,within = .(SESSION, HALF)
                    ,type = 3
                    ,detailed = TRUE
                    ,return_aov = TRUE
)

TT_ANOVA$ANOVA
#get partial eta squared
aovTT <- aovEffectSize(ezObj = TT_ANOVA, effectSize = "pes")
aovDispTable(aovTT)
#calculate effect sizes for main effects
TT_SESSION <- aggregate(lgTT~SESSION*PPT, mean, data=HALF_dat)
TT_SESSION <- dcast(PPT~SESSION, value.var = "lgTT", data=TT_SESSION)
TT_SESSION$D <- TT_SESSION$Session_1-TT_SESSION$Session_2
mean(TT_SESSION$D); sd(TT_SESSION$D); mean(TT_SESSION$D)/sd(TT_SESSION$D)
TT_HALF <- aggregate(lgTT~HALF*PPT, mean, data=HALF_dat)
TT_HALF <- dcast(PPT~HALF, value.var = "lgTT", data=TT_HALF)
TT_HALF$D <- TT_HALF$`1`-TT_HALF$`2`
mean(TT_HALF$D); sd(TT_HALF$D); mean(TT_HALF$D)/sd(TT_HALF$D)

cor.test(EWS_HALF$D, TT_HALF$D, method="p")
cor.test(EWS_SESSION$D, TT_SESSION$D, method="p")

#####ANALYSIS of BISECTION BIAS BY EPOCH#####

#GET LIST OF PARTICIPANTS TO EXCLUDE FOR TEST_RETEST
XCLUDE_HAND <- RED[RED$HAND_FILTER==FALSE, "PPT"]

#REMOVE DPI FILTER from LB_dat
XCLUDE_DPI <- DV_dat[DV_dat$DPI_FILTER==FALSE, c("PPT","SESSION")]
LB_dat <- LB_dat[!((LB_dat$PPT %in% XCLUDE_DPI$PPT) & (LB_dat$SESSION %in% XCLUDE_DPI$SESSION)), ]

#empty EPOCH_dat
EPOCH_dat <- read.csv(text="epochs,n1,n2,nTRT,DBE1_mean,DBE2_mean,EWB1_mean,EWB2_mean,DBE1_sd,DBE2_sd,EWB1_sd,EWB2_sd,DBE_r,EWB_r")

#recalculate for different epoch numbers (2:20), where 1 epoch = 4 trials (1 per bisection stimulus)
for(epochs in 2:20){
  
  #empty DV_dat
  DV_dat <- read.csv(text="PPT,SESSION,rsq,k,sPL,dPR,DBE,LB_DURATION,LB_RESPTIME")
  
  #iterate by participant
  for(PPT in levels(LB_dat$PPT)){
    #select PPT data
    tmp1 <- LB_dat[LB_dat$PPT == PPT, ]
    #iterate by session
    for(SESSION in levels(factor(tmp1$SESSION))){
      tmp2 <- tmp1[tmp1$SESSION == SESSION, ]
      #restrict to epoch
      tmp2 <- tmp2[1:(epochs*4), ]
      #regression to extract endpoint weightings
      model <- lm(P~L+R, data=tmp2)
      #extract DVs from model
      rsq <- summary(model)$r.squared
      k <-  as.numeric(coefficients(model)[1])
      dPL <- as.numeric(coefficients(model)[2])
      dPR <- as.numeric(coefficients(model)[3])
      #calculate DBE per condition then average
      DBE_means <- aggregate(DBE~stim1, mean, data=tmp2)
      DBE <- mean(DBE_means$DBE)
      #duration of block
      LB_DURATION <- (max(tmp2$timestamp)-min(tmp2$timestamp))/60000#converting ms to minutes
      LB_RESPTIME <- median(diff(tmp2$timestamp))
      #add to DV_dat
      DV_dat <- rbind(DV_dat, cbind.data.frame(PPT,SESSION,rsq,k,dPL,dPR,DBE,LB_DURATION,LB_RESPTIME))
    }
  }
  
  #calculate endpoints composite measures
  DV_dat$EWB <- DV_dat$dPR - DV_dat$dPL
  DV_dat$EWS <- DV_dat$dPL + DV_dat$dPR
  
  #FILTER INADEQUATE AND IMPLAUSIBLE FITS
  DV_dat$LB_FILTER <- DV_dat$rsq >= .7 & DV_dat$EWS > .5 & abs(DV_dat$EWB) < .5
  DV_dat <- DV_dat[DV_dat$LB_FILTER == TRUE, ]
  
  #calculate summary variables
  n1 <- as.numeric(nrow(DV_dat[DV_dat$SESSION=="Session_1",]))
  n2 <- as.numeric(nrow(DV_dat[DV_dat$SESSION=="Session_2",]))
  DBE1_mean <- mean(DV_dat[DV_dat$SESSION=="Session_1", "DBE"])
  DBE2_mean <- mean(DV_dat[DV_dat$SESSION=="Session_2", "DBE"])
  EWB1_mean <- mean(DV_dat[DV_dat$SESSION=="Session_1", "EWB"])
  EWB2_mean <- mean(DV_dat[DV_dat$SESSION=="Session_2", "EWB"])
  DBE1_sd <- sd(DV_dat[DV_dat$SESSION=="Session_1", "DBE"])
  EWB1_sd <- sd(DV_dat[DV_dat$SESSION=="Session_1", "EWB"])
  DBE2_sd <- sd(DV_dat[DV_dat$SESSION=="Session_2", "DBE"])
  EWB2_sd <- sd(DV_dat[DV_dat$SESSION=="Session_2", "EWB"])
  #test-retest reliability
  #EXCLUDE INCONSISTENT HAND USE
  DV_dat <- DV_dat[!(DV_dat$PPT %in% XCLUDE_HAND), ]
  DBEwide <- dcast(PPT~SESSION, value.var = "DBE", data=DV_dat)
  DBE_r <- cor(DBEwide$Session_1, DBEwide$Session_2, use="pairwise.complete.obs")
  EWBwide <- dcast(PPT~SESSION, value.var = "EWB", data=DV_dat)
  EWB_r <- cor(EWBwide$Session_1, EWBwide$Session_2, use="pairwise.complete.obs")
  nTRT <- sum(complete.cases(EWBwide))
  
  tmp <- cbind.data.frame(epochs,n1,n2,nTRT,DBE1_mean,DBE2_mean,EWB1_mean,EWB2_mean,
                          DBE1_sd,DBE2_sd,EWB1_sd,EWB2_sd,DBE_r,EWB_r)
  
  EPOCH_dat <-rbind(EPOCH_dat, tmp)
}

#calculate effect size
EPOCH_dat$DBE1_d <- EPOCH_dat$DBE1_mean/EPOCH_dat$DBE1_sd
EPOCH_dat$DBE2_d <- EPOCH_dat$DBE2_mean/EPOCH_dat$DBE2_sd
EPOCH_dat$EWB1_d <- EPOCH_dat$EWB1_mean/EPOCH_dat$EWB1_sd
EPOCH_dat$EWB2_d <- EPOCH_dat$EWB2_mean/EPOCH_dat$EWB2_sd

#critical d value given minimum n
crit_t <- qt(p = .025, lower.tail = TRUE, df = min(c(EPOCH_dat$n1, EPOCH_dat$n2)-1))
crit_d <- as.numeric(t_to_d(t = crit_t, paired = T, df_error = min(c(EPOCH_dat$n1, EPOCH_dat$n2)-1))[1])

#make dataframes for plotting
EPOCH_d <- melt(data=EPOCH_dat, id.vars = "epochs", measure.vars = c("DBE1_d", "DBE2_d", "EWB1_d", "EWB2_d"))
EPOCH_d$variable <- factor(EPOCH_d$variable, labels=c("DBE Session 1", "DBE Session 2", "EWB Session 1", "EWB Session 2"))

#test-retest reliability
EPOCH_r <- melt(data=EPOCH_dat, id.vars = "epochs", measure.vars = c("DBE_r", "EWB_r"))
EPOCH_r$variable <- factor(EPOCH_r$variable, labels=c("DBE", "EWB"))

#plot d values
ggplot(EPOCH_d, aes(x=epochs, y=value, colour=variable, linetype=variable))+
  geom_line(size=1)+
  geom_hline(yintercept=crit_d, linetype="dotted")+
  scale_color_manual(values=c("grey50", "grey50", "black", "black"))+
  scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed"))+
  scale_x_continuous(name = "Trial epochs", limits = c(1,20), breaks=seq(2,20,2))+
  scale_y_reverse(name = "Standardised effect size (d)", limits= c(0,-.4))+
  ggtitle("(a)")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(0.78, 0.17), legend.title = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        legend.text = element_text(size=12),
        axis.title = element_text(size=14), title = element_text(size=16),
        aspect.ratio = .9) -> Fig3a

#plot test-retest reliability
ggplot(EPOCH_r, aes(x=epochs, y=value, colour=variable))+
  geom_line(size=1)+
  scale_color_manual(values=c("grey50", "black"), labels=c("DBE", "EWB"))+
  scale_x_continuous(name = "Trial epochs", limits = c(1,20), breaks=seq(2,20,2))+
  scale_y_continuous(name = "Test-retest reliability (r)", limits = c(0.2,.8), breaks=seq(.2,.8,.1))+
  geom_hline(yintercept=.65, linetype="dotted")+
  ggtitle("(b)")+
  theme_bw()+
  theme(panel.grid = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = c(0.87, 0.15), legend.title = element_blank(),
        legend.background = element_rect(fill = NA, color = NA),
        legend.text = element_text(size=12),
        title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        aspect.ratio = .98) -> Fig3b

#output Fig 3#
tiff("PSEUDO_Fig3.tif", units="in", width=10, height=5, res=300)
plot_grid(Fig3a, Fig3b, ncol=2, align = "h")
dev.off()

#output summary data by epoch
write.csv(EPOCH_dat, "PSEUDO__by_EPOCH.csv", row.names = FALSE)

#####tidy up this unholy mess####
rm(ALL_dat, ALL_tmp, PPT_tmp, TEST_dat, file, files, DURATION, DBE_means, DBE, dPL, dPR, k, rsq,
   model, a, b, JND, PSE, WALD_p, PPT,EHI, DEVICE, HAND, AGE, EHI_dat,
   EWB, LOG_dat, tmp1, tmp2, LB_DURATION, LM_DURATION, LM_VALID, 
   LB_RESPTIME, LM_RESPTIME, SESSION, SEX, PPT_dat, mydir,
   DBEwide, df, EPOCH_r, EWBwide, EWS_HALF, EWS_SESSION, HALF_dat,
   LB_dat, LM_dat, tmp, TT_HALF, TT_SESSION, XCLUDE_DPI, COMPLETE, DBE_r, DBE1_mean, DBE1_sd,
   DBE2_mean, DBE2_sd, epochs, EWB_r, EWB1_mean, EWB1_sd, EWS,
   EWB2_mean, EWB2_sd, HALF, nTRT, Rlabel, XCLUDE_HAND,
   crit_d, crit_t, n1, n2, EWS_ANOVA, aovEWS, EWS_plot,
   TT_ANOVA, aovTT, TT_plot, EPOCH_d)
