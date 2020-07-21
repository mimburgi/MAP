library(dplyr)
library(brms)

dat<-read.table("./Frober/RawDR_Exp3_noprac_reformat.txt", header = T)
## preproc ####
dat$Subject<-as.factor(dat$Subject)

#add sr column for easier switch rate calculations
dat$sr<-ifelse(dat$Transition==2, 1, 0)
dat$sr[dat$Transition=="NaN"]<- NA

#mark E1 trials
dat<-dat %>% mutate(prevAcc = lag(Target.ACC)) %>% mutate(E1=ifelse(prevAcc==0, 1, 0))

#add log RT column
dat$logRT<-log(dat$Target.RT)

#add switchFX column (effects-coded sr)
#used to assess changes in RT where reference is average of switch and repeat
dat$switchFX<-ifelse(dat$sr==1, 1, -1)

#separate out rewarded part (test) from baseline
test<-subset(dat, Mode=="Test")
bl<-subset(dat, Mode=='Baseline')

#remove outlier RTs
for (subj in (levels(bl$Subject))){
  upperRTlim<-mean(bl$Target.RT[bl$Subject==subj]) + 3*sd(bl$Target.RT[bl$Subject==subj])
  bl$RToutlier[bl$Subject==subj & bl$Target.RT > upperRTlim]<-1
  bl$RToutlier[bl$Subject==subj & bl$Target.RT < upperRTlim]<-0
}

bl<-subset(bl, RToutlier == 0)

for (subj in (levels(test$Subject))){
  upperRTlim<-mean(test$Target.RT[bl$Subject==subj]) + 3*sd(test$Target.RT[test$Subject==subj])
  test$RToutlier[test$Subject==subj & test$Target.RT > upperRTlim]<-1
  test$RToutlier[test$Subject==subj & test$Target.RT > upperRTlim]<-0
}
test<-subset(test, RToutlier == 0)


#remove incorrect trials
bl<-subset(bl, Target.ACC==1)
test<-subset(test, Target.ACC==1)

#remove trials following errors
bl<-subset(bl, E1==0)
test<-subset(test, E1==0)

#remove trials that were not switches or repeats
bl<-subset(bl, !is.na(sr))
test<-subset(test, !is.na(sr))

#remove explicit trials
bl<-subset(bl, TrialType==2)
test<-subset(test, TrialType==2)

#merge both back together to test interaction
dat<-rbind(bl, test)

## reg models for sr decrease over time ####

## reg models for sr decrease over time by reward presence ####
# srmod2<-brm(sr ~ TrialNr*Mode + (1 +TrialNr |Subject), data = dat, family = "bernoulli",
#            iter=3000, control = list(adapt_delta=.9))
# # including subject level interaction term and Mode effects yielded consistent ESS size issues
# # subject level TrialNR seems fine, raised adapt delta to get rid of a few divergent transitions
# # no interaction
# 
# saveRDS(srmod2, 'FroberE3_srChg_interaction_paperExclusionCriteria.rds')
srmod<-readRDS('FroberE3_srChg_interaction_paperExclusionCriteria.rds')
summary(srmod)
#no 3-way interaction

# srmod<-brm(sr ~ TrialNr + Mode + (1 + TrialNr|Subject), data = dat, family = "bernoulli")
# saveRDS(srmod, 'FroberE3_srChg_nointeraction_paperExclusionCriteria.rds')
srmod<-readRDS('FroberE3_srChg_nointeraction_paperExclusionCriteria.rds')
summary(srmod)
#no decline here, possibly due to restricted opportunities to switch in both training and test phases


##todo: compare magnitude of sr decline in each condition ####
#need a BF equivalent for a single coefficeint to test interactin term null

## reg models for rt and switch cost decline ####
scmod<-brm(logRT ~ TrialNr*switchFX*Mode + (1 + TrialNr*switchFX|Subject), data = dat)
saveRDS(scmod, 'FroberE3_scChg_3way_PaperExclusionCriteria.rds')
scmod<-readRDS('FroberE3_scChg_3way_PaperExclusionCriteria.rds')
summary(scmod)
#no 3 way interaction

scmod2<-brm(logRT ~ TrialNr*switchFX + switchFX*Mode + TrialNr*Mode + (1 + TrialNr*switchFX |Subject), data = dat)
saveRDS(scmod2, 'FroberE3_scChg_2ways_PaperExclusionCriteria.rds')
summary(scmod2)
#no 2 way interactions

scmod3<-brm(logRT ~ TrialNr*switchFX + Mode + (1 + TrialNr*switchFX|Subject), data = dat)
saveRDS(scmod3, 'FroberE3_scChg_noRewInteraction_PaperExclusionCriteria.rds')
scmod3<-readRDS('FroberE3_scChg_noRewInteraction_PaperExclusionCriteria.rds')
summary(scmod3)
#no decline in SC

rtmod<-brm(logRT ~ TrialNr + switchFX + Mode + (1 + TrialNr + switchFX|Subject), data = dat)
saveRDS(rtmod, 'FroberE3_rtChg_PaperExclusionCriteria.rds')
rtmod<-readRDS('FroberE3_rtChg_PaperExclusionCriteria.rds')
summary(rtmod)

#significant decline in RT, no switch cost effect interestingly


## relationship between rt decline and sr decline individual coefficients ####
srdecs<-as.data.frame(ranef(srmod))$Subject.Estimate.TrialNr
rtdecs<-as.data.frame(ranef(rtmod))$Subject.Estimate.TrialNr

#normality checks
shapiro.test(srdecs) #normal
shapiro.test(rtdecs) #normal
cor.test(srdecs, rtdecs) #no relationship

