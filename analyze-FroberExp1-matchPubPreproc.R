#todo: separate extra analyses using super low sr exclusion criteria from paper into another script

dat<-read.table("./Frober/RawDR_Exp1_noprac_reformat.txt", header = T)
library(brms)
library(dplyr)
library(BayesFactor)

## preproc ####
dat$Subject<-as.factor(dat$Subject)

#add sr column for easier switch rate calculations
dat$sr<-ifelse(dat$Transition=="s", 1, 0)
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

#subject average switch rates
sraggs_test<-aggregate(sr ~ Subject, FUN=mean, data = test)

#same exclusion criteria the paper used
goodsubs<-sraggs_test$Subject[sraggs_test$sr > .05]
bl<-subset(bl, Subject %in% goodsubs)
test<-subset(test, Subject %in% goodsubs)
bl$Subject<-droplevels(bl$Subject)
test$Subject<-droplevels(test$Subject)

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

#merge both back together to test interaction
dat<-rbind(bl, test)

## reg models for sr decrease over time ####

## reg models for sr decrease over time by reward presence ####
# srmod<-brm(sr ~ Trialnr*Mode + (1 + Trialnr*Mode|Subject), data = dat, family = "bernoulli")
# saveRDS(srmod, 'FroberE1_srChg_interaction_paperExclusionCriteria.rds')
srmod<-readRDS('FroberE1_srChg_interaction_paperExclusionCriteria.rds')
summary(srmod)
#no 3-way interaction

# srmod2<-brm(sr ~ Trialnr + Mode + (1 + Trialnr + Mode|Subject), data = dat, family = "bernoulli")
# saveRDS(srmod2, 'FroberE1_srChg_nointeraction_paperExclusionCriteria.rds')
srmod2<-readRDS('FroberE1_srChg_nointeraction_paperExclusionCriteria.rds')
summary(srmod2)

##todo: compare magnitude of sr decline in each condition ####
#need a BF equivalent for a single coefficeint to test interactin term null

## reg models for rt and switch cost decline ####
# scmod<-brm(logRT ~ Trialnr*switchFX*Mode + (1 + Trialnr*switchFX*Mode|Subject), data = dat)
# saveRDS(scmod, 'FroberE1_scChg_3way_PaperExclusionCriteria.rds')
scmod<-readRDS('FroberE1_scChg_3way_PaperExclusionCriteria.rds')
summary(scmod)

scmod2<-brm(logRT ~ Trialnr*switchFX + switchFX*Mode + Trialnr*Mode + (1 + Trialnr*switchFX + Mode|Subject), data = dat)
saveRDS(scmod2, 'FroberE1_scChg_2ways_PaperExclusionCriteria.rds')
summary(scmod2)

# scmod3<-brm(logRT ~ Trialnr*switchFX + Mode + (1 + Trialnr*switchFX + Mode|Subject), data = dat)
# saveRDS(scmod3, 'FroberE1_scChg_noRewInteraction_PaperExclusionCriteria.rds')
scmod3<-readRDS('FroberE1_scChg_noRewInteraction_PaperExclusionCriteria.rds')
summary(scmod3)
#no decline in switch cost, sign main effect of Mode (interestingly, longer RTs in Test condition)

# rtmod<-brm(logRT ~ Trialnr + switchFX + Mode + (1 + Trialnr + switchFX + Mode|Subject), data = dat)
# saveRDS(rtmod, 'FroberE1_rtChg_PaperExclusionCriteria.rds')
rtmod<-readRDS('FroberE1_rtChg_PaperExclusionCriteria.rds')
summary(rtmod)

## relationship between rt decline and sr decline individual coefficients ####
srdecs<-as.data.frame(ranef(srmod2))$Subject.Estimate.Trialnr
rtdecs<-as.data.frame(ranef(rtmod))$Subject.Estimate.Trialnr

#normality checks
shapiro.test(srdecs) #normal
shapiro.test(rtdecs) #normal
cor.test(srdecs, rtdecs) #no relationship

