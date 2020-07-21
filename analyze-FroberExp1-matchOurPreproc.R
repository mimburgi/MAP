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

#mark and remove E1 trials
dat<-dat %>% mutate(prevAcc = lag(Target.ACC)) %>% mutate(E1=ifelse(prevAcc==0, 1, 0))
dat<-subset(dat, E1==0)

#add log RT column
dat$logRT<-log(dat$Target.RT)

#add switchFX column (effects-coded sr)
#used to assess changes in RT where reference is average of switch and repeat
dat$switchFX<-ifelse(dat$sr==1, 1, -1)

#subject average switch rates
sraggs_test<-aggregate(sr ~ Subject, FUN=mean, data = dat)

#remove subs with bad SRs
goodsubs<-sraggs_test$Subject[sraggs_test$sr > .2]
dat<-subset(dat, Subject %in% goodsubs)
dat$Subject<-droplevels(dat$Subject)


#remove incorrect trials
dat<-subset(dat, Target.ACC==1)

#remove trials following errors
dat<-subset(dat, E1==0)

#remove outlier RTs
upperRTlim<-mean(dat$Target.RT) + 3*sd(dat$Target.RT)
dat<-subset(dat, Target.RT < upperRTlim)

#remove trials that were not switches or repeats
dat<-subset(dat, !is.na(sr))

#separate test from baseline
bl<-subset(dat, Mode=="Baseline")
test<-subset(dat, Mode=="Test")

## reg models for sr decrease over time by reward presence ####
# srmod<-brm(sr ~ Trialnr*Mode + (1 + Trialnr*Mode|Subject), data = dat, family = "bernoulli")
# saveRDS(srmod, 'FroberE1_srChg_interaction_ourExclusionCriteria.rds')
srmod<-readRDS('FroberE1_srChg_interaction_ourExclusionCriteria.rds')
summary(srmod)
#no interaction

# srmod2<-brm(sr ~ Trialnr + Mode + (1 + Trialnr + Mode|Subject), data = dat, family = "bernoulli")
# saveRDS(srmod2, 'FroberE1_srChg_nointeraction_ourExclusionCriteria.rds')
srmod2<-readRDS('FroberE1_srChg_nointeraction_ourExclusionCriteria.rds')
summary(srmod2)
#no interaction

#significant decline in sr over time is present, does not differ with the introduction of reward (no interaction)

#todo: run rt analyses

#extra analyses for good measure using same exclusion criteria as the paper
srmod_bl2<-brm(sr ~ Trialnr + (1 + Trialnr|Subject), data = bl2, family = "bernoulli", control = list(adapt_delta=.85))
#first run yielded a couple of divergent transitions, increased adapt delta to be sure
saveRDS(srmod_bl2, 'FroberE1_srChg_baseline_paperExclusionCriteria.rds')
#srmod_bl2<-readRDS('FroberE1_srChg_baseline_paperExclusionCriteria.rds')
summary(srmod_bl2)

srmod_test2<-brm(sr ~ Trialnr + (1 + Trialnr|Subject), data = test2, family = "bernoulli")
saveRDS(srmod_test2, 'FroberE1_srChg_test_paperExclusionCriteria.rds')
#srmod_test<-readRDS('FroberE1_srChg_test_paperExclusionCriteria.rds')
summary(srmod_test2)

#declines are still present

## compare magnitude of sr decline in each condition ####
bl_subjFX<-as.data.frame(ranef(srmod_bl)$Subject)$Estimate.Trialnr
test_subjFX<-as.data.frame(ranef(srmod_test)$Subject)$Estimate.Trialnr
t.test(bl_subjFX, test_subjFX, paired = T) #almost no difference at all
#test support for null with BF
ttestBF(bl_subjFX, test_subjFX, paired = T)
#strong support for the null
#presence of reward does not seem to change rate of SR decline


#extra tests with paper exclusion criteria for good measure
bl_subjFX2<-as.data.frame(ranef(srmod_bl2)$Subject)$Estimate.Trialnr
test_subjFX2<-as.data.frame(ranef(srmod_test2)$Subject)$Estimate.Trialnr
t.test(bl_subjFX2, test_subjFX2, paired = T) #almost no difference at all
#test support for null with BF
ttestBF(bl_subjFX2, test_subjFX2, paired = T)
#strong support for the null


## reg models for rt and switch cost decline ####
scmod_bl<-brm(logRT ~ Trialnr*switchFX + (1 + Trialnr*switchFX|Subject), data = bl)
saveRDS(scmod_bl, 'FroberE1_scChg_baseline.rds')
#srmod_bl<-readRDS('FroberE1_srChg_baseline.rds')
summary(scmod_bl)

scmod_test<-brm(logRT ~ Trialnr*switchFX + (1 + Trialnr*switchFX|Subject), data = test)
saveRDS(scmod_test, 'FroberE1_scChg_test.rds')
#scmod_test<-readRDS('FroberE1_scChg_test.rds')
summary(scmod_test)

rtmod_bl<-brm(logRT ~ Trialnr + switchFX + (1 + Trialnr + switchFX|Subject), data = bl)
saveRDS(rtmod_bl, 'FroberE1_rtChg_baseline.rds')
#rtmod_bl<-readRDS('FroberE1_rtChg_baseline.rds')
summary(rtmod_bl)

rtmod_test<-brm(logRT ~ Trialnr + switchFX + (1 + Trialnr + switchFX|Subject), data = test)

saveRDS(rtmod_test, 'FroberE1_rtChg_test.rds')
#rtmod_test<-readRDS('FroberE1_rtChg_test.rds')
summary(rtmod_test)