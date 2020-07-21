dat<-read.table("./Frober/RawDR_Exp1_noprac_reformat.txt", header = T)
library(brms) #for the bayesian logistic regressions
library(dplyr) #for random preproc stuff, you probably won't need it

## preproc ####
dat$Subject<-as.factor(dat$Subject)

#add sr column for easier switch rate calculations
dat$sr<-ifelse(dat$Transition=="s", 1, 0)
dat$sr[dat$Transition=="NaN"]<- NA

#mark E1 trials
dat<-dat %>% mutate(prevAcc = lag(Target.ACC)) %>% mutate(E1=ifelse(prevAcc==0, 1, 0))

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

## reg models for sr decrease over time by reward presence ####

srmod<-brm(sr ~ Trialnr + Mode + (1 + Trialnr + Mode|Subject), data = dat, family = "bernoulli")

#I like to save the model when it's done like this because they can take a while to run
#saveRDS(srmod, 'FroberE1_srChg_nointeraction_paperExclusionCriteria.rds')

#then instead of rerunning it just load it up when you need it again
#srmod<-readRDS('FroberE1_srChg_nointeraction_paperExclusionCriteria.rds')
summary(srmod) #this is the group level effect
#the coefficient is tiny because it represents a change in logit on a single trial level
#you can print more digits like this
print(summary(srmod), digits=10)
#in this case, there is a significant decline
#the CI for the coefficient doesn't include zero


#this is how you get the subject level coefficients
#AKA random effects
srdecs<-as.data.frame(ranef(srmod))$Subject.Estimate.Trialnr