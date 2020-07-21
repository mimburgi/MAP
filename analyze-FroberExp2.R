dat<-read.table("./Frober/RawDR_Exp2_noprac_reformat.txt", header = T)

#renumber Trialn
dat$trialCumul<-NA
dat$Subject<-as.factor(dat$Subject)
dat<-subset(dat, Mode=="Test")
for (sub in levels(dat$Subject)){
  dat$trialCumul[dat$Subject==sub]<-
    c(1:length(dat$trialCumul[dat$Subject==sub]))
}

useexp<-subset(dat, Transition!="NaN")
useexp$sr<-ifelse(useexp$Transition=="s", 1, 0)
useexp<-subset(useexp, Subject != "16") #100 percent sr

#trim outliers
upperlim=mean(useexp$Target.RT) + 3*sd(useexp$Target.RT)
useexp<-subset(useexp, Target.RT < upperlim)
useexp<-subset(useexp, Target.RT > 200)

expcorr<-subset(useexp, Target.ACC==1)




###

tLengths=seq(100, 480, 20)
exp_rels<-data.frame(trialNum=numeric(), alpha=numeric())

for (tLen in tLengths){
  subset<-subset(expcorr, trialCumul < tLen)
  rel<-splithalf(subset, outcome="RT", score="difference", var.RT = "Target.RT", 
                 var.compare = c("Transition"), compare1="r", compare2="s",
                 var.participant = "Subject", var.trialnum = "trialCumul")
  
  exp_rels[nrow(exp_rels)+1,]<-c(tLen,rel$final_estimates$spearmanbrown)
}

plot(exp_rels$trialNum, exp_rels$alpha)
