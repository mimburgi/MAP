dat<-read.table("./Frober/RawDR_Exp3_noprac_reformat.txt", header = T)
#renumber Trialn
dat$Subject<-as.factor(dat$Subject)
dat<-subset(dat, Mode=="Test")
dat$trialCumul<-NA
for (sub in levels(dat$Subject)){
  dat$trialCumul[dat$Subject==sub]<-
    c(1:length(dat$trialCumul[dat$Subject==sub]))
}
vol<-subset(dat, TrialType==2)



usevol<-subset(vol, TrialNr!=1)
usevol$sr<-usevol$Transition-1

usevol$Subject<-as.factor(usevol$Subject)

usevol$numChoices<-NA
for (sub in levels(usevol$Subject)){
  usevol$numChoices[usevol$Subject==sub]<-
    c(1:length(usevol$numChoices[usevol$Subject==sub]))
}

mod<-glm(sr ~ trialCumul, data = subset(usevol, Target.ACC==1), family="binomial")
summary(mod)

mod<-glm(sr ~ numChoices, data = subset(usevol, Target.ACC==1), family="binomial")
summary(mod)


outtable<-subset(usevol, Target.ACC==1)
outtable<-outtable[c("Subject", "trialCumul", "sr")]
colnames(outtable)<-c("subj", "trialCumul", "sr")
outtable$exp<-"Frober E3 80/20"
write.table(outtable, "outtable_FroberE3.txt", row.names = F)
