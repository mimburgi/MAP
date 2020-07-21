dat<-read.table("./Frober/RawDR_Exp1_noprac_reformat.txt", header = T)
#renumber Trialn
dat$Subject<-as.factor(dat$Subject)
dat<-subset(dat, Mode=="Test")
dat$trialCumul<-NA
for (sub in levels(dat$Subject)){
  dat$trialCumul[dat$Subject==sub]<-
    c(1:length(dat$trialCumul[dat$Subject==sub]))
}

usevol<-subset(dat, Transition!="NaN")
usevol$sr<-ifelse(usevol$Transition=="s", 1, 0)

usevol$Subject<-as.factor(usevol$Subject)

# ###
# library(splithalf)
# upperlim=mean(usevol$Target.RT) + 3*sd(usevol$Target.RT)
# #usevol<-subset(usevol, Target.RT < upperlim)
# usevol<-subset(usevol, Target.RT > 200)
# 
volcorr<-subset(usevol, Target.ACC==1)
tLengths=c(seq(50, 350, 20))

rels_halfs_Frober<-data.frame(trialNum=numeric(), alpha=numeric())
for (tLen in tLengths){
  subset<-subset(subset(volcorr), trialCumul < tLen)
  set.seed(1)
  rel<-splithalf(subset, outcome="accuracy", var.ACC = "sr",
                 score="average", halftype = "halfs", var.participant = "Subject", var.trialnum = "trialCumul")
  
  rels_halfs_Frober[nrow(rels_halfs_Frober)+1,]<-c(tLen,rel$spearmanbrown)
}

plot(rels_halfs_Frober$trialNum, rels_halfs_Frober$alpha)
title("first half vs second half, Frober")

rels_randsplit_Frober<-data.frame(trialNum=numeric(), alpha=numeric())
for (tLen in tLengths){
  subset<-subset(subset(volcorr), trialCumul < tLen)
  set.seed(1)
  rel<-splithalf(subset, outcome="accuracy", var.ACC = "sr",
                 score="average", var.participant = "Subject", var.trialnum = "trialCumul")
  
  rels_randsplit_Frober[nrow(rels_randsplit_Frober)+1,]<-c(tLen,rel$final_estimates$spearmanbrown)
}

plot(rels_randsplit_Frober$trialNum, rels_randsplit_Frober$alpha)
title("random splits, Frober")


# tLengths=c(seq(100, 350, 10))
# vol_rels<-data.frame(trialNum=numeric(), alpha=numeric())
# for (tLen in tLengths){
#   subset<-subset(subset(volcorr), trialCumul < tLen)
#   set.seed(1)
#   rel<-splithalf(subset, outcome="RT", score="difference", var.RT = "Target.RT", 
#                       var.compare = c("Transition"), compare1="r", compare2="s",
#                       var.participant = "Subject", var.trialnum = "trialCumul")
#   
#   vol_rels[nrow(vol_rels)+1,]<-c(tLen,rel$final_estimates$spearmanbrown)
# }
# 
# plot(vol_rels$trialNum, vol_rels$alpha)
# 
##
#sr decreases over time
mod3<-glm(as.factor(sr) ~ trialCumul, data = subset(volcorr, trialCumul < 65), family = "binomial")
summary(mod3)

outtable<-volcorr[c("Subject", "trialCumul", "sr")]
colnames(outtable)<-c("subj", "trialCumul", "sr")
outtable$exp<-"Frober E1 Vol"
write.table(outtable, "outtable_FroberE1.txt", row.names = F)

outtable<-subset(volcorr, trialCumul < 65)
outtable<-outtable[c("Subject", "trialCumul", "sr")]
colnames(outtable)<-c("subj", "trialCumul", "sr")
outtable$exp<-"Frober E1 Vol First 64 trials"
write.table(outtable, "outtable_FroberE1_first64.txt", row.names = F)

#reliability of sr
###
vcorr_srtrim<-subset(volcorr, Subject != "25")
vcorr_srtrim<-subset(vcorr_srtrim, Subject != "18")

tLengths=c(seq(30, 350, 10))
sr_rels<-data.frame(trialNum=numeric(), alpha=numeric())
for (tLen in tLengths){
  subset<-subset(subset(vcorr_srtrim), trialCumul < tLen)
  set.seed(1)
  rel<-splithalf(subset, outcome="accuracy", score="average", var.ACC = "sr", 
                 var.participant = "Subject", var.trialnum = "trialCumul",
                 halftype="halfs")
  
  sr_rels[nrow(sr_rels)+1,]<-c(tLen,rel$spearmanbrown)
}

plot(sr_rels$trialNum, sr_rels$alpha)

##

