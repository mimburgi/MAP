voldf<-read.table("Imburgio-data.txt", header = T)

voldf<-subset(voldf, task_acc==1)


voldf<-subset(voldf, altRep != "-1")



# voldf<-voldf[,c("subj", "trialCumul", "sr")]
# colnames(voldf)<-c("subj", "trialCumul", "sr")
# voldf$exp<-"Imburgio"
# write.table(voldf, "../outtable_Imburgio.txt", row.names = F)
# 
# voldfTrim<-subset(voldf, trialCumul < 65)
# voldfTrim$exp<-"Imburgio first 64"
# write.table(voldfTrim, "../outtable_Imburgio_first64.txt", row.names = F)

summary(glm(sr ~ trialCumul, data=voldf, family = "binomial"))

summary(glm(sr ~ trialCumul, data=voldfTrim, family = "binomial"))


##
library(splithalf)

tLengths=c(seq(50, 390, 20))
rels_randsplit_Imburgio<-data.frame(trialNum=numeric(), alpha=numeric())
for (tLen in tLengths){
  subset<-subset(subset(voldf), trialCumul < tLen)
  set.seed(1)
  rel<-splithalf(subset, outcome="accuracy", var.ACC = "sr",
                 score="average",
                      var.participant = "subj", var.trialnum = "trialCumul")

  rels_randsplit_Imburgio[nrow(rels_randsplit_Imburgio)+1,]<-c(tLen,rel$final_estimates$spearmanbrown)
}

plot(rels_randsplit_Imburgio$trialNum, rels_randsplit_Imburgio$alpha)
title("random splits")
#
rels_halfs_Imburgio<-data.frame(trialNum=numeric(), alpha=numeric())
for (tLen in tLengths){
  subset<-subset(subset(voldf), trialCumul < tLen)
  set.seed(1)
  rel<-splithalf(subset, outcome="accuracy", var.ACC = "sr",
                 score="average", halftype = "halfs",
                 var.participant = "subj", var.trialnum = "trialCumul")
  
  rels_halfs_Imburgio[nrow(rels_halfs_Imburgio)+1,]<-c(tLen,rel$spearmanbrown)
}

plot(rels_halfs_Imburgio$trialNum, rels_halfs_Imburgio$alpha)
title("first half vs second half, Imburgio")
#
