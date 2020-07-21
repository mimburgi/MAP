dat<-read.csv("./Braem/SPSSdat_expanded.csv", encoding = "UTF-8")
library(dplyr)
library(brms)

## preproc ####
colnames(dat)[1]<-"subj"

#reward condition
dat$condition<-ifelse(dat$subj%%2==1, "s", "r")

#add cumulative trial number
dat$trialCumul<-NA
dat$subj<- as.factor(dat$subj)
for (sub in levels(dat$subj)){
  dat$trialCumul[dat$subj==sub]<-
    c(1:length(dat$trialCumul[dat$subj==sub]))
}

#mark E1 trials
dat$E1<-0
dat$E1[lag(dat$corr==0)]<-1

#take only correct, voluntary trials not following errors
use<-subset(dat, !(is.na(taskchswi) & E1==1))

#exclude subjs with bad perf (using criteria Braem did)
badswitchsubs<-c(2, 20, 28, 34, 44, 1, 3, 9, 19, 33, 39)
use2<-subset(use, !(subj %in% badswitchsubs))
use2$subj<-as.factor(use2$subj)


use2$subj<-as.factor(use2$subj)

## SR Decline models ####

# condsrmod<-brm(taskchswi ~ trialCumul*condition + (1 + trialCumul*condition|subj), data=use2, family="bernoulli")
condsrmod<-readRDS('Braem-rewcondsrChg-PaperExclusionCriteria.rds')


# srmod<-brm(as.factor(taskchswi) ~ trialCumul + (1 + trialCumul|subj), data=use2, family="bernoulli")
#no decline on a group level
srmod<-readRDS('Braem-rewcondsrChg-PaperExclusionCriteria.rds')


#extract subject level declines to look for correlations between that and BIS/BAS
subjFX<-as.data.frame(ranef(srmod)$subj)$Estimate.trialCumul %>% as.data.frame()
subjFX$subj<-rownames(subjFX)
colnames(subjFX)[1]<-'SRdec'
## subject level correlations between BIS/BAS and SR decline ####

#read in BISBAS scores from Braem's analyses
aggs<-read.csv('./Braem/SPSSdat_aggs.csv')
colnames(aggs)[1]<-"subj"
aggsuse<-subset(aggs, filter_.==1) #retain only usable participants

#add BISBAS scores to random effects estimates
subjFX$BIS<-aggsuse$BIS
subjFX$BASr<-aggsuse$BASr
subjFX$BASd<-aggsuse$BASd
subjFX$BASf<-aggsuse$BASf


#test normality of measures to see what correlation method to use
shapiro.test(subjFX$SRdec) #normal
shapiro.test(subjFX$BIS) #not normal, use spearman
shapiro.test(subjFX$BASr) #not normal, use spearman
shapiro.test(subjFX$BASd) #not normal, use spearman
shapiro.test(subjFX$BASf) #normal-ish (p = .058), safest to check both spearman and pearson (if normal they should agree)
#might help to look at density and QQplot for BASf
plot(density(subjFX$BASf)) #looks bimodal
qqnorm(subjFX$BASf)
qqline(subjFX$BASf)



cor.test(subjFX$SRdec, subjFX$BIS, method="spearman") #p < .01, rho = -.49
#more inhibition means greater decline in SR
#makes sense if switching is aversive, participants avoid aversive choice more over time
plot(subjFX$SRdec, subjFX$BIS)


cor.test(subjFX$SRdec, subjFX$BASr, method="spearman") #p = .63
cor.test(subjFX$SRdec, subjFX$BASd, method="spearman") #p = .49
#neither is significant

cor.test(subjFX$SRdec, subjFX$BASf, method="spearman") #p = .02, rho = .37
cor.test(subjFX$SRdec, subjFX$BASf, method="pearson") #p = .09, r = .27
cor.test(subjFX$SRdec, subjFX$BASf, method="kendall") #p = .025, tau = .26
#possible that greater fun seeking means increase in SR over time

plot(subjFX$BASf, subjFX$SRdec)
#the two outliers are probably affecting p for pearson a lot
#spearman is less affected by outliers, as is kendalls

## look for relationships between overall SR and BISBAS ####
srs<-aggregate(taskchswi ~ subj, data = use2, FUN=mean)
subjFX$srMean<-srs$taskchswi
shapiro.test(subjFX$srMean) #normal enough

cor.test(subjFX$srMean, subjFX$BIS, method="spearman") #NS
cor.test(subjFX$srMean, subjFX$BASr, method="spearman") #NS
cor.test(subjFX$srMean, subjFX$BASd, method="spearman") #NS
cor.test(subjFX$srMean, subjFX$BASf, method="spearman") #NS
cor.test(subjFX$srMean, subjFX$BASf, method="pearson") #NS

#write out 
write.table(subjFX, "Braem_indFX_BISBAS.txt", row.names = F)


#write out 
outtable<-use2[c("subj", "trialCumul", "taskchswi", "BIS", "BAS")]
colnames(outtable)<-c("subj", "trialCumul", "sr", "BIS", "BAS")
outtable$exp<-"Braem 50/50"
write.table(outtable, "outtable_Braem_BISBAS.txt", row.names = F)

