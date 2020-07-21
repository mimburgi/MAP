####read in raw data files####
setwd("./rawdata/")
files = list.files(pattern = "*.txt")
for(file in files){
  thisdf<-read.table(file, header = T)
  if (file == files[1]){
    fulldf<-thisdf
  }
  else{
    fulldf<-rbind(thisdf, fulldf)
  }
}
setwd("..")



####preproc/data cleaning####
voldf<-subset(fulldf, task_cue == "?")

#set switches correctly for voldf
library(data.table)
voldf$sr[voldf$task != shift(voldf$task)]<-1
voldf$sr[voldf$task == shift(voldf$task)]<-0
voldf$sr[voldf$altRep == -1]<- -1

#remove subjects with bad switch rats from voldf
srtots<-aggregate(sr ~ subj, data = subset(voldf, altRep > -1), FUN = mean)
badsubs<-srtots$subj[srtots$sr > .9 | srtots$sr < .1]

voldf<-subset(voldf, !(subj %in% badsubs))

#remove bad accuracies
acctots_vol<-aggregate(task_acc ~ subj, data = voldf, FUN = mean)
badsubs<-acctots_vol$subj[acctots_vol$acc < .6]
voldf<-subset(voldf, !(subj %in% badsubs))

voldf$altRep<-voldf$sr
voldf$subj<-as.factor(voldf$subj)
voldf$altRep<-as.factor(voldf$altRep)
###

voldf$trialCumul<-NA
for (sub in levels(voldf$subj)){
  voldf$trialCumul[voldf$subj==sub]<-
    c(1:length(voldf$trialCumul[voldf$subj==sub]))
}

##

write.table(voldf, "../Imburgio-data.txt", row.names = F)