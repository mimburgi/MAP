files=list.files(pattern="outtable*")
for (file in files){
  if (file==files[1]){
    dat<-read.table(file, header = T)
  }else{
    tmp<-read.table(file, header = T)
    dat<-rbind(dat, tmp)
  }
}

dat$subj<-as.factor(dat$subj)

library(ggplot2)
ggplot(dat,aes(y = sr, x = trialCumul, colour = exp, shape = exp)) +
  geom_smooth(method = "lm", fill = NA)



for(exp in levels(dat$exp)){
  for (sub in levels(dat$subj)){
    dat$numChoices[dat$subj==sub & dat$exp == exp]<-
      c(1:length(dat$numChoices[dat$subj==sub & dat$exp == exp]))
  }
}

ggplot(dat,aes(y = sr, x = numChoices, colour = exp, shape = exp)) +
  geom_smooth(method = "lm", fill = NA)
