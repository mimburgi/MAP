voldf<-read.table("Imburgio-data.txt", header = T)
library(brms)

# ## preproc, remove unnecessary trials ####
# 
# voldf<-subset(voldf, task_acc==1)
# 
# 
# voldf<-subset(voldf, altRep != "-1")
# voldf<-subset(voldf, E1==0)
# voldf<-subset(voldf, taskRT > 200)
# 
# voldf$switchFXcode<-ifelse(voldf$sr==1, 1, -1)
# voldf$subj<-as.factor(voldf$subj)
# 
# #remove upper RT outliers
# upperRTbound<-mean(voldf$taskRT) + 3*sd(voldf$taskRT)
# voldf<-subset(voldf, taskRT < upperRTbound)
# 
# ## sr change analyses ####
# mod_srChg<-brm(sr ~ trialCumul + (1+trialCumul|subj), 
#                data=voldf, family = "bernoulli", control = list(max_treedepth=15), iter = 3000) 
# #increased max treedepth here because first run yielded 10 iterations with treedepth above 10
# #warning about Bulk ESS too low, tried again with 3000 iter (instead of 2000) which worked
# saveRDS(mod_srChg, "Imburgio_srchgmod.rds")
mod_srChg<-readRDS('Imburgio_srchgmod.rds')


# ##rt change analyses ####
# voldf$logRT<-log(voldf$taskRT)
# mod_scChg<-brm(logRT ~ trialCumul*altRep + (1 + trialCumul + altRep|subj),
#                data = voldf, control=list(max_treedepth=15), iter = 4000)
# #increased max treedepth because every iteration exceeded the default of 10 on first run
# #increased iterations because with default of 2000 iterations, Bulk ESS was too low
# #this model has good diagnostic indicators, no effect of switch cost at a group level
# saveRDS(mod_scChg, 'Imburgio_scchgmod.rds')
mod_scChg<-readRDS('Imburgio_scchgmod.rds')

# 
# mod_rtChg<-brm(logRT ~ trialCumul + switchFXcode + (1 + trialCumul + switchFXcode|subj),
#                data = voldf, control=list(max_treedepth=15), iter = 4000)
# #increased max treedepth because all iterations hit treedepth limit in the first run
# #increased iter because Bulk ESS was too low at default iteration number
# saveRDS(mod_rtChg, 'Imburgio_rtchgmod.rds')
mod_rtChg<-readRDS('Imburgio_rtchgmod.rds')


## relationships between RT and SR declines ####
srdecs<-as.data.frame(ranef(mod_srChg))$subj.Estimate.trialCumul
rtdecs<-as.data.frame(ranef(mod_rtChg))$subj.Estimate.trialCumul
cor.test(srdecs, rtdecs, method = 'pearson') #weak but sign relationship

#check normality
shapiro.test(srdecs) #not normal - very skewed, pulled by large decreases in some
shapiro.test(rtdecs) #normal

cor.test(srdecs, rtdecs, method = 'spearman')
cor.test(srdecs, rtdecs, method = 'kendall')
#still significant with nonparametric tests

#remove outliers in srdecs
sroutliers<-which(srdecs < -.008) #three people, probably true outliers
srdecs_trimmed<-srdecs[-sroutliers]
plot(density(srdecs_trimmed)) #much closer to normal

rtdecs_trimmed<-rtdecs[-sroutliers]

cor.test(srdecs_trimmed, rtdecs_trimmed, method = 'pearson') #still sign
cor.test(srdecs_trimmed, rtdecs_trimmed, method = 'spearman') #still sign
cor.test(srdecs_trimmed, rtdecs_trimmed, method = 'kendall') #still sign


plot(srdecs_trimmed, rtdecs_trimmed) #does look like you can see the relationship