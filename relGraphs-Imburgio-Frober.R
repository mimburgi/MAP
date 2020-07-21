rels_halfs_Frober$exp<-"Frober"
rels_halfs_Imburgio$exp<-"Imburgio"
rels_halfs<-rbind(rels_halfs_Frober, rels_halfs_Imburgio)
library(ggplot2)
ggplot(rels_halfs,aes(y = alpha, x = trialNum, colour = exp, shape = exp)) +
  stat_smooth(method = "loess", fill = NA) + 
  ggtitle("First-Second Half Splits") + 
  labs(y="Spearman-Brown correlation", x="Number of Trials Included")


rels_randsplit_Frober$exp<-"Frober"
rels_randsplit_Imburgio$exp<-"Imburgio"
rels_randsplit<-rbind(rels_randsplit_Frober, rels_randsplit_Imburgio)
library(ggplot2)
ggplot(rels_randsplit,aes(y = alpha, x = trialNum, colour = exp, shape = exp)) +
  stat_smooth(method = "loess", fill = NA) + 
  ggtitle("Random Splits") + 
  labs(y="Spearman-Brown correlation", x="Number of Trials Included")
