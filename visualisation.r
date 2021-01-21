#summarized results
#####################
library(data.table)
results <- "/home/jonas/Documents/TheoMod/Assignment/Alcon.Coevolution/results/"
setwd(results)
run.date <- Sys.Date()
hostdata <- read.csv(gsub(" ", "",paste("standardrun_Host_",run.date,"_summarized_.csv")))
head(hostdata)
parasitedata <- read.csv(gsub(" ", "",paste("standardrun_Parasite_",run.date,"_summarized_.csv")))
head(parasitedata)

#remove 'NA' genotype
gtH <- subset(hostdata, Immune.Genotype=='1'| Immune.Genotype=='2'| Immune.Genotype=='3'|Immune.Genotype=='4'|Immune.Genotype=='5' )
gtP <- subset(parasitedata, Parasite.Infection.Genotype=='1'| Parasite.Infection.Genotype=='2'| Parasite.Infection.Genotype=='3'|Parasite.Infection.Genotype=='4'|Parasite.Infection.Genotype=='5' )

#host
ggplot(gtH, aes(Host.Time)) + 
  geom_point(aes(y = Host.Number.Individuals, colour = as.factor(Immune.Genotype), pch = as.factor(Host.Replicate))) +
  labs(x="Time (years)", y = "Host Individuals", colour = "Genotype", pch = "Replicate", 
       title = "Resource 500") +
  scale_shape_manual(values=c(3, 19))+
  theme_bw()

#parasite
gtP2 <- subset(parasitedata, Parasite.Replicate == 2 & (Parasite.Infection.Genotype=='1'| Parasite.Infection.Genotype=='2'| Parasite.Infection.Genotype=='3'|Parasite.Infection.Genotype=='4'|Parasite.Infection.Genotype=='5'))
ggplot(gtP2, aes(Parasite.Time)) + 
  geom_point(aes(y = Parasite.Number.Individuals, colour = as.factor(Parasite.Infection.Genotype), pch = as.factor(Parasite.Replicate))) +
  labs(x="Time (years)", y = "Parasite Individuals", pch = "Replicate", colour = "Genotype", 
       title = "Resource 500") +
  scale_shape_manual(values=c(3, 16))+
  theme_bw()

################################################################################
##Stats file###
##save and load stats
write.table(stats, file = result.file.name, sep = "\t")
data <- read.table(file = result.file.name, sep = "\t")

library(ggplot2)
library(ggthemes)

##plot popsize ifo time
ggplot(data, aes(time)) + 
  geom_point(aes(y = av.host.popsize, colour = "Gentian")) +
  geom_point(aes(y = av.parasite.popsize, colour = "Alcon")) +
  labs(x="Time (years)", y = "Average population size", colour = "Species", 
       title = "Resource 50") +
  theme_bw()

#equilibrium Host/parasite Ratio
eqp <- mean(data$av.parasite.popsize[round(duration.years-(duration.years/10)):duration.years])
eqh <- mean(data$av.host.popsize[round(duration.years-(duration.years/10)):duration.years])
hoverp <- eqh/eqp
c(eqh, eqp, hoverp)

#equilibrium in function of resource (calculated from above 'hoverp')
qplot(x = c(20,50,75,100,250,500,1000,2000), y = c(1.852719,
                                                1.935147,
                                                1.865336,
                                                1.870178,
                                                1.917626,
                                                1.945995,
                                                1.942982,
                                                1.944654), 
      geom = c("line","point"), xlab = 'Resources', ylab = "Host/Parasite equilibrium") +
  theme_bw()

#equilibrium in function of starting P/H ratio 
#note that here we use Parasite/Host (= 1/ (Host/Parasite))
qplot(x = log2(c(0.125,0.25,0.5,1,2,4,8,16)), y = c(0.074032889851646,
                                           0.139348856238523,
                                           0.252997834591534,
                                           0.488294314381271,
                                           0.959232613908873,
                                           1.78351482974746,
                                           2.14988434697155,
                                           3.94781774504598), 
      geom = c("line","point"), xlab = 'Log2 (Initial Parasite/Host ratio)', ylab = "Parasite/Host equilibrium", main = "Initial P/H ratio i.f.o. equilibrium ratio") +
  theme_bw()




