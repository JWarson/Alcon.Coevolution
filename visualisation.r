#summarized results
#####################
library(data.table)
results <- "/home/jonas/Documents/TheoMod/Assignment/16jan/results/"
setwd(results)
run.date <- Sys.Date()
hostdata <- read.csv(gsub(" ", "",paste("standardrun_Host_",run.date,"_summarized_.csv")))
head(hostdata)
parasitedata <- read.csv(gsub(" ", "",paste("standardrun_Parasite_",run.date,"_summarized_.csv")))
head(parasitedata)

#remove 'NA' genotype
gt <- subset(hostdata, Immune.Genotype=='1'| Immune.Genotype=='2'| Immune.Genotype=='3'|Immune.Genotype=='4'|Immune.Genotype=='5' )

as.factor(gt$Immune.Genotype)
plot(Host.Number.Individuals ~ Host.Time, data = gt, col = Immune.Genotype, pch = Host.Population )
plot(Host.Number.Individuals ~ Host.Time, data = gt, pch = Immune.Genotype, col = Host.Population )

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
       title = "Resource 500") +
  theme_bw()

#equilibrium Host/parasite Ratio
eqp <- mean(data$av.parasite.popsize[round(duration.days-(duration.days/10)):duration.days])
eqh <- mean(data$av.host.popsize[round(duration.days-(duration.days/10)):duration.days])
hoverp <- eqh/eqp
c(eqh, eqp, hoverp)

#equilibrium in function of resource (calculated from above 'hoverp')
qplot(x = c(250,500,1000,1500,2000), y = c(2.04520547945205,
                                           2.04794520547945,
                                           2.05151098901099,
                                           2.05791176231976,
                                           2.07074340527578), 
      geom = "line", xlab = 'Resources', ylab = "Host/Parasite equilibrium") +
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




