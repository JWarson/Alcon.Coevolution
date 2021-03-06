#############################################################
### Helper script for the Digital_Coevolution simulation ####
#############################################################

## You shouldn't be here! 
## Warning: Only change stuff in here if you know what you are doing...

###########################################################
## This script simply coordinates the running of the 
## Digital_Coevolution simulation, and takes those
## steps out of the frontend script that most users
## will interact with. 
# Load data.table library
library(data.table)
# Source the parameterfile.
source(file =  paste(source.file.location, "Alcon.Coevolution_Parameterspace.R", sep = ""), local = TRUE)

# Save a copy of the created parameters
saveRDS(as.list(.GlobalEnv),file = 
          paste(result.file.location, "Alcon.Coevolution_Parameterspace","_", run.date, "_", result.file.name, ".RDS", sep = ""))

# Source the main body of the simulation that includes all dynamics functions
source(file =  paste(source.file.location, "Alcon.Coevolution_Functions.R", sep = ""), local = TRUE)

######################################################################################################################################
### Now we run the simulation
# As a first step we need to create the individuals that are the core of this individual based simulation.
# We do that by calling the individual.creator.function that was sourced in the Main_Body_Digital_Coevolution.R
individual.creator.function() # That's it
stats <- data.frame(row.names = 1:duration.years)
stats$time <- 1:duration.years
stats$av.rep.hosts <- NA
stats$av.rep.parasites <- NA
stats$av.exp.colonies <-NA
stats$av.host.popsize <- NA
stats$av.parasite.popsize <- NA
stats$av.ant.coloniespp <- NA

# Because it is a time forward simulation it is necessary to loop through the timesteps. We do that by calling the dynamics.wrapper function within a loop. 
# The dynamics.wrapper contains all the functions that guide the dynamics of the individuals in each timestep (as for example the host.reproduction.function).
for(i in 1 : duration.years){
  dynamics.wrapper()
  print(paste0("year ", i, " analysed"))
  stats[i, 2] <- length(Host[, rep(.I[Alive.Hosts$Is.Alive & Reproduction.Have > reproduction.threshold.host & Age > 1], times = round((Host[Alive.Hosts$Is.Alive & Reproduction.Have > reproduction.threshold.host & Age > 1 , Reproduction.Have] - reproduction.threshold.host) * reproduction.factor.host))])/ (replicates*length(host.populations))
  stats[i, 3] <- length(Parasite[,rep(.I[Alive.Parasites$Is.Alive & Adopted == 2], times = rpois(length(Parasite[, .I[Alive.Parasites$Is.Alive & Adopted == 2]]),lambda  = reproduction.factor.parasite))]) / (replicates*length(host.populations))
  stats[i, 4] <- length(Ant[,rep(.I[Alive.Ant$Is.Alive & Size >= 2])]) / (replicates*length(host.populations))
  stats[i, 5] <- Host[Alive.Hosts$Is.Alive, .N] / (replicates*length(host.populations))
  stats[i, 6] <- Parasite[Alive.Parasites$Is.Alive, .N] / (replicates*length(host.populations))
  stats[i, 7] <- Ant[Alive.Ant$Is.Alive, .N] / (replicates*length(host.populations))
    
  #print(mean(Host[Alive.Hosts$Is.Alive,Size]))
  #print(mean(Host[Alive.Hosts$Is.Alive,Age]))
  #print(length(reproducing.hosts))
  
  # result saving 
  if(i %in% c(1, seq(from = saving.intervall, to = duration.years, by = saving.intervall))){
    if(raw.results) {
      fwrite(Host[Alive.Hosts$Is.Alive], file = 
               paste(result.file.location, result.file.name, "_Host_", run.date, "_raw_", ".csv", sep = ""), append = TRUE)
      fwrite(Parasite[Alive.Parasites$Is.Alive], file = 
               paste(result.file.location, result.file.name, "_Parasite_", run.date, "_raw_", ".csv", sep = ""), append = TRUE)
    } 
    if(summarized.results) {
      temp.data.host <- copy(Host)
      temp.data.host[, Virulence := virulence]
      temp.data.host[, Popsize := host.populations[1]]
      temp.data.host[, Random.Drift := random.drift]
      temp.data.host[, Parasite.Connection := parasite.migration]
      temp.data.host[, Host.Connection := host.migration]
      temp.data.host[, Host.Time := Time]
      
      temp.data.host[, Host.Number.Individuals := .N, by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Host.Population.Size := .N, by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Host.Number.Individuals.Between := .N, by = list(Host.Time, Host.Replicate, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Host.Population.Size.Between := .N, by = list(Host.Time, Host.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Epidemic.Size.Within := sum(Infection.State), by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Epidemic.Size.Total := sum(Infection.State), by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      
      #########
      temp.data.parasite <- copy(Parasite)
      temp.data.parasite[, Virulence := virulence]
      temp.data.parasite[, Popsize := host.populations[1]]
      temp.data.parasite[, Random.Drift := random.drift]
      temp.data.parasite[, Parasite.Connection := parasite.migration]
      temp.data.parasite[, Host.Connection := host.migration]
      temp.data.parasite[, Parasite.Time := Time]
      
      temp.data.parasite[, Parasite.Number.Individuals := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Population, Parasite.Infection.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.parasite[, Parasite.Population.Size := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.parasite[, Parasite.Number.Individuals.Between := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Infection.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.parasite[, Parasite.Population.Size.Between := .N, by = list(Parasite.Time, Parasite.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      
      #########
      temp.data.host[, Total.Parasite.Number.Individuals := Epidemic.Size.Within + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1] & Parasite.Infection.Genotype == Immune.Genotype[1], .N], by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Total.Parasite.Population.Size := Epidemic.Size.Total + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1], .N], by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Total.Parasite.Number.Individuals.Between := Epidemic.Size.Within + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1] & Parasite.Infection.Genotype == Immune.Genotype[1], .N], by = list(Host.Time, Host.Replicate, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      temp.data.host[, Total.Parasite.Population.Size.Between := Epidemic.Size.Total + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1], .N], by = list(Host.Time, Host.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]
      
      #########
      fwrite(
        unique(temp.data.host[, list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Random.Drift, Parasite.Connection, Host.Connection, Host.Number.Individuals, Host.Population.Size, Host.Number.Individuals.Between, Host.Population.Size.Between, Epidemic.Size.Within, Epidemic.Size.Total, Total.Parasite.Number.Individuals, Total.Parasite.Population.Size, Total.Parasite.Number.Individuals.Between, Total.Parasite.Population.Size.Between, Origin)]), 
        file = paste(result.file.location, result.file.name, "_Host_", run.date, "_summarized_", ".csv", sep = ""), append = TRUE)
      
      fwrite(
        unique(temp.data.parasite[, list(Parasite.Time, Parasite.Replicate, Parasite.Population, Parasite.Infection.Genotype, Virulence, Popsize, Random.Drift, Parasite.Connection, Host.Connection, Parasite.Number.Individuals, Parasite.Population.Size, Parasite.Number.Individuals.Between, Parasite.Population.Size.Between)]), 
        file = paste(result.file.location, result.file.name, "_Parasite_", run.date, "_summarized_", ".csv", sep = ""), append = TRUE)
      
    }
  }
}

##############################################################################
### Post processing
# Loading the accumulated dataset, calculate some summary statistics and metrics, get rid of duplicate entries, and save a smaller dataset. This saves time and discspace.
#library(data.table)
#temp.data.host <- fread(paste(result.file.location, result.file.name, "_Host_", run.date, ".csv", sep = ""), header = TRUE)
#temp.data.host[, Virulence := virulence]
#temp.data.host[, Popsize := host.populations[1]]
#temp.data.host[, Random.Drift := random.drift]
#temp.data.host[, Parasite.Connection := parasite.migration]
#temp.data.host[, Host.Connection := host.migration]
#temp.data.host[, Host.Time := Time]

#temp.data.host[, Host.Number.Individuals := .N, by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Host.Population.Size := .N, by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Host.Number.Individuals.Between := .N, by = list(Host.Time, Host.Replicate, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Host.Population.Size.Between := .N, by = list(Host.Time, Host.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Epidemic.Size.Within := sum(Infection.State), by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Epidemic.Size.Total := sum(Infection.State), by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]

#########
#temp.data.parasite <- fread(paste(result.file.location, result.file.name, "_Parasite_", run.date, ".csv", sep = ""), header = TRUE)
#temp.data.parasite[, Virulence := virulence]
#temp.data.parasite[, Popsize := host.populations[1]]
#temp.data.parasite[, Random.Drift := random.drift]
#temp.data.parasite[, Parasite.Connection := parasite.migration]
#temp.data.parasite[, Host.Connection := host.migration]
#temp.data.parasite[, Parasite.Time := Time]

#temp.data.parasite[, Parasite.Number.Individuals := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Population, Parasite.Infection.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.parasite[, Parasite.Population.Size := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.parasite[, Parasite.Number.Individuals.Between := .N, by = list(Parasite.Time, Parasite.Replicate, Parasite.Infection.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.parasite[, Parasite.Population.Size.Between := .N, by = list(Parasite.Time, Parasite.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]

#########
#temp.data.host[, Total.Parasite.Number.Individuals := Epidemic.Size.Within + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1] & Parasite.Infection.Genotype == Immune.Genotype[1], .N], by = list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Total.Parasite.Population.Size := Epidemic.Size.Total + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1], .N], by = list(Host.Time, Host.Replicate, Host.Population, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Total.Parasite.Number.Individuals.Between := Epidemic.Size.Within + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1] & Parasite.Infection.Genotype == Immune.Genotype[1], .N], by = list(Host.Time, Host.Replicate, Immune.Genotype, Virulence, Popsize, Parasite.Connection, Host.Connection)]
#temp.data.host[, Total.Parasite.Population.Size.Between := Epidemic.Size.Total + temp.data.parasite[Parasite.Replicate == Host.Replicate[1] & Parasite.Population == Host.Population[1] & Parasite.Time == Host.Time[1], .N], by = list(Host.Time, Host.Replicate, Virulence, Popsize, Parasite.Connection, Host.Connection)]

#########
#temp.data.host.small <- unique(temp.data.host[, list(Host.Time, Host.Replicate, Host.Population, Immune.Genotype, Virulence, Popsize, Random.Drift, Parasite.Connection, Host.Connection, Host.Number.Individuals, Host.Population.Size, Host.Number.Individuals.Between, Host.Population.Size.Between, Epidemic.Size.Within, Epidemic.Size.Total,
#                                                     Total.Parasite.Number.Individuals, Total.Parasite.Population.Size, Total.Parasite.Number.Individuals.Between, Total.Parasite.Population.Size.Between, Origin)])
#temp.data.parasite.small <- unique(temp.data.parasite[, list(Parasite.Time, Parasite.Replicate, Parasite.Population, Parasite.Infection.Genotype, Virulence, Popsize, Random.Drift, Parasite.Connection, Host.Connection, Parasite.Number.Individuals, Parasite.Population.Size, Parasite.Number.Individuals.Between, Parasite.Population.Size.Between)])

#fwrite(temp.data.host.small, file = paste(result.file.location, result.file.name, "_Host_", run.date, ".csv", sep = ""))
#fwrite(temp.data.parasite.small, file = paste(result.file.location, result.file.name, "_Parasite_", run.date, ".csv", sep = ""))
