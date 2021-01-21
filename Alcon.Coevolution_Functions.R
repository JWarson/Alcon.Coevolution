########################################################
### Main Body of the Digital_Coevolution simulation ####
########################################################

## You shouldn't be here! 
## Warning: Only change stuff in here if you know what you are doing...

######################################################################################################################################
## Here I create the data.tables that contain each individual of the simulation. Each row is one individual, each column one trait. 
######################################################################################################################################

###################################################################### 
# In this step I preallocate all the data.table space that I could be using in an attempt to speed up the simulation. 
individual.creator.function <- function(){  
  
  preallocation.margin <- 20 # How many times larger is the maximum calculatable population size #15
  parasite.margin <- 1
  preallocation.length <- sum(starting.population.sizes.host * replicates * preallocation.margin)
  preallocation.parasite <- sum(starting.population.sizes.parasite * replicates * preallocation.margin * parasite.margin)
  preallocation.ant <- sum(starting.colony.number.ant * replicates * preallocation.margin)
  
  if (exists("Host")) {
    rm(Host, pos = ".GlobalEnv")
  }
  Host <<- data.table(  
    Alive = integer(preallocation.length),
    Host.Replicate = integer(preallocation.length), 
    Time = integer(preallocation.length),
    Host.Population = integer(preallocation.length),
    Host.Infection.Genotype = factor(NA, levels = c(1 : parasite.genotypes)),
    Age = integer(preallocation.length),
    Reproduction.Allocation = numeric(preallocation.length),
    Immune.Allocation = numeric(preallocation.length),
    Immune.Genotype = factor(sample(c(1 : host.genotypes),size = preallocation.length,prob = rep(1 / host.genotypes,host.genotypes), replace = T), levels = c(1 : host.genotypes)),
    Resource = numeric(preallocation.length),
    Reproduction.Have = numeric(preallocation.length),
    Immune.State = numeric(preallocation.length),
    Infection.State = integer(preallocation.length),
    Infection.Size = numeric(preallocation.length),
    Host.TempID = integer(preallocation.length),
    Size = numeric(preallocation.length),
    Host.Generation = integer(preallocation.length),
    Origin = integer(preallocation.length),
    surv.by.age = numeric(preallocation.length)
  )
  
  ### Here I initialize the starter populations
  # Replicate, this way I hope to paralellize the replicate calculations
  Host[, Host.Replicate := c(rep(1 : replicates, each = sum(starting.population.sizes.host)), integer(preallocation.length - sum(starting.population.sizes.host) * replicates))]
  # Alive variable, 1 = alive, 0 = not alive
  Host[Host.Replicate != 0, Alive := 1L] 
  # Population
  Host[Alive == 1, Host.Population := as.integer(rep(1 : number.populations.host, times = starting.population.sizes.host)), by = Host.Replicate]
  # Age
  Host[Alive == 1, Age := 0L]
  # Resource.In, meaning that the starters start with a full belly
  Host[Alive == 1, Resource := 1]
  # Reproduction.Allocation
  Host[Alive == 1, Reproduction.Allocation := reproduction.allocation]
  # Immune.Allocation
  Host[Alive == 1, Immune.Allocation := immune.allocation]  
  # Immune.Genotype, has been set when initializing, needs to be cleaned
  Host[Alive == !1, Immune.Genotype := NA] 
  # Infection.Genotype
  Host[, Host.Infection.Genotype := NA]
  # Size
  Host[Alive == 1, Size := 1]
  # Host.Generation
  Host[Alive == 1, Host.Generation := 1L]
  # Origin
  Host[Alive == 1, Origin := Host.Population]
  # survival chance by age 
  Host[Alive == 1, surv.by.age := 1L]
  
  # Here I start a vector that just contains if a host is alive or not, to circumvent all the lookups
  if(exists("Alive.Host")) {
    rm(Alive.Host, pos = ".GlobalEnv")
  }
  Alive.Hosts <<- data.table(Is.Alive = Host[, Alive == 1], Is.Infected = Host[, Infection.State == 1])
  
  # Here I initialize the parasite data.table structure
  if(exists("Parasite")) {
    rm(Parasite, pos = ".GlobalEnv")
  }
  Parasite <<- data.table(
    Alive = integer(preallocation.parasite),
    Parasite.Replicate = integer(preallocation.parasite), 
    Time = integer(preallocation.parasite),
    Parasite.Population = integer(preallocation.parasite),
    Parasite.Infection.Genotype = factor(sample(c(1 : parasite.genotypes), size = preallocation.parasite, prob = rep(1 / parasite.genotypes, parasite.genotypes), replace = T), levels = c(1 : parasite.genotypes)),
    Attack.Host.TempID = integer(preallocation.parasite),
    Attack.Host.Genotype = factor(sample(c(1 : parasite.genotypes), size = preallocation.parasite, prob = rep(1 / parasite.genotypes, parasite.genotypes), replace = T), levels = c(1 : parasite.genotypes)), # integer(preallocation.parasite),
    Success.Parasite.Infection.Genotype = factor(NA, levels = c(1 : parasite.genotypes)),
    Assigned = integer(preallocation.parasite),
    Adopted = integer(preallocation.parasite),
    Age = integer(preallocation.parasite)
  )
  
  # Here I initialize the starter populations for the parasite
  # Replicate
  Parasite[, Parasite.Replicate := c(rep(1 : replicates, each = sum(starting.population.sizes.parasite)), integer(preallocation.parasite - sum(starting.population.sizes.parasite) * replicates))]
  # Alive variable, 1 = alive, 0 = not alive
  Parasite[Parasite.Replicate != 0, Alive := 1L]
  # Population
  Parasite[Alive == 1, Parasite.Population := as.integer(rep(1 : number.populations.parasite, times = starting.population.sizes.parasite)), by = Parasite.Replicate]
  # Age 
  Parasite[Alive == 1, Age :=0L]
  # Infection.Genotype, has been set when initializing, needs to be cleaned
  Parasite[Alive != 1, Parasite.Infection.Genotype := NA]
  # Atack.Host.Rownumber
  Parasite[, Attack.Host.TempID := NA]
  # Atack.Host.Genotype
  Parasite[, Attack.Host.Genotype := NA]
  
  # Here I start a vector that just contains if a Parasite is alive or not, to circumvent all the lookups
  if(exists("Alive.Parasite")) {
    rm(Alive.Parasite, pos = ".GlobalEnv")
  }
  Alive.Parasites <<- data.table(Is.Alive = Parasite[, Alive == 1], Is.Assigned = FALSE)
  
  ##Ant##
  #######
  if(exists("Ant")) {
    rm(Ant, pos = ".GlobalEnv")
  }
  #Ant data table
  Ant <<- data.table(
    Alive = integer(preallocation.ant),
    Ant.Replicate = integer(preallocation.ant),
    Time = integer(preallocation.ant),
    Ant.Population = integer(preallocation.ant),
    Size = numeric(preallocation.ant),
    Infected = integer(preallocation.ant),
    K = integer(preallocation.ant),
    Exposure.chance = numeric(preallocation.ant),
    Infection.Size = integer(preallocation.ant),
    Infection.Age = integer(preallocation.ant)
  )
  
  #Initialization
  #replicate
  Ant[, Ant.Replicate := c(rep(1 : replicates, each = sum(starting.colony.number.ant)), integer(preallocation.ant - sum(starting.colony.number.ant) * replicates))]
  # Alive variable, 1 = alive, 0 = not alive
  Ant[Ant.Replicate != 0, Alive := 1L]
  #population
  Ant[Alive == 1, Ant.Population := as.integer(rep(1 : number.populations.ant, times = starting.colony.number.ant)), by = Ant.Replicate]
  #size
  Ant[Alive == 1, Size := 1L]
  #carrying capacity K, attaches the carrying capacity of each population to the data table (a fixed K would only be correct when all populations got same amount of resources)
  Ant[Alive == 1, K := rep(Carrying.Capacity, times = starting.colony.number.ant), by = Ant.Replicate]
  
  #ant alive vector
  Alive.Ant <<- data.table(Is.Alive = Ant[, Alive == 1])
}


######################################################################################################################################
## Here I define the dynamics that make up each individual of the simulation. Each of these functions will be carried out once per tick
######################################################################################################################################

######################################################################################################################################
#### Time marker, counts the number of time steps that the simulation has been though # time.function
time.function <- function() {
  Host[, Time := Time + 1L]
  Parasite[, Time := Time + 1L]
  Ant[, Time := Time + 1L]
}
######################################################################################################################################
# Ageing functions. this function adds one age to each individuals age spot, and then kills of the that have used too much energy or are too old
senescence.function <- function() {
  # additional drift parameter
  Host[Alive.Hosts$Is.Alive, Alive := rbinom(n = .N, size = 1, prob = (1 - random.drift))]
  Parasite[Alive.Parasites$Is.Alive, Alive := rbinom(n = .N, size = 1, prob = (1 - random.drift))]
    
  # senescence
  Host[Alive.Hosts$Is.Alive, Age := Age + 1L]
  Parasite[Alive.Parasites$Is.Alive, Age := Age + 1L]
  Parasite[Alive.Parasites$Is.Alive & Age > age.threshold.parasite, Alive := 0L]
  
  # make change of dying proportional to age 
  Host[Alive.Hosts$Is.Alive & Age <= age.threshold.host, surv.by.age := (1 - (Age/(age.threshold.host + 1)))]

  # add chance on dying based on excess of energy used
  # put resource on 0 if there are leftovers of energy (used for vital functions that are not explicitly defined in the model)
  Host[Alive.Hosts$Is.Alive & Resource > 0, Resource := 0L]

  
  Host[Alive.Hosts$Is.Alive, 
       Alive := rbinom(n = .N, size = 1, 
                       prob = ((surv.by.age * (pmax((Immune.Allocation * Size), abs(Resource)) - abs(Resource)))/(Immune.Allocation * Size)))]

  # updating the host and parasite alive vectors
  set(Alive.Hosts, j = "Is.Alive", value = Host[, Alive == 1])
  #Alive.Hosts[, Is.Alive := Host[, Alive == 1]]
  set(Alive.Parasites, j = "Is.Alive", value = Parasite[, Alive == 1])
  #Alive.Parasites[, Is.Alive := Parasite[, Alive == 1]]
  
}


######################################################################################################################################
# Resource functions
 host.resource.function <- function(){
  # This part takes all the available resources, and redistributes them according to host size.
  #those that were infected last year (Is.Infected) will get less resources
  Host[Alive.Hosts$Is.Alive & Alive.Hosts$Is.Infected == F, 
       Resource := (rpois(n = .N, 
                          lambda = (Size * min(1, (resources.host[Host.Population[1]] / sum(Size)))) * resource.grain) / resource.grain), 
       by = list(Host.Population, Host.Replicate)]
   
  Host[Alive.Hosts$Is.Alive & Alive.Hosts$Is.Infected, 
       Resource := (1 - next.year.withdrawal)*(rpois(n = .N, 
                                                   lambda = (Size * min(1, (resources.host[Host.Population[1]] / sum(Size)))) * resource.grain) / resource.grain), 
       by = list(Host.Population, Host.Replicate)]
  
  # This part restricts resource in to Size + 10% in order to avoid unrealistic overfeeding
  Host[Alive.Hosts$Is.Alive, Resource := pmin(Resource, (Size * 1.1))]
}

#####################################################################################################################################
# Infection.function.host; Here the infection grows and then withdraws resources from the host with a limitation of the size of the host
infection.function <- function(){ 
  
  # parasite draws resources, dependent on virulence of parasite
  if (sum(over.inf.hosts$Var1) > 0){
    Host[over.inf.hosts$Var1, Infection.Size := over.inf.hosts$Var1/density.dependence] #rescales infection size, if density = density.dependance (15) -> inf.size = 1
    Host[Alive.Hosts$Is.Alive, Resource := Resource - (Infection.Size * virulence)]
  }
}

#####################################################################################################################################  

alter.parasite.reproduction <- function () {
  reproducing.parasites <- Parasite[,rep(.I[Alive.Parasites$Is.Alive & Adopted == 2], 
                                         times = rpois(length(Parasite[, .I[Alive.Parasites$Is.Alive & Adopted == 2]]),
                                                       lambda  = reproduction.factor.parasite))]
  if (Parasite[reproducing.parasites, .N] > 0) {
    Parasite[Parasite[, .I[!Alive.Parasites$Is.Alive][seq(Host[reproducing.parasites, .N])]],
             `:=` (Alive = 1L, Parasite.Replicate = Parasite[reproducing.parasites, Parasite.Replicate], Parasite.Population = Parasite[reproducing.parasites, Parasite.Population], Parasite.Infection.Genotype = Parasite[reproducing.parasites, Parasite.Infection.Genotype], Attack.Host.TempID = NA, Attack.Host.Genotype = NA, Success.Parasite.Infection.Genotype = NA, Ingested = 0, Age = 1L)
    ]
    set(Alive.Parasites, j = "Is.Alive", value = Parasite[, Alive == 1])
  }
}

######################################################################################################################################
## exposure.function; here the parasite infects new hosts 
host.exposure.function <- function(){
  #This part sets an identifier to be used later on
  Host[Alive.Hosts$Is.Alive, Host.TempID := 1:.N]
  Host[!Alive.Hosts$Is.Alive, Host.TempID := NA]
  
  set(Parasite, j = "Assigned", value = 0)
  set(Parasite, j = "Attack.Host.TempID", value = NA)
  set(Parasite, j = "Attack.Host.Genotype", value = NA)
  set(Parasite, j = "Success.Parasite.Infection.Genotype", value = NA)
  
  # This part does assign to each parasite score if it has been assigned to a host, dependent on resource availability, host and parasite population size
  ##########
  Parasite[Alive.Parasites$Is.Alive & Age == 1, 
           Assigned := rbinom(n = .N, size = 1, prob = min(1, (sum(Host[Alive.Hosts$Is.Alive & Host.Population == Parasite.Population[1] & Host.Replicate == Parasite.Replicate[1], Size]) / resources.host[Parasite.Population[1]]))),
           by = list(Parasite.Population, Parasite.Replicate)]
  
  # Updating the Alive.Parasites$Is.Assigned vector for faster lookups
  Alive.Parasites[Alive.Parasites$Is.Alive, Is.Assigned := Parasite[Alive.Parasites$Is.Alive, Assigned == 1]]
  
  # This part does assigns to each parasite the host tempID it has been assigned to 
  Parasite[Alive.Parasites$Is.Assigned,
           Attack.Host.TempID := 
             base:::sample(x = Host[Alive.Hosts$Is.Alive & Host.Population == Parasite.Population[1] & Host.Replicate == Parasite.Replicate[1], Host.TempID], 
                           size = .N, 
                           replace = TRUE, 
                           prob = c(
                             Host[Alive.Hosts$Is.Alive & Host.Population == Parasite.Population[1] & Host.Replicate == Parasite.Replicate[1], Size] * 
                               (.N / resources.host[Parasite.Population[1]]) * 
                               min(1, resources.host[Parasite.Population[1]] / 
                                     sum(Host[Alive.Hosts$Is.Alive & Host.Population == Parasite.Population[1] & Host.Replicate == Parasite.Replicate[1], Size]))
                           )
             ), 
           by = list(Parasite.Population, Parasite.Replicate)]
  
  # This part does assign to each parasite the host genotype it has been Assigned to
  Parasite[Alive.Parasites$Is.Assigned, Attack.Host.Genotype := Host[Alive.Hosts$Is.Alive][Attack.Host.TempID, Immune.Genotype]]
  
  # This part calculates which parasite successfully infects, takes into account the relative abundance of Assigned parasites and their genetic specificity
  Parasite[Alive.Parasites$Is.Assigned & !is.na(Attack.Host.TempID), Success.Parasite.Infection.Genotype := sample(c(NA, Parasite.Infection.Genotype), size = 1, prob = c(1,infection.table[Attack.Host.Genotype[1], Parasite.Infection.Genotype]), replace = TRUE), by = list(Attack.Host.TempID, Parasite.Population, Parasite.Replicate)]
  
  # And the last thing to do would be to assign the infection genotype back to the host
  #setorder(Parasite, - Alive, Attack.Host.TempID, na.last = TRUE)
  infected.hosts <- unique(Parasite[Alive.Parasites$Is.Assigned & !is.na(Success.Parasite.Infection.Genotype)], by = "Attack.Host.TempID")$Attack.Host.TempID
  infected.hosts.infection.genotypes <- unique(Parasite[Alive.Parasites$Is.Assigned & !is.na(Success.Parasite.Infection.Genotype)], by = "Attack.Host.TempID")$Success.Parasite.Infection.Genotype
  Host[Host[, .I[Alive.Hosts$Is.Alive]][infected.hosts],
       Host.Infection.Genotype := infected.hosts.infection.genotypes
       ]
  #Host[Host.TempID %in% infected.hosts, Host.Infection.Genotype := infected.hosts.infection.genotypes] 
  
  # And the very last thing is to update the infection status of the host that got assigned a infection.genotype
  Host[Alive.Hosts$Is.Alive & !is.na(Host.Infection.Genotype) & Infection.State == 0, c("Infection.Size", "Infection.State") := 1]
  
  # And kill the parasites that have been Assigned ##We dont want this
  # Parasite[Alive.Parasites$Is.Assigned, Alive := 0]
  
  # Update the parasite alive vector
  # setorder(Parasite, -Alive)
  set(Alive.Parasites, j = "Is.Alive", value = Parasite[, Alive == 1])
  #Alive.Parasites[, Is.Alive := Parasite[, Alive == 1]]
  
  # Update the Alive.Paraistes$Is.Assigned vector
  set(Alive.Parasites, j = "Is.Assigned", value = FALSE)
  
  #Update Alive.Hosts$Is.Infected
  set(Alive.Hosts, j = "Is.Infected", value = Host[, Infection.State == 1])
}

#########################
#Density dependent parasite mortality
#If more than 8 together on 1 gentian -> chance to die is (density - 8)/density
dens.dep.mortality <- function () {
  over.inf.hosts <- as.data.frame(table(Parasite$Attack.Host.TempID))
  over.inf.hosts2 <- subset(over.inf.hosts, over.inf.hosts$Freq > density.dependence)
  if (length(over.inf.hosts$Var1) > 0){
    Parasite[Parasite$Attack.Host.TempID %in% over.inf.hosts2$Var1, Alive := rbinom(sum(Parasite$Attack.Host.TempID %in% over.inf.hosts2$Var1),1, density.dependence/over.inf.hosts2[Attack.Host.TempID, "Freq"])]
    Parasite[is.na(Parasite$Alive), Alive := 0L] #dens.dep.mortality creates a few na's, here set to 0
    #update alive vector
    set(Alive.Parasites, j = "Is.Alive", value = Parasite[, Alive == 1])
  }
}

#####################################################################################################################################
# metabol.funtion removes energy from the host to allocate it towards immunity buildup
metabolism.function <- function(){
  Host[Alive.Hosts$Is.Alive, Immune.State := Immune.Allocation * Size] 
  Host[Alive.Hosts$Is.Alive, Resource := Resource - (Immune.Allocation * Size)]
  
  # If the host has still energy left after energy is used for immunity the host will allocate an amount of energy for reproduction based on its size, age 
  # and a predefined reproduction allocation value 
  Host[Alive.Hosts$Is.Alive & Resource >= 0 & Age >= 2, 
       Reproduction.Have := Reproduction.Have + (pmin(Reproduction.Allocation * Size, Resource) * (1 - (2 / Age)))]
 
  # optional function where part of the energy is allocated towards growth  
  # Depending on the age of the host, resources are funneled more towards reproduction or more towards growth
  if (host.size == "ON"){
    Host[Alive.Hosts$Is.Alive & Resource >= 0, Size := Size + (pmin(Reproduction.Allocation * Size, Resource) * (2 / Age))]
  }
  Host[Alive.Hosts$Is.Alive & Resource >= 0, Resource := Resource - pmin(Reproduction.Allocation * Size, Resource)]
}

######################################################################################################################################
# Reproduction function #host.reproduction.function
host.reproduction.function <- function(){
  # Looks complicated, does first chose the current overall population size, sets the reproduction lenght and then updates the respective columns.
  # It Calculates the position of all hosts with enough resources to reproduce, the repeats those positions for the number of times defined by the resources of each.
  # Of this it calculates the length to set the reproduction length. Then it choses the same subset of the host population and mirrors it down in the dataframe into the precalculated empty positions.
  reproducing.hosts <- Host[, rep(.I[Alive.Hosts$Is.Alive & Reproduction.Have > reproduction.threshold.host], times = round((Host[Alive.Hosts$Is.Alive & Reproduction.Have > reproduction.threshold.host, Reproduction.Have] - reproduction.threshold.host) * reproduction.factor.host))]
  
  if (Host[reproducing.hosts, .N] > 0) {
    
    ############### 
    Host[Host[, .I[!Alive.Hosts$Is.Alive][seq(Host[reproducing.hosts, .N])]],
         `:=` (Alive = 1L, Host.Replicate = Host[reproducing.hosts, Host.Replicate], Host.Population = Host[reproducing.hosts, Host.Population], Host.Infection.Genotype = NA, Age = 1, Resource = 1, Reproduction.Allocation = Host[reproducing.hosts, Reproduction.Allocation], Immune.Allocation = Host[reproducing.hosts, Immune.Allocation], Immune.Genotype = Host[reproducing.hosts, Immune.Genotype], Reproduction.Have = 0, Immune.State = Host[reproducing.hosts, Immune.State], Infection.State = 0L, Infection.Size = 0, Parasite.Resources = 0, Host.TempID = NA, Size = 1, Host.Generation = (Host[reproducing.hosts, Host.Generation] + 1L), Origin = Host[reproducing.hosts, Host.Population])
         #c("Alive","Host.Replicate","Host.Population","Reproduction.Allocation","Immune.Allocation","Immune.Genotype", "Immune.State", "Age", "Resource.Have", "Resource.In", "Size", "Resource.Work", "Reproduction.Have", "Origin", "Host.Generation", "Parasite.Generation") := c(Host[reproducing.hosts, list(Alive, Host.Replicate, Host.Population, Reproduction.Allocation, Immune.Allocation, Immune.Genotype, Immune.State)], list(Age = 1, Resource.Have = 1, Resource.In = 1, Size = 1, Resource.Work = 0, Reproduction.Have = 0), list(Origin = Host[reproducing.hosts, Host.Population],  Host.Generation = (Host[reproducing.hosts, Host.Generation] + 1L), Parasite.Generation = 0))
         ]
    ##############
    Host[Alive.Hosts$Is.Alive & Reproduction.Have > reproduction.threshold.host, Reproduction.Have := Reproduction.Have - (round(Reproduction.Have) - reproduction.threshold.host)]
  
    # Uptdate the Alive.Hosts$Is.Alive vector
    set(Alive.Hosts, j = "Is.Alive", value = Host[, Alive == 1])
    #Alive.Hosts[, Is.Alive := Host[, Alive == 1]]
    }

}

######################################################################################################################################
# Migration.functions 1
#####################################
## Parasite migration function  #parasite.migration.function
parasite.migration.function <- function(){
  Parasite[Alive.Parasites$Is.Alive, 
           Parasite.Population := base:::sample(1:number.populations.parasite, 
                                                size = .N, 
                                                prob = migration.matrix.parasite[Parasite.Population[1], ], 
                                                replace = TRUE), 
           by = list(Parasite.Population, Parasite.Replicate)]
}
### Maybe this part of the simulation needs to be updated. Currently this will cause problems with uneven population sizes
### Something that takes the existing population vector and shuffles it around maybe. something that takes the indicated fraction of 
### the population, pools it into the global parasite population, and backdistributes it. 
### That way the relative population sizes are maintained.
# 
######################################################################################################################################
# Migration.functions 2
## Host migration function #host.migration.function
host.migration.function <- function(){
  Host[Alive.Hosts$Is.Alive, 
       Host.Population := base:::sample(1:number.populations.host, 
                                        size = .N, 
                                        prob = migration.matrix.host[Host.Population[1], ], 
                                        replace = TRUE),
       by = list(Host.Population, Host.Replicate)]
} 

#################
##Ant functions##
##################################################################################
##Colony growth
colony.growth <- function(){
  #growth rate rescaled ifo carrying capacity. if N = K -> no growth
  Ant[Alive.Ant$Is.Alive, Size := Size * (1 + (colony.growth.rate * (K - .N)/K)), by = list(Ant.Replicate, Ant.Population)]
 }

#Ant exposure function: chance to get infected depends on amount of infected plant
# 3 factors contribute to higher infection chance: % infected gentians in pop, gentians per colony and colony size (bigger colony higher chance)
#now exposure chance = %infected gentians * Size/max size in population (rescaled size)
exposure.ant <- function(){
  if (Host[Alive.Hosts$Is.Alive & Infection.State == 1, .N] > 0){ #if there are infected hosts
    num.infected.gentians <- (Host[Alive.Hosts$Is.Alive & Infection.State == 1, .N, by = list(Host.Replicate, Host.Population)])
    #the following is needed to ensure that an uninfected population has '0' infected (.N does not count 0's)
    infection.table <- CJ(c(1:replicates), c(1:length(populations.ant)))
    colnames(infection.table) <- c('Host.Replicate', 'Host.Population')
    infection.table$Infected <- rep(0, times = replicates*length(populations.host)) #assigns 0 to all populations
    setDT(infection.table)[setDT(num.infected.gentians), on = c("Host.Replicate", "Host.Population"), Infected := N] #replaces zero with actual number of infected gentians (so uninfected pop remain 0)
    
    total.gentians <- (Host[Alive.Hosts$Is.Alive, .N, by = list(Host.Replicate, Host.Population)])
    infection.table$Infected <- infection.table$Infected / total.gentians$N
    colonies.per.pop.rep <- Ant[Alive.Ant$Is.Alive, .N, by = list(Ant.Replicate,Ant.Population)]
    max.size.colonies <- Ant[Alive.Ant$Is.Alive , .SD[which.max(Size)], by = list(Ant.Population, Ant.Replicate)]
    
    #Ant[, Exposure.chance :=  Ant[,(rep(prop.infected.gentians[,N], colonies.per.pop.rep[,N]))*(Size/(rep(max.size.colonies[,Size], colonies.per.pop.rep[,N])))]]
    Exposure.chance <- Ant[Alive.Ant$Is.Alive,(rep(infection.table[,Infected], times = colonies.per.pop.rep[,N]))*(Size/(rep(max.size.colonies[,Size], times = colonies.per.pop.rep[,N])))]
    Ant[Alive.Ant$Is.Alive, Infected := rbinom(n = .N, 1, prob = Exposure.chance)] #apply probability of colony being infected
  }
}

#virulence.function.ant; if colony is infected, size will reduce
virulence.ant <- function(){
  Ant[Infected == 1, Size := Size * (1 - parasitism.ant)]
  Ant[Alive.Ant$Is.Alive & Size < ant.size.threshold, Alive := 0] #Size below threshold -> colony dies
  set(Alive.Ant,j = "Is.Alive", value = Ant[, Alive == 1]) #update alive vector
}

#colony expansion function (size =< 2 -> colony splits in two equal colonies (we could integrate a 'cost of splitting' by small size reduction in the new colony))
colony.expansion <- function(){
  expanding.colonies <- Ant[, rep(.I[Alive.Ant$Is.Alive & Size >= 2])] #selects row of big enough colonies
  if (Ant[expanding.colonies, .N] > 0) {
    Ant[Ant[, .I[!Alive.Ant$Is.Alive][seq(Ant[expanding.colonies, .N])]],
        `:=`(Alive = 1L, Ant.Replicate = Ant[expanding.colonies, Ant.Replicate], Ant.Population = Ant[expanding.colonies, Ant.Population], Age = 1, Size = 1, K = Ant[expanding.colonies, K])]
    Ant[expanding.colonies, Size := Size / 2] 
    set(Alive.Ant,j = "Is.Alive", value = Ant[, Alive == 1]) #update alive vector
  }
}

##After 2 years: infection state back to zero + parasite can infect plant again


##parasite allocation function (parasites on plants move to ant)
parasite.allocation <- function() {
  parasites.to.allocate <-  Parasite[Alive.Parasites$Is.Alive & (Adopted == 1 | Age == 1), .N, by = list(Parasite.Replicate, Parasite.Population)]
  
  if (sum(parasites.to.allocate$N) > 0) {
    #update parasite DT
    Parasite[Alive.Parasites$Is.Alive & Adopted == 2 , Alive := 0L] #parasites that are 2 years old will have reproduced and now die
    #those adopted last year move on and can reproduce adopted == 2 (then die)
    Parasite[Alive.Parasites$Is.Alive & Adopted == 1, Adopted := Adopted + 1L] 
    #70% chance of being adopted and Assigned to 0 (moved on to ant)
    Parasite[Alive.Parasites$Is.Alive & (Age == 1), 
             `:=`(Adopted = rbinom(n = .N, 1, 0.7), Assigned = 0L)] 
    #parasites that are not adopted die
    Parasite[Alive.Parasites$Is.Alive & Adopted == 0 , Alive := 0L] 
    #update alive vector
    set(Alive.Parasites, j = "Is.Alive", value = Parasite[, Alive == 1]) 
    #update ant DT
    infected.colonies.per.pop.rep <- Ant[Alive.Ant$Is.Alive & Infected == 1, .N, by = list(Ant.Replicate,Ant.Population)]
    temp.size.calc <- (0.7 * parasites.to.allocate[,N])/infected.colonies.per.pop.rep[,N]
    Ant[Alive.Ant$Is.Alive & Infected == 1, Infection.Size := Infection.Size + round(rnorm(.N, mean = rep(temp.size.calc, infected.colonies.per.pop.rep[,N]), sd = 2))] #sd might need adjustment, but rnorm allows some variance
    #update alive vector
    set(Alive.Ant,j = "Is.Alive", value = Ant[, Alive == 1]) 
    #update host DT
    Host[Alive.Hosts$Is.Alive, 
         `:=`(Infection.State = 0L, Infection.Size = 0L)]
  }
}
######################################################################################################################################
# Here I will combine all the functions defined above into one wrapper function to be called. One call is one timestep. The order of the function can have an influence on the dynamics of the thing
dynamics.wrapper <- function(){
  time.function()
  senescence.function()
  alter.parasite.reproduction()
  host.resource.function()
  host.exposure.function()
  dens.dep.mortality()
  infection.function()
  metabolism.function()
  host.reproduction.function()
  exposure.ant()
  colony.growth()
  virulence.ant()
  colony.expansion()
  parasite.allocation()
  host.migration.function()
  parasite.migration.function()
}
