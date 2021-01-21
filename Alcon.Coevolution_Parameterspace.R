  ##################################################################
### Main Parameterspace of the Digital_Coevolution simulation ####
##################################################################

## You shouldn't be here! 
## Warning: Only change stuff in here if you know what you are doing...

###########################################################################
###### Parameter space
# Internal dynamic parameters
# Host internal dynamic parameters
host.size <- "OFF" 
age.threshold.host <- 30
resource.threshold.host <- 0.2
reproduction.threshold.host <- 0.1 # with host.size off and a resource grain of 10, a reproduction threshold of 2 should lead to a first clutch at an age of 10. # Ancient param: 10
reproduction.factor.host <- 2#6
reproduction.allocation <- 0.35
immune.allocation <- 0.01

#Parasite internal dynamic parameters
parasite.genotypes <- host.genotypes
age.threshold.parasite <- 3 #60
reproduction.factor.parasite <-  5 #23

#Infection dynamic parameters
infection.table <- matrix((1 - parasite.specificity), nrow = host.genotypes, ncol = parasite.genotypes)   # row is host, column is parasite
diag(infection.table) <- 1
infection.growth.factor <- 1.15 # This factor gives the per time unit growth of a infection, in percent

# Genetic parameters
#neutral.mutation.rate <- 0.001

# Resource parameters
resource.grain <- 10 # This is the modifier that can increase or decrease the variance in resource distribution. The higher the number the less variance. With value 1 its a normal poisson distribution

######################################################################
######################################################################
# Here at the same time the number of populations and their size are set. For each population define explicitly the amount of resources it receives daily. Currently, population size settles at about half the resources given. 
resources.host <- host.populations
######################################################################
number.populations.host <- length(resources.host)
populations.host <- c(1 : number.populations.host)
starting.population.sizes.host <- ceiling(resources.host / 2)
#############
number.populations.parasite <- number.populations.host
populations.parasite <- c(1 : number.populations.parasite)
starting.population.sizes.parasite <- ceiling(resources.host / 2)
###Ant
number.populations.ant <- length(resources.host)
populations.ant <- c(1 : number.populations.ant)
starting.colony.number.ant <- ceiling(resources.host / (gentians.per.colony)) 

###
Carrying.Capacity <- starting.colony.number.ant * K.factor

######################################################################
# External Population dynamic parameters:
migration.matrix.host <- matrix((host.migration / number.populations.host), nrow = number.populations.host, ncol = number.populations.host)
diag(migration.matrix.host) <- diag(migration.matrix.host) + (1 - host.migration)

migration.matrix.parasite <- matrix((parasite.migration / number.populations.parasite), nrow = number.populations.parasite, ncol = number.populations.parasite)
diag(migration.matrix.parasite) <- diag(migration.matrix.parasite) + (1 - parasite.migration)
