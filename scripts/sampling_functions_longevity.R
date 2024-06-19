#########################################################################################################
##  set mortality parameters
############################################################################################################

# fitness parameters
h <- 0.5     # SR is intermediate between SS, RR - default

# LLIN mortality
mu_RR_ITN <- c(0.005, 0.219, 0.391)    ## high resistance (TIA) strain ITN mortality (Viana et al. 2017)

mu_SS_ITN <- c(1,1,1)                     
mu_SR_ITN <- c((mu_SS_ITN[1]*(1-h) + mu_RR_ITN[1]*h),   ## average weighted by h
               (mu_SS_ITN[2]*(1-h) + mu_RR_ITN[2]*h), 
               (mu_SS_ITN[3]*(1-h) + mu_RR_ITN[3]*h))  

## daily mortality parameters from data
mortality <- read.csv( file="input/mortality.csv", header=TRUE, sep=',')
mu_SS_eggs <- 0.2
mu_SS_pupae <- 0.25
mu_SS_E0 <- mortality[,2]
SS_survival <- 1-mu_SS_E0
mu_SS_E1 <- mortality[,2]
mu_SS_E2 <- mortality[,2]

mu_RR_eggs <- 0.2
mu_RR_pupae <- 0.25
mu_RR_E0 <- mortality[,2]
RR_survival <- 1-mu_RR_E0
mu_RR_E1 <- mortality[,4]
mu_RR_E2 <- mortality[,4]

mu_SR_eggs <- 0.2
mu_SR_pupae <- 0.25
mu_SR_E0 <- mu_SS_E0 + h*(mu_RR_E0 - mu_SS_E0)
SR_survival <- 1-mu_SR_E0
mu_SR_E1 <- mu_RR_E1
mu_SR_E2 <- mu_RR_E2


############################################################################################################
##  set up probability of survival from each adult compartment
############################################################################################################

population.type <- list()

population.type[["adult"]] <- c(
  ##SS
  "SS_E0_d1", "SS_E0_d2", "SS_E0_d3", "SS_E0_d4", "SS_E0_d5", "SS_E0_d6", "SS_E0_d7", "SS_E0_d8", "SS_E0_d9", "SS_E0_d10", 
  "SS_E0_d11", "SS_E0_d12", "SS_E0_d13", "SS_E0_d14", "SS_E0_d15", "SS_E0_d16", "SS_E0_d17", "SS_E0_d18", "SS_E0_d19",
  "SS_E0_d20", "SS_E0_d21", "SS_E0_d22", "SS_E0_d23", "SS_E0_d24", "SS_E0_d25", "SS_E0_d26", 
  "SS_E1_d4", "SS_E1_d5", "SS_E1_d6", "SS_E1_d7", "SS_E1_d8", "SS_E1_d9", "SS_E1_d10", "SS_E1_d11", "SS_E1_d12", "SS_E1_d13",
  "SS_E1_d14", "SS_E1_d15", "SS_E1_d16", "SS_E1_d17", "SS_E1_d18", "SS_E1_d19", "SS_E1_d20", "SS_E1_d21", "SS_E1_d22", 
  "SS_E1_d23", "SS_E1_d24", "SS_E1_d25", "SS_E1_d26", 
  "SS_E2_d8", "SS_E2_d9", "SS_E2_d10", "SS_E2_d11", "SS_E2_d12", "SS_E2_d13", "SS_E2_d14", "SS_E2_d15", "SS_E2_d16", 
  "SS_E2_d17", "SS_E2_d18", "SS_E2_d19", "SS_E2_d20", "SS_E2_d21", "SS_E2_d22", "SS_E2_d23", "SS_E2_d24", "SS_E2_d25", 
  "SS_E2_d26", 
                   
  ## SR
  "SR_E0_d1", "SR_E0_d2", "SR_E0_d3", "SR_E0_d4", "SR_E0_d5", "SR_E0_d6", "SR_E0_d7", "SR_E0_d8", "SR_E0_d9", "SR_E0_d10", 
  "SR_E0_d11", "SR_E0_d12", "SR_E0_d13", "SR_E0_d14", "SR_E0_d15", "SR_E0_d16", "SR_E0_d17", "SR_E0_d18", "SR_E0_d19", 
  "SR_E0_d20", "SR_E0_d21", "SR_E0_d22", "SR_E0_d23", "SR_E0_d24", "SR_E0_d25", "SR_E0_d26", 
  "SR_E1_d4", "SR_E1_d5", "SR_E1_d6", "SR_E1_d7", "SR_E1_d8", "SR_E1_d9", "SR_E1_d10", "SR_E1_d11", "SR_E1_d12", "SR_E1_d13", 
  "SR_E1_d14", "SR_E1_d15", "SR_E1_d16", "SR_E1_d17", "SR_E1_d18", "SR_E1_d19", "SR_E1_d20", "SR_E1_d21", "SR_E1_d22", 
  "SR_E1_d23", "SR_E1_d24", "SR_E1_d25", "SR_E1_d26", 
  "SR_E2_d8", "SR_E2_d9", "SR_E2_d10", "SR_E2_d11", "SR_E2_d12", "SR_E2_d13", "SR_E2_d14", "SR_E2_d15", "SR_E2_d16", 
  "SR_E2_d17", "SR_E2_d18", "SR_E2_d19", "SR_E2_d20", "SR_E2_d21", "SR_E2_d22", "SR_E2_d23", "SR_E2_d24", "SR_E2_d25", 
  "SR_E2_d26", 
                    
  ## RR
  "RR_E0_d1", "RR_E0_d2", "RR_E0_d3", "RR_E0_d4", "RR_E0_d5", "RR_E0_d6", "RR_E0_d7", "RR_E0_d8", "RR_E0_d9", "RR_E0_d10", 
  "RR_E0_d11", "RR_E0_d12", "RR_E0_d13", "RR_E0_d14", "RR_E0_d15", "RR_E0_d16", "RR_E0_d17", "RR_E0_d18", "RR_E0_d19", 
  "RR_E0_d20", "RR_E0_d21", "RR_E0_d22", "RR_E0_d23", "RR_E0_d24", "RR_E0_d25", "RR_E0_d26", 
  "RR_E1_d4", "RR_E1_d5", "RR_E1_d6", "RR_E1_d7", "RR_E1_d8", "RR_E1_d9", "RR_E1_d10", "RR_E1_d11", "RR_E1_d12", "RR_E1_d13", 
  "RR_E1_d14", "RR_E1_d15", "RR_E1_d16", "RR_E1_d17", "RR_E1_d18", "RR_E1_d19", "RR_E1_d20", "RR_E1_d21", "RR_E1_d22", 
  "RR_E1_d23", "RR_E1_d24", "RR_E1_d25", "RR_E1_d26",
  "RR_E2_d8", "RR_E2_d9", "RR_E2_d10", "RR_E2_d11", "RR_E2_d12", "RR_E2_d13", "RR_E2_d14", "RR_E2_d15", "RR_E2_d16", 
  "RR_E2_d17", "RR_E2_d18", "RR_E2_d19", "RR_E2_d20", "RR_E2_d21", "RR_E2_d22", "RR_E2_d23", "RR_E2_d24", "RR_E2_d25", 
  "RR_E2_d26")

population.type[["larval"]] <- c("SS_larvae_d1", "SS_larvae_d2", "SS_larvae_d3", "SS_larvae_d4", "SS_larvae_d5", "SS_larvae_d6", "SS_larvae_d7", "SS_larvae_d8", 
                                 "SR_larvae_d1", "SR_larvae_d2", "SR_larvae_d3", "SR_larvae_d4", "SR_larvae_d5", "SR_larvae_d6", "SR_larvae_d7", "SR_larvae_d8", 
                                 "RR_larvae_d1", "RR_larvae_d2", "RR_larvae_d3", "RR_larvae_d4", "RR_larvae_d5", "RR_larvae_d6", "RR_larvae_d7", "RR_larvae_d8")

sampling.mortality <- matrix(data=NA, nrow=4, ncol=(length(population.type[["adult"]])+length(population.type[["larval"]])))
colnames(sampling.mortality) <- c(population.type[["adult"]], population.type[["larval"]])
rownames(sampling.mortality) <- c("age.based.mortality", "LLIN.mortality", "assay.survival", "age.effects.only.survival")
  
sampling.mortality[1,] <- c(mu_SS_E0[1:26], mu_SS_E0[4:26], mu_SS_E0[8:26], mu_SR_E0[1:26], mu_SR_E1[4:26], 
                          mu_SR_E2[8:26], mu_RR_E0[1:26], mu_RR_E1[4:26], mu_RR_E2[8:26], rep(mu_SS_E0[4], 8), 
                          rep(mu_SR_E0[4], 8), rep(mu_RR_E0[4], 8))

sampling.mortality[2,] <- c(rep(mu_SS_ITN[1],26), rep(mu_SS_ITN[2],23), rep(mu_SS_ITN[3],19), 
                      rep(mu_SR_ITN[1],26), rep(mu_SR_ITN[2],23), rep(mu_SR_ITN[3],19),
                      rep(mu_RR_ITN[1],26), rep(mu_RR_ITN[2],23), rep(mu_RR_ITN[3],19), 
                      rep(mu_SS_ITN[1], 8), rep(mu_SR_ITN[1], 8), rep(mu_RR_ITN[1], 8))

sampling.mortality[3,] <- (1-sampling.mortality[1,])*(1-sampling.mortality[2,])

sampling.mortality[4,] <- c(rep(mu_SS_ITN[1],(26+23+19)),
                            rep(mu_SR_ITN[1],(26+23+19)),
                            rep(mu_RR_ITN[1],(26+23+19)), rep(NA, 24))

agebased_ageonly <- c(mu_SS_E0[1:26], mu_SS_E0[4:26], mu_SS_E0[8:26], mu_SR_E0[1:26], mu_SR_E0[4:26], 
                mu_SR_E0[8:26], mu_RR_E0[1:26], mu_RR_E0[4:26], mu_RR_E0[8:26], rep(mu_SS_E0[4], 8), 
                rep(mu_SR_E0[4], 8), rep(mu_RR_E0[4], 8))

sampling.mortality[4,] <- (1-agebased_ageonly)*(1-sampling.mortality[4,])


construct.pop <- function(compartments){
  pop <- c()
  for(x in 1:length(compartments)){
    pop <- c(pop, rep(names(compartments)[x], as.integer(compartments[x])))
  }
  return(pop)
}

resistance.assay <- function(sample.type, ## i.e. adult or larval
                             Coverage,    ## insecticde coverage (not p)
                             n,     ## number of mosquitoes per sample
                             N,     ## number of times sampled 
                             df){   
  
  compartments <- population.type[[sample.type]]
  observed.survival <- data.frame(NULL)

  df %>% 
    filter(coverage==Coverage) -> df
  
  for(j in 1:nrow(df)){
    
    sampling.population <- c(df[j, compartments])
    full.pop <- construct.pop(sampling.population)
    
    r <- as.numeric((df[j, "RR_POP"]+0.5*df[j, "SR_POP"])/df[j, "POP"])
    
    for (i in 1:N){
      
      individual.sample <- sample(full.pop, n, replace = FALSE, prob=NULL)    ## this is where we actually sample from the sampling bin
      total.survivors <- 0
      
      for(x in 1:length(individual.sample)) {
        observation <- rbinom(1, 1, sampling.mortality[3, individual.sample[x]])        ## this is where we expose each individual mosquito to see if they survive
        total.survivors <- total.survivors + observation
      }
      
      observed.survival <- rbind(observed.survival, total.survivors/n)
      
    }
  }
  
  colnames(observed.survival) = "survival"
  
  observed.survival %>%
    mutate(sample=sample.type, 
           coverage = Coverage, 
           spatial_coefficient = s,
           test = "test", 
           resistance = r) -> observed.survival
  
  return(observed.survival)
}

control.assay <- function(sample.type, ## i.e. adult or larval
                          Coverage,    ## insecticde coverage (not p)
                          n,     ## number of mosquitoes per sample
                          N,     ## number of times sampled 
                          df){   
  
  compartments <- population.type[[sample.type]]
  observed.survival <- data.frame(data=NULL)
  
  df %>% 
    filter(coverage==Coverage) -> df

  for(j in 1:nrow(df)){
    
    sampling.population <- c(df[j, compartments])
    full.pop <- construct.pop(sampling.population)
    
    r <- as.numeric((df[j, "RR_POP"]+0.5*df[j, "SR_POP"])/df[j, "POP"])
    
    for (i in 1:N){
      individual.sample <- sample(full.pop, n, replace = FALSE, prob=NULL)    ## this is where we actually sample from the sampling bin
      total.survivors <- 0
      
      for(x in 1:length(individual.sample)) {
        observation <- rbinom(1, 1, (1-sampling.mortality[1, individual.sample[x]]))        ## this is where we expose each individual mosquito to see if they survive
        total.survivors <- total.survivors + observation
      }
      
      observed.survival <- rbind(observed.survival, total.survivors/n)
      
    }
  }
  
  colnames(observed.survival) = "survival"
  
  observed.survival %>%
    mutate(sample=sample.type, 
           coverage = Coverage, 
           spatial_coefficient = s,
           test = "control", 
           resistance = r) -> observed.survival
  
  return(observed.survival)
}


## Abbott's control adjustment
control.adjustment <- function(test_survival, control_survival){
  
  adjusted_survival <- 100* (1 - 100*((100-test_survival)/100 - (100-control_survival)/100)/(100-control_survival/100))
  
  return(adjusted_survival)
  
}


malaria.prevalence <- function(coverage, resistance, population.list, day){
  if(missing(day)){
    day <- cutoff.matrix[resistance*20, coverage*20]
  }
  if(!is.na(day)){
    prevalence <- c(population.list[[coverage*20]][day, "I_H"])   ## sampling around the true "sampling date" to smooth jitters
  }
  return(prevalence)
}


age.distribution <- function(coverage, R.freq, population.list, cutoff.matrix, day){

  if(missing(day)){
    day <- cutoff.matrix[R.freq*20, coverage*20]
  }

  compartments <- population.type[["adult"]]
  age.dist <- as.data.frame(matrix(data=NA, nrow=length(compartments), ncol=3, dimnames=list(c(), c("age", "genotype", "quantity"))))
  age.dist[,1] <- c(rep(c(1:26, 4:26, 8:26),3))
  age.dist[,2] <- c(rep("SS",68), rep("SR",68), rep("RR",68))

  for(i in seq_along(compartments)){
    age.dist[i,3] <- population.list[[coverage*20]][day, compartments[i]]
  }

  return(age.dist)
}

