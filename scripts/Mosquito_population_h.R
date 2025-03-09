library(ggplot2)
library(tidyverse)

setwd()

args=(commandArgs(TRUE))

for (i in 1:length(args)) {
  eval (parse (text = args[[i]] ))
}

## set run time
days <- 10000

mortality <- read.csv("input/mortality.csv", header=TRUE, sep=',')

## set coverage
coverage_denom <- 20
LLIN_efficacy <- 0.8  ## assuming coverage efficacy is reduced by 20% due to damage/outdoor biting


## build storage matrix with a row for each genotype/age/exposure combo, column for each day
compartments <- c("day",

                      ## SS never exposed
                      "SS_eggs_d1", "SS_eggs_d2", "SS_eggs_d3", "SS_larvae_d1", "SS_larvae_d2", "SS_larvae_d3", "SS_larvae_d4",
                      "SS_larvae_d5", "SS_larvae_d6", "SS_larvae_d7", "SS_larvae_d8", "SS_pupae_d1", "SS_pupae_d2", "SS_E0_d1",
                      "SS_E0_d2", "SS_E0_d3", "SS_E0_d4", "SS_E0_d5", "SS_E0_d6", "SS_E0_d7", "SS_E0_d8", "SS_E0_d9", "SS_E0_d10", "SS_E0_d11",
                      "SS_E0_d12", "SS_E0_d13", "SS_E0_d14", "SS_E0_d15", "SS_E0_d16", "SS_E0_d17", "SS_E0_d18", "SS_E0_d19", "SS_E0_d20",
                      "SS_E0_d21", "SS_E0_d22", "SS_E0_d23", "SS_E0_d24", "SS_E0_d25", "SS_E0_d26",

                      ## Once exposed
                      "SS_E1_d4", "SS_E1_d5", "SS_E1_d6", "SS_E1_d7", "SS_E1_d8", "SS_E1_d9", "SS_E1_d10", "SS_E1_d11", "SS_E1_d12", "SS_E1_d13",
                      "SS_E1_d14", "SS_E1_d15", "SS_E1_d16", "SS_E1_d17", "SS_E1_d18", "SS_E1_d19", "SS_E1_d20", "SS_E1_d21", "SS_E1_d22",
                      "SS_E1_d23", "SS_E1_d24", "SS_E1_d25", "SS_E1_d26",

                      # More than once exposed
                      "SS_E2_d8", "SS_E2_d9", "SS_E2_d10", "SS_E2_d11", "SS_E2_d12", "SS_E2_d13", "SS_E2_d14", "SS_E2_d15", "SS_E2_d16",
                      "SS_E2_d17", "SS_E2_d18", "SS_E2_d19", "SS_E2_d20", "SS_E2_d21", "SS_E2_d22", "SS_E2_d23", "SS_E2_d24", "SS_E2_d25",
                      "SS_E2_d26",

                      ## SR never exposed
                      "SR_eggs_d1", "SR_eggs_d2", "SR_eggs_d3", "SR_larvae_d1", "SR_larvae_d2", "SR_larvae_d3", "SR_larvae_d4",
                      "SR_larvae_d5", "SR_larvae_d6", "SR_larvae_d7", "SR_larvae_d8", "SR_pupae_d1", "SR_pupae_d2", "SR_E0_d1",
                      "SR_E0_d2", "SR_E0_d3", "SR_E0_d4", "SR_E0_d5", "SR_E0_d6", "SR_E0_d7", "SR_E0_d8", "SR_E0_d9", "SR_E0_d10", "SR_E0_d11",
                      "SR_E0_d12", "SR_E0_d13", "SR_E0_d14", "SR_E0_d15", "SR_E0_d16", "SR_E0_d17", "SR_E0_d18", "SR_E0_d19", "SR_E0_d20",
                      "SR_E0_d21", "SR_E0_d22", "SR_E0_d23", "SR_E0_d24", "SR_E0_d25", "SR_E0_d26",

                      ## Once exposed
                      "SR_E1_d4", "SR_E1_d5", "SR_E1_d6", "SR_E1_d7", "SR_E1_d8", "SR_E1_d9", "SR_E1_d10", "SR_E1_d11", "SR_E1_d12", "SR_E1_d13",
                      "SR_E1_d14", "SR_E1_d15", "SR_E1_d16", "SR_E1_d17", "SR_E1_d18", "SR_E1_d19", "SR_E1_d20", "SR_E1_d21", "SR_E1_d22",
                      "SR_E1_d23", "SR_E1_d24", "SR_E1_d25", "SR_E1_d26",

                      # More than once exposed
                      "SR_E2_d8", "SR_E2_d9", "SR_E2_d10", "SR_E2_d11", "SR_E2_d12", "SR_E2_d13", "SR_E2_d14", "SR_E2_d15", "SR_E2_d16",
                      "SR_E2_d17", "SR_E2_d18", "SR_E2_d19", "SR_E2_d20", "SR_E2_d21", "SR_E2_d22", "SR_E2_d23", "SR_E2_d24", "SR_E2_d25",
                      "SR_E2_d26",

                      ## RR never exposed
                      "RR_eggs_d1", "RR_eggs_d2", "RR_eggs_d3", "RR_larvae_d1", "RR_larvae_d2", "RR_larvae_d3", "RR_larvae_d4",
                      "RR_larvae_d5", "RR_larvae_d6", "RR_larvae_d7", "RR_larvae_d8", "RR_pupae_d1", "RR_pupae_d2", "RR_E0_d1",
                      "RR_E0_d2", "RR_E0_d3", "RR_E0_d4", "RR_E0_d5", "RR_E0_d6", "RR_E0_d7", "RR_E0_d8", "RR_E0_d9", "RR_E0_d10", "RR_E0_d11",
                      "RR_E0_d12", "RR_E0_d13", "RR_E0_d14", "RR_E0_d15", "RR_E0_d16", "RR_E0_d17", "RR_E0_d18", "RR_E0_d19", "RR_E0_d20",
                      "RR_E0_d21", "RR_E0_d22", "RR_E0_d23", "RR_E0_d24", "RR_E0_d25", "RR_E0_d26",

                      ## Once exposed
                      "RR_E1_d4", "RR_E1_d5", "RR_E1_d6", "RR_E1_d7", "RR_E1_d8", "RR_E1_d9", "RR_E1_d10", "RR_E1_d11", "RR_E1_d12", "RR_E1_d13",
                      "RR_E1_d14", "RR_E1_d15", "RR_E1_d16", "RR_E1_d17", "RR_E1_d18", "RR_E1_d19", "RR_E1_d20", "RR_E1_d21", "RR_E1_d22",
                      "RR_E1_d23", "RR_E1_d24", "RR_E1_d25", "RR_E1_d26",

                      # More than once exposed
                      "RR_E2_d8", "RR_E2_d9", "RR_E2_d10", "RR_E2_d11", "RR_E2_d12", "RR_E2_d13", "RR_E2_d14", "RR_E2_d15", "RR_E2_d16",
                      "RR_E2_d17", "RR_E2_d18", "RR_E2_d19", "RR_E2_d20", "RR_E2_d21", "RR_E2_d22", "RR_E2_d23", "RR_E2_d24", "RR_E2_d25",
                      "RR_E2_d26",

                      "infected_biting", "I_H", "BetaM", "BetaH",
                      "larval_pop", "SS_POP", "SR_POP", "RR_POP", "POP",
                      "E0", "E1", "E2", "pr_exposed", "n_exposed", "potentially_exposed")

pop.mx <- matrix(data=NA, nrow=days+50, ncol=length(compartments))
colnames(pop.mx) <- compartments
pop.mx[,"day"] <- seq(-49, days)

## SET PARAMETERS

for(h in c(0.25, 0.5, 0.75)){  # h is dominance of resistance
  
  # fitness parameters
  c <- 0       # fitness cost of homozygous resistance (note: ratio measure, not difference)
  
  # LLIN mortality
  mu_RR_ITN <- c(0.005, 0.219, 0.391)    ## high resistance (TIA) strain ITN mortality (Viana et al. 2017)
  #c(0.723, 0.5713, 0.7752)  ## med resistance (TOR) strain ITN mortality (Viana et al. 2017)
  #c(0.005, 0.005, 0.005)
  
  mu_SS_ITN <- c(1,1,1)
  mu_SR_ITN <- c((mu_SS_ITN[1]*(1-h) + mu_RR_ITN[1]*h),   ## weighted average
                 (mu_SS_ITN[2]*(1-h) + mu_RR_ITN[2]*h),
                 (mu_SS_ITN[3]*(1-h) + mu_RR_ITN[3]*h))
  
  # initial genotype proportions
  RR_alleles <- 0.01
  SS_alleles <- 1-RR_alleles
  
  RR_start <- RR_alleles**2
  SR_start <- 2*RR_alleles*SS_alleles
  SS_start <- SS_alleles**2
  
  a <-  #7.36*10^(-6)             #low transmission: prevalence = 0.05 without ITN
    1.65*10^(-5)    #medium transmission: prevalence = 0.45 without ITN
  #9.11*10^(-5)   #high transmission: prevalence = 0.85 without ITN
  
  ## pull daily mortality parameters from data
  mu_SS_eggs <- 0.2
  mu_SS_pupae <- 0.25
  mu_SS_E0 <- mortality[,2]
  SS_survival <- 1-mu_SS_E0
  mu_SS_E1 <- mu_SS_E0
  mu_SS_E2 <- mu_SS_E0
  
  mu_RR_eggs <- 0.2
  mu_RR_pupae <- 0.25
  mu_RR_E0 <- mortality[,3]
  RR_survival <- 1-mu_RR_E0
  mu_RR_E1 <- mortality[,4]
  mu_RR_E2 <- mortality[,4]
  
  mu_SR_eggs <- 0.2
  mu_SR_pupae <- 0.25
  mu_SR_E0 <- mu_SS_E0 + h*(mu_RR_E0 - mu_SS_E0)
  SR_survival <- 1-mu_SR_E0
  mu_SR_E1 <- mu_RR_E1
  mu_SR_E2 <- mu_RR_E2
  
  ## infection parameters
  c1 <-  0.05           #controls steepness of curve of BetaM
  c2 <-  0.1366         #controls maximum BetaM risk (set s.t. max risk, q = c_2^2/(c_1 + c_2) = 0.1)
  v <-  1/75            #human recovery rate (75 days)
  b <-  0.55            #probability of infection given bite from mosquito
  
  # lifecycle parameters
  SS_eggcount <- 60                     #eggs per SS
  SR_eggcount <- SS_eggcount*(1 - c*h)  #eggs per SR
  RR_eggcount <- SS_eggcount*(1 - c)    #eggs per RR
  
  k <- 5000000      #larval "intraspecific competition" constant s.t. population = 100000
  females <- 0.5    #proportion population female  -- could modify for gene drive
  
  for(cov in c(1:19)){
    p <- (cov/coverage_denom)*LLIN_efficacy
    q <- 1-p
    
    for (s in seq(0,1,by=0.1)){   ## s is "spatial clustering coefficient"
      
      pop_master <- NULL
      
      # set up compartments
      pop.mx[, 2:ncol(pop.mx)] <- NA  ## matrix is reset to NA, then unexposed are overwritten below as 0
      pop.mx[1:50, 2:ncol(pop.mx)] <- 0
      
      eq_pop <- 100000  #equilibrium population
      mu_ratio <- 0
      for(j in 1:26){
        mu_ratio = mu_ratio + prod(SS_survival[1:(j-1)])
      }
      
      pop.mx[,"SS_eggs_d1"] <- c(rep(SS_start*eq_pop/mu_ratio/(((1-mu_SS_pupae)**10)*(1-mu_SS_eggs)**3),50), rep(NA, days))
      pop.mx[,"SS_eggs_d2"] <- c(rep(SS_start*eq_pop/mu_ratio/(((1-mu_SS_pupae)**10)*(1-mu_SS_eggs)**2),50), rep(NA, days))
      pop.mx[,"SS_eggs_d3"] <- c(rep(SS_start*eq_pop/mu_ratio/(((1-mu_SS_pupae)**10)*(1-mu_SS_eggs)),50), rep(NA, days))
      pop.mx[,"SS_larvae_d1"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**10,50), rep(NA, days))
      pop.mx[,"SS_larvae_d2"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**9,50), rep(NA, days))
      pop.mx[,"SS_larvae_d3"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**8,50), rep(NA, days))
      pop.mx[,"SS_larvae_d4"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**7,50), rep(NA, days))
      pop.mx[,"SS_larvae_d5"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**6,50), rep(NA, days))
      pop.mx[,"SS_larvae_d6"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**5,50), rep(NA, days))
      pop.mx[,"SS_larvae_d7"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**4,50), rep(NA, days))
      pop.mx[,"SS_larvae_d8"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**3,50), rep(NA, days))
      pop.mx[,"SS_pupae_d1"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae)**2,50), rep(NA, days))
      pop.mx[,"SS_pupae_d2"] <- c(rep(SS_start*eq_pop/mu_ratio/(1-mu_SS_pupae),50), rep(NA, days))
      pop.mx[,"SS_E0_d1"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:0]),50), rep(NA, days))       #1st adult day: rest
      pop.mx[,"SS_E0_d2"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:1]),50), rep(NA, days))       #2nd adult day: mate
      pop.mx[,"SS_E0_d3"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:2]),50), rep(NA, days))       #3rd adult day: first feed
      pop.mx[,"SS_E0_d4"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:3]),50), rep(NA, days))       #4th adult day: rest
      pop.mx[,"SS_E0_d5"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:4]),50), rep(NA, days))       #5th adult day: rest
      pop.mx[,"SS_E0_d6"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:5]),50), rep(NA, days))       #6th adult day: lay
      pop.mx[,"SS_E0_d7"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:6]),50), rep(NA, days))       #7th adult day: second feed
      pop.mx[,"SS_E0_d8"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:7]),50), rep(NA, days))       #8th adult day: rest
      pop.mx[,"SS_E0_d9"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:8]),50), rep(NA, days))       #9th adult day: rest
      pop.mx[,"SS_E0_d10"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:9]),50), rep(NA, days))      #lay
      pop.mx[,"SS_E0_d11"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:10]),50), rep(NA, days))      #third feed
      pop.mx[,"SS_E0_d12"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:11]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d13"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:12]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d14"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:13]),50), rep(NA, days))      #lay
      pop.mx[,"SS_E0_d15"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:14]),50), rep(NA, days))      #fourth feed
      pop.mx[,"SS_E0_d16"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:15]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d17"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:16]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d18"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:17]),50), rep(NA, days))      #lay
      pop.mx[,"SS_E0_d19"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:18]),50), rep(NA, days))      #fifth feed
      pop.mx[,"SS_E0_d20"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:19]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d21"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:20]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d22"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:21]),50), rep(NA, days))      #lay
      pop.mx[,"SS_E0_d23"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:22]),50), rep(NA, days))      #sixth feed
      pop.mx[,"SS_E0_d24"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:23]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d25"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:24]),50), rep(NA, days))      #rest
      pop.mx[,"SS_E0_d26"] <- c(rep(SS_start*eq_pop/mu_ratio*prod(SS_survival[1:25]),50), rep(NA, days))      #lay
      
      
      ## Initial compartments: SR (partially resistant) genotype, never exposed
      pop.mx[,"SR_eggs_d1"] <- c(rep(SR_start*eq_pop/mu_ratio/(((1-mu_SR_pupae)**10)*(1-mu_SR_eggs)**3),50), rep(NA, days))
      pop.mx[,"SR_eggs_d2"] <- c(rep(SR_start*eq_pop/mu_ratio/(((1-mu_SR_pupae)**10)*(1-mu_SR_eggs)**2),50), rep(NA, days))
      pop.mx[,"SR_eggs_d3"] <- c(rep(SR_start*eq_pop/mu_ratio/(((1-mu_SR_pupae)**10)*(1-mu_SR_eggs)),50), rep(NA, days))
      pop.mx[,"SR_larvae_d1"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**10,50), rep(NA, days))
      pop.mx[,"SR_larvae_d2"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**9,50), rep(NA, days))
      pop.mx[,"SR_larvae_d3"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**8,50), rep(NA, days))
      pop.mx[,"SR_larvae_d4"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**7,50), rep(NA, days))
      pop.mx[,"SR_larvae_d5"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**6,50), rep(NA, days))
      pop.mx[,"SR_larvae_d6"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**5,50), rep(NA, days))
      pop.mx[,"SR_larvae_d7"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**4,50), rep(NA, days))
      pop.mx[,"SR_larvae_d8"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**3,50), rep(NA, days))
      pop.mx[,"SR_pupae_d1"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae)**2,50), rep(NA, days))
      pop.mx[,"SR_pupae_d2"] <- c(rep(SR_start*eq_pop/mu_ratio/(1-mu_SR_pupae),50), rep(NA, days))
      pop.mx[,"SR_E0_d1"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:0]),50), rep(NA, days))       #1st adult day: rest
      pop.mx[,"SR_E0_d2"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:1]),50), rep(NA, days))       #2nd adult day: mate
      pop.mx[,"SR_E0_d3"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:2]),50), rep(NA, days))       #3rd adult day: first feed
      pop.mx[,"SR_E0_d4"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:3]),50), rep(NA, days))       #4th adult day: rest
      pop.mx[,"SR_E0_d5"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:4]),50), rep(NA, days))       #5th adult day: rest
      pop.mx[,"SR_E0_d6"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:5]),50), rep(NA, days))       #6th adult day: lay
      pop.mx[,"SR_E0_d7"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:6]),50), rep(NA, days))       #7th adult day: second feed
      pop.mx[,"SR_E0_d8"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:7]),50), rep(NA, days))       #8th adult day: rest
      pop.mx[,"SR_E0_d9"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:8]),50), rep(NA, days))       #9th adult day: rest
      pop.mx[,"SR_E0_d10"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:9]),50), rep(NA, days))      #lay
      pop.mx[,"SR_E0_d11"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:10]),50), rep(NA, days))      #third feed
      pop.mx[,"SR_E0_d12"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:11]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d13"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:12]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d14"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:13]),50), rep(NA, days))      #lay
      pop.mx[,"SR_E0_d15"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:14]),50), rep(NA, days))      #fourth feed (first infectious feed)
      pop.mx[,"SR_E0_d16"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:15]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d17"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:16]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d18"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:17]),50), rep(NA, days))      #lay
      pop.mx[,"SR_E0_d19"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:18]),50), rep(NA, days))      #fifth feed
      pop.mx[,"SR_E0_d20"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:19]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d21"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:20]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d22"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:21]),50), rep(NA, days))      #lay
      pop.mx[,"SR_E0_d23"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:22]),50), rep(NA, days))      #sixth feed
      pop.mx[,"SR_E0_d24"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:23]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d25"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:24]),50), rep(NA, days))      #rest
      pop.mx[,"SR_E0_d26"] <- c(rep(SR_start*eq_pop/mu_ratio*prod(SR_survival[1:25]),50), rep(NA, days))      #lay
      
      
      ## Initial compartments: RR (resistant) genotype, never exposed
      pop.mx[,"RR_eggs_d1"] <- c(rep(RR_start*eq_pop/mu_ratio/(((1-mu_RR_pupae)**10)*(1-mu_RR_eggs)**3),50), rep(NA, days))
      pop.mx[,"RR_eggs_d2"] <- c(rep(RR_start*eq_pop/mu_ratio/(((1-mu_RR_pupae)**10)*(1-mu_RR_eggs)**2),50), rep(NA, days))
      pop.mx[,"RR_eggs_d3"] <- c(rep(RR_start*eq_pop/mu_ratio/(((1-mu_RR_pupae)**10)*(1-mu_RR_eggs)),50), rep(NA, days))
      pop.mx[,"RR_larvae_d1"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**10,50), rep(NA, days))
      pop.mx[,"RR_larvae_d2"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**9,50), rep(NA, days))
      pop.mx[,"RR_larvae_d3"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**8,50), rep(NA, days))
      pop.mx[,"RR_larvae_d4"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**7,50), rep(NA, days))
      pop.mx[,"RR_larvae_d5"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**6,50), rep(NA, days))
      pop.mx[,"RR_larvae_d6"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**5,50), rep(NA, days))
      pop.mx[,"RR_larvae_d7"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**4,50), rep(NA, days))
      pop.mx[,"RR_larvae_d8"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**3,50), rep(NA, days))
      pop.mx[,"RR_pupae_d1"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae)**2,50), rep(NA, days))
      pop.mx[,"RR_pupae_d1"] <- c(rep(RR_start*eq_pop/mu_ratio/(1-mu_RR_pupae),50), rep(NA, days))
      pop.mx[,"RR_E0_d1"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:0]),50), rep(NA, days))       #1st adult day: rest
      pop.mx[,"RR_E0_d2"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:1]),50), rep(NA, days))       #2nd adult day: mate
      pop.mx[,"RR_E0_d3"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:2]),50), rep(NA, days))       #3rd adult day: first feed
      pop.mx[,"RR_E0_d4"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:3]),50), rep(NA, days))       #4th adult day: rest
      pop.mx[,"RR_E0_d5"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:4]),50), rep(NA, days))       #5th adult day: rest
      pop.mx[,"RR_E0_d6"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:5]),50), rep(NA, days))       #6th adult day: lay
      pop.mx[,"RR_E0_d7"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:6]),50), rep(NA, days))       #7th adult day: second feed
      pop.mx[,"RR_E0_d8"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:7]),50), rep(NA, days))       #8th adult day: rest
      pop.mx[,"RR_E0_d9"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:8]),50), rep(NA, days))       #9th adult day: rest
      pop.mx[,"RR_E0_d10"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:9]),50), rep(NA, days))      #lay
      pop.mx[,"RR_E0_d11"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:10]),50), rep(NA, days))      #third feed
      pop.mx[,"RR_E0_d12"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:11]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d13"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:12]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d14"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:13]),50), rep(NA, days))      #lay
      pop.mx[,"RR_E0_d15"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:14]),50), rep(NA, days))      #fourth feed
      pop.mx[,"RR_E0_d16"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:15]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d17"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:16]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d18"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:17]),50), rep(NA, days))      #lay
      pop.mx[,"RR_E0_d19"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:18]),50), rep(NA, days))      #fifth feed
      pop.mx[,"RR_E0_d20"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:19]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d21"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:20]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d22"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:21]),50), rep(NA, days))      #lay
      pop.mx[,"RR_E0_d23"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:22]),50), rep(NA, days))      #sixth feed
      pop.mx[,"RR_E0_d24"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:23]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d25"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:24]),50), rep(NA, days))      #rest
      pop.mx[,"RR_E0_d26"] <- c(rep(RR_start*eq_pop/mu_ratio*prod(RR_survival[1:25]),50), rep(NA, days))      #lay
      
      #  infection model
      pop.mx[,"infected_biting"] <- c(rep(0,50), rep(NA, days))
      pop.mx[,"I_H"] <- c(rep(0.45, 50), rep(NA, days))             #proportion of human population infected
      pop.mx[,"BetaM"] <- c(rep(0.001, 50), rep(NA, days))
      pop.mx[,"BetaH"] <- c(rep(0.00001, 50), rep(NA, days))
      
      for (t in 51:(days+50)){
        
        n_exposed <- 0
        potentially_exposed <- 0
        
        # total larval and by-genotype populations
        pop.mx[,"larval_pop"] <- rowSums(pop.mx[,c(5:12, 86:93, 167:174)])
        pop.mx[,"SS_POP"] <- rowSums(pop.mx[,c(15:82)])
        pop.mx[,"SR_POP"] <- rowSums(pop.mx[,c(96:163)])
        pop.mx[,"RR_POP"] <- rowSums(pop.mx[,c(177:244)])
        pop.mx[,"POP"] <- rowSums(pop.mx[,c(250:252)])
        
        pop.mx[,"E0"]<- rowSums(pop.mx[,c(15:40, 96:121, 177:202)])
        pop.mx[,"E1"] <- rowSums(pop.mx[,c(41:63, 122:144, 203:225)])
        pop.mx[,"E2"] <- rowSums(pop.mx[,c(64:82, 145:163, 226:244)])
        
        # SS compartments -- UNEXPOSED   (egg counts calculated under HWE)
        pop.mx[,"SS_eggs_d1"][t] <- (  (pop.mx[,"SS_E0_d6"][t-1] + pop.mx[,"SS_E1_d6"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-5] + (1/2)*pop.mx[,"SR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                       + (pop.mx[,"SS_E0_d10"][t-1] + pop.mx[,"SS_E1_d10"][t-1] + pop.mx[,"SS_E2_d10"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-9] + (1/2)*pop.mx[,"SR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                       + (pop.mx[,"SS_E0_d14"][t-1] + pop.mx[,"SS_E1_d14"][t-1] + pop.mx[,"SS_E2_d14"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-13]+ (1/2)*pop.mx[,"SR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                       + (pop.mx[,"SS_E0_d18"][t-1] + pop.mx[,"SS_E1_d18"][t-1] + pop.mx[,"SS_E2_d18"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-17]+ (1/2)*pop.mx[,"SR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                       + (pop.mx[,"SS_E0_d22"][t-1] + pop.mx[,"SS_E1_d22"][t-1] + pop.mx[,"SS_E2_d22"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-21] + (1/2)*pop.mx[,"SR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                       + (pop.mx[,"SS_E0_d26"][t-1] + pop.mx[,"SS_E1_d26"][t-1] + pop.mx[,"SS_E2_d26"][t-1]) * SS_eggcount * (pop.mx[,"SS_POP"][t-25] + (1/2)*pop.mx[,"SR_POP"][t-25])/pop.mx[,"POP"][t-25]
                                       + (pop.mx[,"SR_E0_d6"][t-1] + pop.mx[,"SR_E1_d6"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-5] + (1/4)*pop.mx[,"SR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                       + (pop.mx[,"SR_E0_d10"][t-1] + pop.mx[,"SR_E1_d10"][t-1] + pop.mx[,"SR_E2_d10"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-9] + (1/4)*pop.mx[,"SR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                       + (pop.mx[,"SR_E0_d14"][t-1] + pop.mx[,"SR_E1_d14"][t-1] + pop.mx[,"SR_E2_d14"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-13] + (1/4)*pop.mx[,"SR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                       + (pop.mx[,"SR_E0_d18"][t-1] + pop.mx[,"SR_E1_d18"][t-1] + pop.mx[,"SR_E2_d18"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-17] + (1/4)*pop.mx[,"SR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                       + (pop.mx[,"SR_E0_d22"][t-1] + pop.mx[,"SR_E1_d22"][t-1] + pop.mx[,"SR_E2_d22"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-21] + (1/4)*pop.mx[,"SR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                       + (pop.mx[,"SR_E0_d26"][t-1] + pop.mx[,"SR_E1_d26"][t-1] + pop.mx[,"SR_E2_d26"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-25] + (1/4)*pop.mx[,"SR_POP"][t-25])/pop.mx[,"POP"][t-25])
        
        pop.mx[,"SS_eggs_d2"][t] <- (1-mu_SS_eggs) * pop.mx[,"SS_eggs_d1"][t-1]
        pop.mx[,"SS_eggs_d3"][t] <- (1-mu_SS_eggs) * pop.mx[,"SS_eggs_d2"][t-1]
        pop.mx[,"SS_larvae_d1"][t] <- (1-mu_SS_eggs) * pop.mx[,"SS_eggs_d3"][t-1]
        pop.mx[,"SS_larvae_d2"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d1"][t-1]
        pop.mx[,"SS_larvae_d3"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d2"][t-1]
        pop.mx[,"SS_larvae_d4"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d3"][t-1]
        pop.mx[,"SS_larvae_d5"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d4"][t-1]
        pop.mx[,"SS_larvae_d6"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d5"][t-1]
        pop.mx[,"SS_larvae_d7"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d6"][t-1]
        pop.mx[,"SS_larvae_d8"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d7"][t-1]
        pop.mx[,"SS_pupae_d1"][t] <-  exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SS_larvae_d8"][t-1]
        pop.mx[,"SS_pupae_d2"][t] <- (1-mu_SS_pupae) * pop.mx[,"SS_pupae_d1"][t-1]
        pop.mx[,"SS_E0_d1"][t] <- (1-mu_SS_pupae) * pop.mx[,"SS_pupae_d2"][t-1] * females      #rest
        pop.mx[,"SS_E0_d2"][t] <- (1-mu_SS_E0[1]) * pop.mx[,"SS_E0_d1"][t-1]                   #mate
        pop.mx[,"SS_E0_d3"][t] <- (1-mu_SS_E0[2]) * pop.mx[,"SS_E0_d2"][t-1]                   #first feed
        
        pop.mx[,"SS_E0_d4"][t] <- (1-mu_SS_E0[3]) * q * pop.mx[,"SS_E0_d3"][t-1]               #rest (move to E1)
        pop.mx[,"SS_E0_d5"][t] <- (1-mu_SS_E0[4]) * pop.mx[,"SS_E0_d4"][t-1]                   #rest
        pop.mx[,"SS_E0_d6"][t] <- (1-mu_SS_E0[5]) * pop.mx[,"SS_E0_d5"][t-1]                   #lay (contribute to eggs)
        pop.mx[,"SS_E0_d7"][t] <- (1-mu_SS_E0[6]) * pop.mx[,"SS_E0_d6"][t-1]                   #second feed
        
        p1a = p-s*p
        q1a = q+s*p
        
        pop.mx[,"SS_E0_d8"][t] <- (1-mu_SS_E0[7]) * q1a * pop.mx[,"SS_E0_d7"][t-1]    #rest (move to E1)
        pop.mx[,"SS_E0_d9"][t] <- (1-mu_SS_E0[8]) * pop.mx[,"SS_E0_d8"][t-1]                   #rest
        pop.mx[,"SS_E0_d10"][t] <- (1-mu_SS_E0[9]) * pop.mx[,"SS_E0_d9"][t-1]                  #lay (contribute to eggs)
        pop.mx[,"SS_E0_d11"][t] <- (1-mu_SS_E0[10]) * pop.mx[,"SS_E0_d10"][t-1]                #third feed
        
        p2a = p1a - s*p1a
        q2a = q1a + s*p1a
        
        pop.mx[,"SS_E0_d12"][t] <- (1-mu_SS_E0[11]) * q2a * pop.mx[,"SS_E0_d11"][t-1] #rest (move to E1)
        pop.mx[,"SS_E0_d13"][t] <- (1-mu_SS_E0[12]) * pop.mx[,"SS_E0_d12"][t-1]                #rest
        pop.mx[,"SS_E0_d14"][t] <- (1-mu_SS_E0[13]) * pop.mx[,"SS_E0_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E0_d15"][t] <- (1-mu_SS_E0[14]) * pop.mx[,"SS_E0_d14"][t-1]                #fourth feed ----------------- (count for transmission)
        
        p3a = p2a - s*p2a
        q3a = q2a + s*p2a
        
        pop.mx[,"SS_E0_d16"][t] <- (1-mu_SS_E0[15]) * q3a * pop.mx[,"SS_E0_d15"][t-1] #rest (move to E1)
        pop.mx[,"SS_E0_d17"][t] <- (1-mu_SS_E0[16]) * pop.mx[,"SS_E0_d16"][t-1]                #rest
        pop.mx[,"SS_E0_d18"][t] <- (1-mu_SS_E0[17]) * pop.mx[,"SS_E0_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E0_d19"][t] <- (1-mu_SS_E0[18]) * pop.mx[,"SS_E0_d18"][t-1]                #fifth feed ------------------ (count for transmission)
        
        p4a = p3a - s*p3a
        q4a = q3a + s*p3a
        
        pop.mx[,"SS_E0_d20"][t] <- (1-mu_SS_E0[19]) * q4a * pop.mx[,"SS_E0_d19"][t-1] #rest (move to E1)
        pop.mx[,"SS_E0_d21"][t] <- (1-mu_SS_E0[20]) * pop.mx[,"SS_E0_d20"][t-1]                 #rest
        pop.mx[,"SS_E0_d22"][t] <- (1-mu_SS_E0[21]) * pop.mx[,"SS_E0_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E0_d23"][t] <- (1-mu_SS_E0[22]) * pop.mx[,"SS_E0_d22"][t-1]                #sixth feed ------------------ (count for transmission)
        
        p5a = p4a - s*p4a
        q5a = q4a + s*p4a
        
        pop.mx[,"SS_E0_d24"][t] <- (1-mu_SS_E0[23]) * q5a * pop.mx[,"SS_E0_d23"][t-1] #rest (move to E1)
        pop.mx[,"SS_E0_d25"][t] <- (1-mu_SS_E0[24]) * pop.mx[,"SS_E0_d24"][t-1]                #rest
        pop.mx[,"SS_E0_d26"][t] <- (1-mu_SS_E0[25]) * pop.mx[,"SS_E0_d25"][t-1]                #lay (contribute to eggs)
        
        
        ## SINGLE EXPOSURES -- only possible beginning after day 3 of adulthood (first feeding day)
        
        n_exposed =  p * pop.mx[,"SS_E0_d3"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d3"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d4"][t] <- (1-mu_SS_E0[3]) * p * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d3"][t-1]   #rest
        pop.mx[,"SS_E1_d5"][t] <- (1-mu_SS_E1[4]) * pop.mx[,"SS_E1_d4"][t-1]                                 #rest
        pop.mx[,"SS_E1_d6"][t] <- (1-mu_SS_E1[5]) * pop.mx[,"SS_E1_d5"][t-1]                                 #lay (contribute to eggs)
        pop.mx[,"SS_E1_d7"][t] <- (1-mu_SS_E1[6]) * pop.mx[,"SS_E1_d6"][t-1]                                 #second feed
        
        p1b = p + s*q   ## exposed
        q1b = q - s*q   ## not exposed
        
        n_exposed =  p1a * pop.mx[,"SS_E0_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d7"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d8"][t] <- ((1-mu_SS_E1[7]) * q1b * pop.mx[,"SS_E1_d7"][t-1]) + ((1-mu_SS_E0[7]) * p1a * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d7"][t-1])        #rest
        pop.mx[,"SS_E1_d9"][t] <- (1-mu_SS_E1[8]) * pop.mx[,"SS_E1_d8"][t-1]                                 #rest
        pop.mx[,"SS_E1_d10"][t] <- (1-mu_SS_E1[9]) * pop.mx[,"SS_E1_d9"][t-1]                                #lay (contribute to eggs)
        pop.mx[,"SS_E1_d11"][t] <- (1-mu_SS_E1[10]) * pop.mx[,"SS_E1_d10"][t-1]                              #third feed
        
        p2b = mean(p1a + s*q1a, p1b - s*p1b)    #if coming from a, then it was exposed (p+s*q)-- if coming from b, then not exposed (p-s*p)
        q2b = mean(q1a - s*q1a, q1b + s*p1b)                        #if coming from a, then it was exposed (p+s*q)-- if coming from b, then not exposed (p-s*p)
        
        n_exposed =  p2a * pop.mx[,"SS_E0_d11"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d11"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d12"][t] <- ((1-mu_SS_E1[11]) * q2b * pop.mx[,"SS_E1_d11"][t-1]) + ((1-mu_SS_E0[11]) * p2a * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d11"][t-1])   #rest
        pop.mx[,"SS_E1_d13"][t] <- (1-mu_SS_E1[12]) * pop.mx[,"SS_E1_d12"][t-1]                #rest
        pop.mx[,"SS_E1_d14"][t] <- (1-mu_SS_E1[13]) * pop.mx[,"SS_E1_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E1_d15"][t] <- (1-mu_SS_E1[14]) * pop.mx[,"SS_E1_d14"][t-1]                #fourth feed ----------------- (count for transmission)
        
        p3b = mean(p2a + s*q2a, p2b - s*p2b)
        q3b = mean(q2a - s*q2a, q2b + s*p2b)
        
        n_exposed =  p3a * pop.mx[,"SS_E0_d15"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d15"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d16"][t] <- ((1-mu_SS_E1[15]) * q3b * pop.mx[,"SS_E1_d15"][t-1]) + ((1-mu_SS_E0[15]) * p3a * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d15"][t-1])   #rest
        pop.mx[,"SS_E1_d17"][t] <- (1-mu_SS_E1[16]) * pop.mx[,"SS_E1_d16"][t-1]                #rest
        pop.mx[,"SS_E1_d18"][t] <- (1-mu_SS_E1[17]) * pop.mx[,"SS_E1_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E1_d19"][t] <- (1-mu_SS_E1[18]) * pop.mx[,"SS_E1_d18"][t-1]                #fifth feed ----------------- (count for transmission)
        
        p4b = mean(p3a + s*q3a, p3b - s*p3b)
        q4b = mean(q3a - s*q3a, q3b + s*p3b)
        
        n_exposed =  p4a * pop.mx[,"SS_E0_d19"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d19"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d20"][t] <- ((1-mu_SS_E1[19]) * q4b * pop.mx[,"SS_E1_d19"][t-1]) + ((1-mu_SS_E0[19]) * p4a * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d19"][t-1])   #rest
        pop.mx[,"SS_E1_d21"][t] <- (1-mu_SS_E1[20]) * pop.mx[,"SS_E1_d20"][t-1]                #rest
        pop.mx[,"SS_E1_d22"][t] <- (1-mu_SS_E1[21]) * pop.mx[,"SS_E1_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E1_d23"][t] <- (1-mu_SS_E1[22]) * pop.mx[,"SS_E1_d22"][t-1]                #sixth feed ----------------- (count for transmission)
        
        p5b = mean(p4a + s*q4a, p4b - s*p4b)
        q5b = mean(q4a - s*q4a, q4b + s*p4b)
        
        n_exposed =  p5a * pop.mx[,"SS_E0_d23"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E0_d23"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E1_d24"][t] <- ((1-mu_SS_E1[23]) * q5b * pop.mx[,"SS_E1_d23"][t-1]) + ((1-mu_SS_E0[23]) * p5a * (1-mu_SS_ITN[1]) * pop.mx[,"SS_E0_d23"][t-1])   #rest
        pop.mx[,"SS_E1_d25"][t] <- (1-mu_SS_E1[24]) * pop.mx[,"SS_E1_d24"][t-1]                #rest
        pop.mx[,"SS_E1_d26"][t] <- (1-mu_SS_E1[25]) * pop.mx[,"SS_E1_d25"][t-1]                #lay (contribute to eggs)
        
        ## MULTIPLE EXPOSURES -- only possible after 7th day of adulthood (second feeding day)
        
        n_exposed =  p1b * pop.mx[,"SS_E1_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SS_E1_d7"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E2_d8"][t] <- (1-mu_SS_E1[7]) * p1b * (1-mu_SS_ITN[2]) * pop.mx[,"SS_E1_d7"][t-1]          #rest
        pop.mx[,"SS_E2_d9"][t] <- (1-mu_SS_E2[8]) * pop.mx[,"SS_E2_d8"][t-1]                   #rest
        pop.mx[,"SS_E2_d10"][t] <- (1-mu_SS_E2[9]) * pop.mx[,"SS_E2_d9"][t-1]                  #lay (contribute to eggs)
        pop.mx[,"SS_E2_d11"][t] <- (1-mu_SS_E2[10]) * pop.mx[,"SS_E2_d10"][t-1]
        
        #third feed
        p2c = p1b + s*q1b
        q2c = q1b - s*q1b
        
        n_exposed =  (p2b * pop.mx[,"SS_E1_d11"][t-1] + p2c * pop.mx[,"SS_E2_d11"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SS_E1_d11"][t-1] + pop.mx[,"SS_E2_d11"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E2_d12"][t] <- (((1-mu_SS_E2[11]) * q2c * pop.mx[,"SS_E2_d11"][t-1])
                                    + ((1-mu_SS_E1[11]) * p2b * (1-mu_SS_ITN[2]) * pop.mx[,"SS_E1_d11"][t-1])
                                    + ((1-mu_SS_E2[11]) * p2c * (1-mu_SS_ITN[3]) * pop.mx[,"SS_E2_d11"][t-1]))    #rest
        pop.mx[,"SS_E2_d13"][t] <- (1-mu_SS_E2[12]) * pop.mx[,"SS_E2_d12"][t-1]                #rest
        pop.mx[,"SS_E2_d14"][t] <- (1-mu_SS_E2[13]) * pop.mx[,"SS_E2_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E2_d15"][t] <- (1-mu_SS_E2[14]) * pop.mx[,"SS_E2_d14"][t-1]
        
        #fourth feed ------------------------------------------------------------------------- (counts for transmission)
        p3c = mean(p2b + s*q2b,  ## exposed from M
                   p2c - s*p2c,  ## unexposed from E
                   p2c + s*q2c)  ## exposed from E
        
        q3c = mean(q2b - s*q2b,  ## exposed from M
                   q2c + s*p2c,  ## unexposed from E
                   q2c - s*q2c)  ## exposed from E
        
        
        n_exposed =  (p3b * pop.mx[,"SS_E1_d15"][t-1] + p3c * pop.mx[,"SS_E2_d15"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SS_E1_d15"][t-1] + pop.mx[,"SS_E2_d15"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E2_d16"][t] <- (((1-mu_SS_E2[15]) * q3c * pop.mx[,"SS_E2_d15"][t-1])
                                    + ((1-mu_SS_E1[15]) * p3b * (1-mu_SS_ITN[2]) * pop.mx[,"SS_E1_d15"][t-1])
                                    + ((1-mu_SS_E2[15]) * p3c * (1-mu_SS_ITN[3]) * pop.mx[,"SS_E2_d15"][t-1]))   #rest
        pop.mx[,"SS_E2_d17"][t] <- (1-mu_SS_E2[16]) * pop.mx[,"SS_E2_d16"][t-1]                #rest
        pop.mx[,"SS_E2_d18"][t] <- (1-mu_SS_E2[17]) * pop.mx[,"SS_E2_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E2_d19"][t] <- (1-mu_SS_E2[18]) * pop.mx[,"SS_E2_d18"][t-1]
        
        #fifth feed ------------------------------------------------------------------------ (count for transmission)
        p4c = mean(p3b + s*q3b,  ## exposed from M
                   p3c - s*p3c,  ## unexposed from E
                   p3c + s*q3c)  ## exposed from E
        
        q4c = mean(q3b - s*q3b,
                   q3c + s*p3c,
                   q3c - s*q3c)
        
        n_exposed =  (p4b * pop.mx[,"SS_E1_d19"][t-1] + p4c * pop.mx[,"SS_E2_d19"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SS_E1_d19"][t-1] + pop.mx[,"SS_E2_d19"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E2_d20"][t] <- (((1-mu_SS_E2[19]) * q4c * pop.mx[,"SS_E2_d19"][t-1])
                                    + ((1-mu_SS_E1[19]) * p4b * (1-mu_SS_ITN[2]) * pop.mx[,"SS_E1_d19"][t-1])
                                    + ((1-mu_SS_E2[19]) * p4c * (1-mu_SS_ITN[3]) * pop.mx[,"SS_E2_d19"][t-1]))   #rest
        pop.mx[,"SS_E2_d21"][t] <- (1-mu_SS_E2[20]) * pop.mx[,"SS_E2_d20"][t-1]                #rest
        pop.mx[,"SS_E2_d22"][t] <- (1-mu_SS_E2[21]) * pop.mx[,"SS_E2_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SS_E2_d23"][t] <- (1-mu_SS_E2[22]) * pop.mx[,"SS_E2_d22"][t-1]
        
        #sixth feed ------------------------------------------------------------------------- (count for transmission)
        p5c = mean(p4b + s*q4b,  ## exposed from M
                   p4c - s*p4c,  ## unexposed from E
                   p4c + s*q4c)  ## exposed from E
        
        q5c = mean(q4b - s*q4b,
                   q4c + s*p4c,
                   q4c - s*q4c)
        
        n_exposed =  (p5b * pop.mx[,"SS_E1_d23"][t-1] + p5c * pop.mx[,"SS_E2_d23"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SS_E1_d23"][t-1] + pop.mx[,"SS_E2_d23"][t-1] + potentially_exposed
        
        pop.mx[,"SS_E2_d24"][t] <- (((1-mu_SS_E2[23]) * q5c * pop.mx[,"SS_E2_d23"][t-1])
                                    + ((1-mu_SS_E1[23]) * p5b * (1-mu_SS_ITN[2]) * pop.mx[,"SS_E1_d23"][t-1])
                                    + ((1-mu_SS_E2[23]) * p5c * (1-mu_SS_ITN[3]) * pop.mx[,"SS_E2_d23"][t-1]))   #rest
        pop.mx[,"SS_E2_d25"][t] <- (1-mu_SS_E2[24]) * pop.mx[,"SS_E2_d24"][t-1]                #rest
        pop.mx[,"SS_E2_d26"][t] <- (1-mu_SS_E2[25]) * pop.mx[,"SS_E2_d25"][t-1]                #lay (contribute to eggs)
        
        
        
        ## SR Compartments
        ## UNEXPOSED
        pop.mx[,"SR_eggs_d1"][t] <- ((pop.mx[,"SS_E0_d6"][t-1] + pop.mx[,"SS_E1_d6"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-5] + pop.mx[,"RR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                     + (pop.mx[,"SS_E0_d10"][t-1] + pop.mx[,"SS_E1_d10"][t-1] + pop.mx[,"SS_E2_d10"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-9] + pop.mx[,"RR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                     + (pop.mx[,"SS_E0_d14"][t-1] + pop.mx[,"SS_E1_d14"][t-1] + pop.mx[,"SS_E2_d14"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-13] + pop.mx[,"RR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                     + (pop.mx[,"SS_E0_d18"][t-1] + pop.mx[,"SS_E1_d18"][t-1] + pop.mx[,"SS_E2_d18"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-17] + pop.mx[,"RR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                     + (pop.mx[,"SS_E0_d22"][t-1] + pop.mx[,"SS_E1_d22"][t-1] + pop.mx[,"SS_E2_d22"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-21] + pop.mx[,"RR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                     + (pop.mx[,"SS_E0_d26"][t-1] + pop.mx[,"SS_E1_d26"][t-1] + pop.mx[,"SS_E2_d26"][t-1]) * SS_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-25] + pop.mx[,"RR_POP"][t-25])/pop.mx[,"POP"][t-25]
                                     + (pop.mx[,"SR_E0_d6"][t-1] + pop.mx[,"SR_E1_d6"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-5] + (1/2)*pop.mx[,"SR_POP"][t-5] + (1/2)*pop.mx[,"RR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                     + (pop.mx[,"SR_E0_d10"][t-1] + pop.mx[,"SR_E1_d10"][t-1] + pop.mx[,"SR_E2_d10"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-9] + (1/2)*pop.mx[,"SR_POP"][t-9] + (1/2)*pop.mx[,"RR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                     + (pop.mx[,"SR_E0_d14"][t-1] + pop.mx[,"SR_E1_d14"][t-1] + pop.mx[,"SR_E2_d14"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-13] + (1/2)*pop.mx[,"SR_POP"][t-13] + (1/2)*pop.mx[,"RR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                     + (pop.mx[,"SR_E0_d18"][t-1] + pop.mx[,"SR_E1_d18"][t-1] + pop.mx[,"SR_E2_d18"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-17] + (1/2)*pop.mx[,"SR_POP"][t-17] + (1/2)*pop.mx[,"RR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                     + (pop.mx[,"SR_E0_d22"][t-1] + pop.mx[,"SR_E1_d22"][t-1] + pop.mx[,"SR_E2_d22"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-21] + (1/2)*pop.mx[,"SR_POP"][t-21] + (1/2)*pop.mx[,"RR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                     + (pop.mx[,"SR_E0_d26"][t-1] + pop.mx[,"SR_E1_d26"][t-1] + pop.mx[,"SR_E2_d26"][t-1]) * SR_eggcount * ((1/2)*pop.mx[,"SS_POP"][t-25] + (1/2)*pop.mx[,"SR_POP"][t-25] + (1/2)*pop.mx[,"RR_POP"][t-25])/pop.mx[,"POP"][t-25]
                                     + (pop.mx[,"RR_E0_d6"][t-1] + pop.mx[,"RR_E1_d6"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-5] + pop.mx[,"SS_POP"][t-5])/pop.mx[,"POP"][t-5]
                                     + (pop.mx[,"RR_E0_d10"][t-1] + pop.mx[,"RR_E1_d10"][t-1] + pop.mx[,"RR_E2_d10"][t-9]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-9] + pop.mx[,"SS_POP"][t-9])/pop.mx[,"POP"][t-9]
                                     + (pop.mx[,"RR_E0_d14"][t-1] + pop.mx[,"RR_E1_d14"][t-1] + pop.mx[,"RR_E2_d14"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-13] + pop.mx[,"SS_POP"][t-13])/pop.mx[,"POP"][t-13]
                                     + (pop.mx[,"RR_E0_d15"][t-1] + pop.mx[,"RR_E1_d18"][t-1] + pop.mx[,"RR_E2_d18"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-17] + pop.mx[,"SS_POP"][t-17])/pop.mx[,"POP"][t-17]
                                     + (pop.mx[,"RR_E0_d16"][t-1] + pop.mx[,"RR_E1_d22"][t-1] + pop.mx[,"RR_E2_d22"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-21] + pop.mx[,"SS_POP"][t-21])/pop.mx[,"POP"][t-21]
                                     + (pop.mx[,"RR_E0_d17"][t-1] + pop.mx[,"RR_E1_d26"][t-1] + pop.mx[,"RR_E2_d26"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-25] + pop.mx[,"SS_POP"][t-25])/pop.mx[,"POP"][t-25])
        
        pop.mx[,"SR_eggs_d2"][t] <- (1-mu_SR_eggs) * pop.mx[,"SR_eggs_d1"][t-1]
        pop.mx[,"SR_eggs_d3"][t] <- (1-mu_SR_eggs) * pop.mx[,"SR_eggs_d2"][t-1]
        pop.mx[,"SR_larvae_d1"][t] <- (1-mu_SR_eggs) * pop.mx[,"SR_eggs_d3"][t-1]
        pop.mx[,"SR_larvae_d2"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d1"][t-1]
        pop.mx[,"SR_larvae_d3"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d2"][t-1]
        pop.mx[,"SR_larvae_d4"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d3"][t-1]
        pop.mx[,"SR_larvae_d5"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d4"][t-1]
        pop.mx[,"SR_larvae_d6"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d5"][t-1]
        pop.mx[,"SR_larvae_d7"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d6"][t-1]
        pop.mx[,"SR_larvae_d8"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d7"][t-1]
        pop.mx[,"SR_pupae_d1"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"SR_larvae_d8"][t-1]
        pop.mx[,"SR_pupae_d2"][t] <- (1-mu_SR_pupae) * pop.mx[,"SR_pupae_d1"][t-1]
        pop.mx[,"SR_E0_d1"][t] <- (1-mu_SR_pupae) * pop.mx[,"SR_pupae_d2"][t-1] * females                  #rest
        pop.mx[,"SR_E0_d2"][t] <- (1-mu_SR_E0[1]) * pop.mx[,"SR_E0_d1"][t-1]                   #mate
        pop.mx[,"SR_E0_d3"][t] <- (1-mu_SR_E0[2]) * pop.mx[,"SR_E0_d2"][t-1]                   #first feed
        
        pop.mx[,"SR_E0_d4"][t] <- (1-mu_SR_E0[3]) * q * pop.mx[,"SR_E0_d3"][t-1]      #rest (move to E1)
        pop.mx[,"SR_E0_d5"][t] <- (1-mu_SR_E0[4]) * pop.mx[,"SR_E0_d4"][t-1]                   #rest
        pop.mx[,"SR_E0_d6"][t] <- (1-mu_SR_E0[5]) * pop.mx[,"SR_E0_d5"][t-1]                   #lay (contribute to eggs)
        pop.mx[,"SR_E0_d7"][t] <- (1-mu_SR_E0[6]) * pop.mx[,"SR_E0_d6"][t-1]                   #second feed
        
        pop.mx[,"SR_E0_d8"][t] <- (1-mu_SR_E0[7]) * q1a * pop.mx[,"SR_E0_d7"][t-1]      #rest (move to E1)
        pop.mx[,"SR_E0_d9"][t] <- (1-mu_SR_E0[8]) * pop.mx[,"SR_E0_d8"][t-1]                   #rest
        pop.mx[,"SR_E0_d10"][t] <- (1-mu_SR_E0[9]) * pop.mx[,"SR_E0_d9"][t-1]                  #lay (contribute to eggs)
        pop.mx[,"SR_E0_d11"][t] <- (1-mu_SR_E0[10]) * pop.mx[,"SR_E0_d10"][t-1]                #third feed
        
        pop.mx[,"SR_E0_d12"][t] <- (1-mu_SR_E0[11]) * q2a * pop.mx[,"SR_E0_d11"][t-1]   #rest (move to E1)
        pop.mx[,"SR_E0_d13"][t] <- (1-mu_SR_E0[12]) * pop.mx[,"SR_E0_d12"][t-1]                #rest
        pop.mx[,"SR_E0_d14"][t] <- (1-mu_SR_E0[13]) * pop.mx[,"SR_E0_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E0_d15"][t] <- (1-mu_SR_E0[14]) * pop.mx[,"SR_E0_d14"][t-1]                #fourth feed ----------------- (count for transmission)
        
        pop.mx[,"SR_E0_d16"][t] <- (1-mu_SR_E0[15]) * q3a * pop.mx[,"SR_E0_d15"][t-1]   #rest (move to E1)
        pop.mx[,"SR_E0_d17"][t] <- (1-mu_SR_E0[16]) * pop.mx[,"SR_E0_d16"][t-1]                #rest
        pop.mx[,"SR_E0_d18"][t] <- (1-mu_SR_E0[17]) * pop.mx[,"SR_E0_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E0_d19"][t] <- (1-mu_SR_E0[18]) * pop.mx[,"SR_E0_d18"][t-1]                #fifth feed ----------------- (count for transmission)
        
        pop.mx[,"SR_E0_d20"][t] <- (1-mu_SR_E0[19]) * q4a * pop.mx[,"SR_E0_d19"][t-1]   #rest (move to E1)
        pop.mx[,"SR_E0_d21"][t] <- (1-mu_SR_E0[20]) * pop.mx[,"SR_E0_d20"][t-1]                #rest
        pop.mx[,"SR_E0_d22"][t] <- (1-mu_SR_E0[21]) * pop.mx[,"SR_E0_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E0_d23"][t] <- (1-mu_SR_E0[22]) * pop.mx[,"SR_E0_d22"][t-1]                #sixth feed ----------------- (count for transmission)
        
        pop.mx[,"SR_E0_d24"][t] <- (1-mu_SR_E0[23]) * q5a * pop.mx[,"SR_E0_d23"][t-1]   #rest (move to E1)
        pop.mx[,"SR_E0_d25"][t] <- (1-mu_SR_E0[24]) * pop.mx[,"SR_E0_d24"][t-1]                #rest
        pop.mx[,"SR_E0_d26"][t] <- (1-mu_SR_E0[25]) * pop.mx[,"SR_E0_d25"][t-1]                #lay (contribute to eggs)
        
        ## SINGLE EXPOSURES
        
        n_exposed =  p * pop.mx[,"SR_E0_d3"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d3"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d4"][t] <- (1-mu_SR_E0[3]) * p * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d3"][t-1]          #rest
        pop.mx[,"SR_E1_d5"][t] <- (1-mu_SR_E1[4]) * pop.mx[,"SR_E1_d4"][t-1]                    #rest
        pop.mx[,"SR_E1_d6"][t] <- (1-mu_SR_E1[5]) * pop.mx[,"SR_E1_d5"][t-1]                    #lay (contribute to eggs)
        pop.mx[,"SR_E1_d7"][t] <- (1-mu_SR_E1[6]) * pop.mx[,"SR_E1_d6"][t-1]                    #second feed
        
        n_exposed =  p1a * pop.mx[,"SR_E0_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d7"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d8"][t] <- ((1-mu_SR_E1[7]) * q1b * pop.mx[,"SR_E1_d7"][t-1]) + ((1-mu_SR_E0[7]) * p1a * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d7"][t-1])       #rest
        pop.mx[,"SR_E1_d9"][t] <- (1-mu_SR_E1[8]) * pop.mx[,"SR_E1_d8"][t-1]                    #rest
        pop.mx[,"SR_E1_d10"][t] <- (1-mu_SR_E1[9]) * pop.mx[,"SR_E1_d9"][t-1]                  #lay (contribute to eggs)
        pop.mx[,"SR_E1_d11"][t] <- (1-mu_SR_E1[10]) * pop.mx[,"SR_E1_d10"][t-1]                #third feed
        
        n_exposed =  p2a * pop.mx[,"SR_E0_d11"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d11"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d12"][t] <- ((1-mu_SR_E1[11]) * q2b * pop.mx[,"SR_E1_d11"][t-1]) + ((1-mu_SR_E0[11]) * p2a * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d11"][t-1])   #rest
        pop.mx[,"SR_E1_d13"][t] <- (1-mu_SR_E1[12]) * pop.mx[,"SR_E1_d12"][t-1]                #rest
        pop.mx[,"SR_E1_d14"][t] <- (1-mu_SR_E1[13]) * pop.mx[,"SR_E1_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E1_d15"][t] <- (1-mu_SR_E1[14]) * pop.mx[,"SR_E1_d14"][t-1]                #fourth feed ----------------- (count for transmission)
        
        n_exposed =  p3a * pop.mx[,"SR_E0_d15"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d15"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d16"][t] <- ((1-mu_SR_E1[15]) * q3b * pop.mx[,"SR_E1_d15"][t-1]) + ((1-mu_SR_E0[15]) * p3a * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d15"][t-1])   #rest
        pop.mx[,"SR_E1_d17"][t] <- (1-mu_SR_E1[16]) * pop.mx[,"SR_E1_d16"][t-1]                #rest
        pop.mx[,"SR_E1_d18"][t] <- (1-mu_SR_E1[17]) * pop.mx[,"SR_E1_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E1_d19"][t] <- (1-mu_SR_E1[18]) * pop.mx[,"SR_E1_d18"][t-1]                #fifth feed ----------------- (count for transmission)
        
        n_exposed =  p4a * pop.mx[,"SR_E0_d19"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d19"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d20"][t] <- ((1-mu_SR_E1[19]) * q4b * pop.mx[,"SR_E1_d19"][t-1]) + ((1-mu_SR_E0[19]) * p4a * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d19"][t-1])   #rest
        pop.mx[,"SR_E1_d21"][t] <- (1-mu_SR_E1[20]) * pop.mx[,"SR_E1_d20"][t-1]                #rest
        pop.mx[,"SR_E1_d22"][t] <- (1-mu_SR_E1[21]) * pop.mx[,"SR_E1_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E1_d23"][t] <- (1-mu_SR_E1[22]) * pop.mx[,"SR_E1_d22"][t-1]                #sixth feed ----------------- (count for transmission)
        
        n_exposed =  p5a * pop.mx[,"SR_E0_d23"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E0_d23"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E1_d24"][t] <- ((1-mu_SR_E1[23]) * q5b * pop.mx[,"SR_E1_d23"][t-1]) + ((1-mu_SR_E0[23]) * p5a * (1-mu_SR_ITN[1]) * pop.mx[,"SR_E0_d23"][t-1])   #rest
        pop.mx[,"SR_E1_d25"][t] <- (1-mu_SR_E1[24]) * pop.mx[,"SR_E1_d24"][t-1]                #rest
        pop.mx[,"SR_E1_d26"][t] <- (1-mu_SR_E1[25]) * pop.mx[,"SR_E1_d25"][t-1]                #lay (contribute to eggs)
        
        ## MULTIPLE EXPOSURES
        
        n_exposed =  p1b * pop.mx[,"SR_E1_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"SR_E1_d7"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E2_d8"][t] <- (1-mu_SR_E1[7]) * p1b * (1-mu_SR_ITN[2]) * pop.mx[,"SR_E1_d7"][t-1]          #rest
        pop.mx[,"SR_E2_d9"][t] <- (1-mu_SR_E2[8]) * pop.mx[,"SR_E2_d8"][t-1]                    #rest
        pop.mx[,"SR_E2_d10"][t] <- (1-mu_SR_E2[9]) * pop.mx[,"SR_E2_d9"][t-1]                  #lay (contribute to eggs)
        pop.mx[,"SR_E2_d11"][t] <- (1-mu_SR_E2[10]) * pop.mx[,"SR_E2_d10"][t-1]                #third feed
        
        n_exposed =  (p2b * pop.mx[,"SR_E1_d11"][t-1] + p2c * pop.mx[,"SR_E2_d11"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SR_E1_d11"][t-1] + pop.mx[,"SR_E2_d11"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E2_d12"][t] <- (((1-mu_SR_E2[11]) * q2c * pop.mx[,"SR_E2_d11"][t-1])
                                    + ((1-mu_SR_E1[11]) * p2b * (1-mu_SR_ITN[2]) * pop.mx[,"SR_E1_d11"][t-1])
                                    + ((1-mu_SR_E2[11]) * p2c * (1-mu_SR_ITN[3]) * pop.mx[,"SR_E2_d11"][t-1]))   #rest
        pop.mx[,"SR_E2_d13"][t] <- (1-mu_SR_E2[12]) * pop.mx[,"SR_E2_d12"][t-1]                #rest
        pop.mx[,"SR_E2_d14"][t] <- (1-mu_SR_E2[13]) * pop.mx[,"SR_E2_d13"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E2_d15"][t] <- (1-mu_SR_E2[14]) * pop.mx[,"SR_E2_d14"][t-1]                #fourth feed ----------------- (count for transmission)
        
        n_exposed =  (p3b * pop.mx[,"SR_E1_d15"][t-1] + p3c * pop.mx[,"SR_E2_d15"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SR_E1_d15"][t-1] + pop.mx[,"SR_E2_d15"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E2_d16"][t] <- (((1-mu_SR_E2[15]) * q3c * pop.mx[,"SR_E2_d15"][t-1])
                                    + ((1-mu_SR_E1[15]) * p3b * (1-mu_SR_ITN[2]) * pop.mx[,"SR_E1_d15"][t-1])
                                    + ((1-mu_SR_E2[15]) * p3c * (1-mu_SR_ITN[3]) * pop.mx[,"SR_E2_d15"][t-1]))   #rest
        pop.mx[,"SR_E2_d17"][t] <- (1-mu_SR_E2[16]) * pop.mx[,"SR_E2_d16"][t-1]                #rest
        pop.mx[,"SR_E2_d18"][t] <- (1-mu_SR_E2[17]) * pop.mx[,"SR_E2_d17"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E2_d19"][t] <- (1-mu_SR_E2[18]) * pop.mx[,"SR_E2_d18"][t-1]                #fifth feed ----------------- (count for transmission)
        
        n_exposed =  (p4b * pop.mx[,"SR_E1_d19"][t-1] + p4c * pop.mx[,"SR_E2_d19"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SR_E1_d19"][t-1] + pop.mx[,"SR_E2_d19"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E2_d20"][t] <- (((1-mu_SR_E2[19]) * q4c * pop.mx[,"SR_E2_d19"][t-1])
                                    + ((1-mu_SR_E1[19]) * p4b * (1-mu_SR_ITN[2]) * pop.mx[,"SR_E1_d19"][t-1])
                                    + ((1-mu_SR_E2[19]) * p4c * (1-mu_SR_ITN[3]) * pop.mx[,"SR_E2_d19"][t-1]))   #rest
        pop.mx[,"SR_E2_d21"][t] <- (1-mu_SR_E2[20]) * pop.mx[,"SR_E2_d20"][t-1]                #rest
        pop.mx[,"SR_E2_d22"][t] <- (1-mu_SR_E2[21]) * pop.mx[,"SR_E2_d21"][t-1]                #lay (contribute to eggs)
        pop.mx[,"SR_E2_d23"][t] <- (1-mu_SR_E2[22]) * pop.mx[,"SR_E2_d22"][t-1]                #sixth feed ----------------- (count for transmission)
        
        n_exposed =  (p5b * pop.mx[,"SR_E1_d23"][t-1] + p5c * pop.mx[,"SR_E2_d23"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"SR_E1_d23"][t-1] + pop.mx[,"SR_E2_d23"][t-1] + potentially_exposed
        
        pop.mx[,"SR_E2_d24"][t] <- (((1-mu_SR_E2[23]) * q5c * pop.mx[,"SR_E2_d23"][t-1])
                                    + ((1-mu_SR_E1[23]) * p5b * (1-mu_SR_ITN[2]) * pop.mx[,"SR_E1_d23"][t-1])
                                    + ((1-mu_SR_E2[23]) * p5c * (1-mu_SR_ITN[3]) * pop.mx[,"SR_E2_d23"][t-1]))   #rest
        pop.mx[,"SR_E2_d25"][t] <- (1-mu_SR_E2[24]) * pop.mx[,"SR_E2_d24"][t-1]                #rest
        pop.mx[,"SR_E2_d26"][t] <- (1-mu_SR_E2[25]) * pop.mx[,"SR_E2_d25"][t-1]                #lay (contribute to eggs)
        
        ## RR Compartments
        ## UNEXPOSED
        pop.mx[,"RR_eggs_d1"][t] <- ((pop.mx[,"SR_E0_d6"][t-1] + pop.mx[,"SR_E1_d6"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-5] + (1/2)*pop.mx[,"RR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                     + (pop.mx[,"SR_E0_d10"][t-1] + pop.mx[,"SR_E1_d10"][t-1] + pop.mx[,"SR_E2_d10"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-9] + (1/2)*pop.mx[,"RR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                     + (pop.mx[,"SR_E0_d14"][t-1] + pop.mx[,"SR_E1_d14"][t-1] + pop.mx[,"SR_E2_d14"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-13] + (1/2)*pop.mx[,"RR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                     + (pop.mx[,"SR_E0_d18"][t-1] + pop.mx[,"SR_E1_d18"][t-1] + pop.mx[,"SR_E2_d18"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-17] + (1/2)*pop.mx[,"RR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                     + (pop.mx[,"SR_E0_d22"][t-1] + pop.mx[,"SR_E1_d22"][t-1] + pop.mx[,"SR_E2_d22"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-21] + (1/2)*pop.mx[,"RR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                     + (pop.mx[,"SR_E0_d26"][t-1] + pop.mx[,"SR_E1_d26"][t-1] + pop.mx[,"SR_E2_d26"][t-1]) * SR_eggcount * ((1/4)*pop.mx[,"SR_POP"][t-25] + (1/2)*pop.mx[,"RR_POP"][t-25])/pop.mx[,"POP"][t-25]
                                     + (pop.mx[,"RR_E0_d6"][t-1]  + pop.mx[,"RR_E1_d6"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-5] + pop.mx[,"RR_POP"][t-5])/pop.mx[,"POP"][t-5]
                                     + (pop.mx[,"RR_E0_d10"][t-1] + pop.mx[,"RR_E1_d10"][t-1] + pop.mx[,"RR_E2_d10"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-9] + pop.mx[,"RR_POP"][t-9])/pop.mx[,"POP"][t-9]
                                     + (pop.mx[,"RR_E0_d14"][t-1] + pop.mx[,"RR_E1_d14"][t-1] + pop.mx[,"RR_E2_d14"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-13] + pop.mx[,"RR_POP"][t-13])/pop.mx[,"POP"][t-13]
                                     + (pop.mx[,"RR_E0_d18"][t-1] + pop.mx[,"RR_E1_d18"][t-1] + pop.mx[,"RR_E2_d18"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-17] + pop.mx[,"RR_POP"][t-17])/pop.mx[,"POP"][t-17]
                                     + (pop.mx[,"RR_E0_d22"][t-1] + pop.mx[,"RR_E1_d22"][t-1] + pop.mx[,"RR_E2_d22"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-21] + pop.mx[,"RR_POP"][t-21])/pop.mx[,"POP"][t-21]
                                     + (pop.mx[,"RR_E0_d26"][t-1] + pop.mx[,"RR_E1_d26"][t-1] + pop.mx[,"RR_E2_d26"][t-1]) * RR_eggcount * ((1/2)*pop.mx[,"SR_POP"][t-25] + pop.mx[,"RR_POP"][t-25])/pop.mx[,"POP"][t-25])
        pop.mx[,"RR_eggs_d2"][t] <- (1-mu_RR_eggs) * pop.mx[,"RR_eggs_d1"][t-1]
        pop.mx[,"RR_eggs_d3"][t] <- (1-mu_RR_eggs) * pop.mx[,"RR_eggs_d2"][t-1]
        pop.mx[,"RR_larvae_d1"][t] <- (1-mu_RR_eggs) * pop.mx[,"RR_eggs_d3"][t-1]
        pop.mx[,"RR_larvae_d2"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d1"][t-1]
        pop.mx[,"RR_larvae_d3"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d2"][t-1]
        pop.mx[,"RR_larvae_d4"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d3"][t-1]
        pop.mx[,"RR_larvae_d5"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d4"][t-1]
        pop.mx[,"RR_larvae_d6"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d5"][t-1]
        pop.mx[,"RR_larvae_d7"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d6"][t-1]
        pop.mx[,"RR_larvae_d8"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d7"][t-1]
        pop.mx[,"RR_pupae_d1"][t] <- exp(-0.0356-(pop.mx[,"larval_pop"][t-1]/k)) * pop.mx[,"RR_larvae_d8"][t-1]
        
        pop.mx[,"RR_pupae_d2"][t] <- (1-mu_RR_pupae) * pop.mx[,"RR_pupae_d1"][t-1]
        pop.mx[,"RR_E0_d1"][t] <- (1-mu_RR_pupae) * pop.mx[,"RR_pupae_d2"][t-1] * females          #rest
        pop.mx[,"RR_E0_d2"][t] <- (1-mu_RR_E0[1]) * pop.mx[,"RR_E0_d1"][t-1]                    #mate
        pop.mx[,"RR_E0_d3"][t] <- (1-mu_RR_E0[2]) * pop.mx[,"RR_E0_d2"][t-1]                    #first feed
        
        pop.mx[,"RR_E0_d4"][t] <- (1-mu_RR_E0[3]) * q * pop.mx[,"RR_E0_d3"][t-1]       #rest (move to E1)
        pop.mx[,"RR_E0_d5"][t] <- (1-mu_RR_E0[4]) * pop.mx[,"RR_E0_d4"][t-1]                    #rest
        pop.mx[,"RR_E0_d6"][t] <- (1-mu_RR_E0[5]) * pop.mx[,"RR_E0_d5"][t-1]                    #lay (contribute to eggs)
        pop.mx[,"RR_E0_d7"][t] <- (1-mu_RR_E0[6]) * pop.mx[,"RR_E0_d6"][t-1]                    #second feed
        
        pop.mx[,"RR_E0_d8"][t] <- (1-mu_RR_E0[7]) * q1a * pop.mx[,"RR_E0_d7"][t-1]       #rest (move to E1)
        pop.mx[,"RR_E0_d9"][t] <- (1-mu_RR_E0[8]) * pop.mx[,"RR_E0_d8"][t-1]                    #rest
        pop.mx[,"RR_E0_d10"][t] <- (1-mu_RR_E0[9]) * pop.mx[,"RR_E0_d9"][t-1]                   #lay (contribute to eggs)
        pop.mx[,"RR_E0_d11"][t] <- (1-mu_RR_E0[10]) * pop.mx[,"RR_E0_d10"][t-1]                 #third feed
        
        pop.mx[,"RR_E0_d12"][t] <- (1-mu_RR_E0[11]) * q2a * pop.mx[,"RR_E0_d11"][t-1]    #rest (move to E1)
        pop.mx[,"RR_E0_d13"][t] <- (1-mu_RR_E0[12]) * pop.mx[,"RR_E0_d12"][t-1]                 #rest
        pop.mx[,"RR_E0_d14"][t] <- (1-mu_RR_E0[13]) * pop.mx[,"RR_E0_d13"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E0_d15"][t] <- (1-mu_RR_E0[14]) * pop.mx[,"RR_E0_d14"][t-1]                 #fourth feed ----------------- (count for transmission)
        
        pop.mx[,"RR_E0_d16"][t] <- (1-mu_RR_E0[15]) * q3a * pop.mx[,"RR_E0_d15"][t-1]    #rest (move to E1)
        pop.mx[,"RR_E0_d17"][t] <- (1-mu_RR_E0[16]) * pop.mx[,"RR_E0_d16"][t-1]                 #rest
        pop.mx[,"RR_E0_d18"][t] <- (1-mu_RR_E0[17]) * pop.mx[,"RR_E0_d17"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E0_d19"][t] <- (1-mu_RR_E0[18]) * pop.mx[,"RR_E0_d18"][t-1]                 #fifth feed ----------------- (count for transmission)
        
        pop.mx[,"RR_E0_d20"][t] <- (1-mu_RR_E0[19]) * q4a * pop.mx[,"RR_E0_d19"][t-1]    #rest (move to E1)
        pop.mx[,"RR_E0_d21"][t] <- (1-mu_RR_E0[20]) * pop.mx[,"RR_E0_d20"][t-1]                 #rest
        pop.mx[,"RR_E0_d22"][t] <- (1-mu_RR_E0[21]) * pop.mx[,"RR_E0_d21"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E0_d23"][t] <- (1-mu_RR_E0[22]) * pop.mx[,"RR_E0_d22"][t-1]                 #sixth feed ----------------- (count for transmission)
        
        pop.mx[,"RR_E0_d24"][t] <- (1-mu_RR_E0[23]) * q5a * pop.mx[,"RR_E0_d23"][t-1]    #rest (move to E1)
        pop.mx[,"RR_E0_d25"][t] <- (1-mu_RR_E0[24]) * pop.mx[,"RR_E0_d24"][t-1]                 #rest
        pop.mx[,"RR_E0_d26"][t] <- (1-mu_RR_E0[25]) * pop.mx[,"RR_E0_d25"][t-1]                 #lay (contribute to eggs)
        
        ## SINGLE EXPOSURES
        n_exposed =  p * pop.mx[,"RR_E0_d3"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d3"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d4"][t] <- (1-mu_RR_E0[3]) * p * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d3"][t-1]          #rest
        pop.mx[,"RR_E1_d5"][t] <- (1-mu_RR_E1[4]) * pop.mx[,"RR_E1_d4"][t-1]                    #rest
        pop.mx[,"RR_E1_d6"][t] <- (1-mu_RR_E1[5]) * pop.mx[,"RR_E1_d5"][t-1]                    #lay (contribute to eggs)
        pop.mx[,"RR_E1_d7"][t] <- (1-mu_RR_E1[6]) * pop.mx[,"RR_E1_d6"][t-1]                    #second feed
        
        n_exposed = p1a  * pop.mx[,"RR_E0_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d7"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d8"][t] <- ((1-mu_RR_E1[7]) * q1b * pop.mx[,"RR_E1_d7"][t-1]) + ((1-mu_RR_E0[7]) * p1a * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d7"][t-1])        #rest
        pop.mx[,"RR_E1_d9"][t] <- (1-mu_RR_E1[8]) * pop.mx[,"RR_E1_d8"][t-1]                    #rest
        pop.mx[,"RR_E1_d10"][t] <- (1-mu_RR_E1[9]) * pop.mx[,"RR_E1_d9"][t-1]                   #lay (contribute to eggs)
        pop.mx[,"RR_E1_d11"][t] <- (1-mu_RR_E1[10]) * pop.mx[,"RR_E1_d10"][t-1]                 #third feed
        
        n_exposed =  p2a * pop.mx[,"RR_E0_d11"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d11"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d12"][t] <- ((1-mu_RR_E1[11]) * q2b * pop.mx[,"RR_E1_d11"][t-1]) + ((1-mu_RR_E0[11]) * p2a * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d11"][t-1])   #rest
        pop.mx[,"RR_E1_d13"][t] <- (1-mu_RR_E1[12]) * pop.mx[,"RR_E1_d12"][t-1]                 #rest
        pop.mx[,"RR_E1_d14"][t] <- (1-mu_RR_E1[13]) * pop.mx[,"RR_E1_d13"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E1_d15"][t] <- (1-mu_RR_E1[14]) * pop.mx[,"RR_E1_d14"][t-1]                 #fourth feed ----------------- (count for transmission)
        
        n_exposed =  p3a * pop.mx[,"RR_E0_d15"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d15"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d16"][t] <- ((1-mu_RR_E1[15]) * q3b * pop.mx[,"RR_E1_d15"][t-1]) + ((1-mu_RR_E0[15]) * p3a * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d15"][t-1])   #rest
        pop.mx[,"RR_E1_d17"][t] <- (1-mu_RR_E1[16]) * pop.mx[,"RR_E1_d16"][t-1]                 #rest
        pop.mx[,"RR_E1_d18"][t] <- (1-mu_RR_E1[17]) * pop.mx[,"RR_E1_d17"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E1_d19"][t] <- (1-mu_RR_E1[18]) * pop.mx[,"RR_E1_d18"][t-1]                 #fifth feed ----------------- (count for transmission)
        
        n_exposed =  p4a * pop.mx[,"RR_E0_d19"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d19"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d20"][t] <- ((1-mu_RR_E1[19]) * q4b * pop.mx[,"RR_E1_d19"][t-1]) + ((1-mu_RR_E0[19]) * p4a * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d19"][t-1])   #rest
        pop.mx[,"RR_E1_d21"][t] <- (1-mu_RR_E1[20]) * pop.mx[,"RR_E1_d20"][t-1]                 #rest
        pop.mx[,"RR_E1_d22"][t] <- (1-mu_RR_E1[21]) * pop.mx[,"RR_E1_d21"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E1_d23"][t] <- (1-mu_RR_E1[22]) * pop.mx[,"RR_E1_d22"][t-1]                 #sixth feed ----------------- (count for transmission)
        
        n_exposed =  p5a * pop.mx[,"RR_E0_d23"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E0_d23"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E1_d24"][t] <- ((1-mu_RR_E1[23]) * q5b * pop.mx[,"RR_E1_d23"][t-1]) + ((1-mu_RR_E0[23]) * p5a * (1-mu_RR_ITN[1]) * pop.mx[,"RR_E0_d23"][t-1])   #rest
        pop.mx[,"RR_E1_d25"][t] <- (1-mu_RR_E1[24]) * pop.mx[,"RR_E1_d24"][t-1]                 #rest
        pop.mx[,"RR_E1_d26"][t] <- (1-mu_RR_E1[25]) * pop.mx[,"RR_E1_d25"][t-1]                 #lay (contribute to eggs)
        
        ## MULTIPLE EXPOSURES
        
        n_exposed =  p1b * pop.mx[,"RR_E1_d7"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E1_d7"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E2_d8"][t] <- (1-mu_RR_E1[7]) * p1b * (1-mu_RR_ITN[2]) * pop.mx[,"RR_E1_d7"][t-1]          #rest
        pop.mx[,"RR_E2_d9"][t] <- (1-mu_RR_E2[8]) * pop.mx[,"RR_E2_d8"][t-1]                    #rest
        pop.mx[,"RR_E2_d10"][t] <- (1-mu_RR_E2[9]) * pop.mx[,"RR_E2_d9"][t-1]                   #lay (contribute to eggs)
        pop.mx[,"RR_E2_d11"][t] <- (1-mu_RR_E2[10]) * pop.mx[,"RR_E2_d10"][t-1]                 #third feed
        
        n_exposed =  (p2b * pop.mx[,"RR_E1_d11"][t-1]) + (p2c * pop.mx[,"RR_E2_d11"][t-1]) + n_exposed
        potentially_exposed = pop.mx[,"RR_E1_d11"][t-1] + pop.mx[,"RR_E2_d11"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E2_d12"][t] <- (((1-mu_RR_E2[11]) * q2c * pop.mx[,"RR_E2_d11"][t-1])
                                    + ((1-mu_RR_E1[11]) * p2b * (1-mu_RR_ITN[2]) * pop.mx[,"RR_E1_d11"][t-1])
                                    + ((1-mu_RR_E2[11]) * p2c * (1-mu_RR_ITN[3]) * pop.mx[,"RR_E2_d11"][t-1]))   #rest
        pop.mx[,"RR_E2_d13"][t] <- (1-mu_RR_E2[12]) * pop.mx[,"RR_E2_d12"][t-1]                 #rest
        pop.mx[,"RR_E2_d14"][t] <- (1-mu_RR_E2[13]) * pop.mx[,"RR_E2_d13"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E2_d15"][t] <- (1-mu_RR_E2[14]) * pop.mx[,"RR_E2_d14"][t-1]                 #fourth feed ----------------- (count for transmission)
        
        n_exposed =  p3b * pop.mx[,"RR_E1_d15"][t-1] + p3c * pop.mx[,"RR_E2_d15"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E1_d15"][t-1] + pop.mx[,"RR_E2_d15"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E2_d16"][t] <- (((1-mu_RR_E2[15]) * q3c * pop.mx[,"RR_E2_d15"][t-1])
                                    + ((1-mu_RR_E1[15]) * p3b * (1-mu_RR_ITN[2]) * pop.mx[,"RR_E1_d15"][t-1])
                                    + ((1-mu_RR_E2[15]) * p3c * (1-mu_RR_ITN[3]) * pop.mx[,"RR_E2_d15"][t-1]))   #rest
        pop.mx[,"RR_E2_d17"][t] <- (1-mu_RR_E2[16]) * pop.mx[,"RR_E2_d16"][t-1]                 #rest
        pop.mx[,"RR_E2_d18"][t] <- (1-mu_RR_E2[17]) * pop.mx[,"RR_E2_d17"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E2_d19"][t] <- (1-mu_RR_E2[18]) * pop.mx[,"RR_E2_d18"][t-1]                 #fifth feed ----------------- (count for transmission)
        
        n_exposed =  p4b * pop.mx[,"RR_E1_d19"][t-1] + p4c * pop.mx[,"RR_E2_d19"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E1_d19"][t-1] + pop.mx[,"RR_E2_d19"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E2_d20"][t] <- (((1-mu_RR_E2[19]) * q4c * pop.mx[,"RR_E2_d19"][t-1])
                                    + ((1-mu_RR_E1[19]) * p4b * (1-mu_RR_ITN[2]) * pop.mx[,"RR_E1_d19"][t-1])
                                    + ((1-mu_RR_E2[19]) * p4c * (1-mu_RR_ITN[3]) * pop.mx[,"RR_E2_d19"][t-1]))   #rest
        pop.mx[,"RR_E2_d21"][t] <- (1-mu_RR_E2[20]) * pop.mx[,"RR_E2_d20"][t-1]                 #rest
        pop.mx[,"RR_E2_d22"][t] <- (1-mu_RR_E2[21]) * pop.mx[,"RR_E2_d21"][t-1]                 #lay (contribute to eggs)
        pop.mx[,"RR_E2_d23"][t] <- (1-mu_RR_E2[22]) * pop.mx[,"RR_E2_d22"][t-1]                 #sixth feed ----------------- (count for transmission)
        
        n_exposed =  p5b * pop.mx[,"RR_E1_d23"][t-1] + p5c * pop.mx[,"RR_E2_d23"][t-1] + n_exposed
        potentially_exposed = pop.mx[,"RR_E1_d23"][t-1] + pop.mx[,"RR_E2_d23"][t-1] + potentially_exposed
        
        pop.mx[,"RR_E2_d24"][t] <- (((1-mu_RR_E2[23]) * q5c * pop.mx[,"RR_E2_d23"][t-1])
                                    + ((1-mu_RR_E1[23]) * p5b * (1-mu_RR_ITN[2]) * pop.mx[,"RR_E1_d23"][t-1])
                                    + ((1-mu_RR_E2[23]) * p5c * (1-mu_RR_ITN[3]) * pop.mx[,"RR_E2_d23"][t-1]))   #rest
        pop.mx[,"RR_E2_d25"][t] <- (1-mu_RR_E2[24]) * pop.mx[,"RR_E2_d24"][t-1]                 #rest
        pop.mx[,"RR_E2_d26"][t] <- (1-mu_RR_E2[25]) * pop.mx[,"RR_E2_d25"][t-1]                 #lay (contribute to eggs)
        
        pop.mx[,"n_exposed"][t] <- n_exposed
        pop.mx[,"potentially_exposed"][t] <-  potentially_exposed
        pop.mx[,"pr_exposed"][t] <- n_exposed/potentially_exposed
        
        # infection model
        pop.mx[,"I_H"][t] <-  pop.mx[,"BetaH"][t-1]*(1-pop.mx[,"I_H"][t-1])-(v*pop.mx[,"I_H"][t-1]) + pop.mx[,"I_H"][t-1]        #infected human population
        if (is.na(pop.mx[,"I_H"][t])) {
          pop.mx[,"I_H"][t] <- 0
        } else if(pop.mx[,"I_H"][t]< 0) {                                                 #we can remove this if it's acceptable for I_H to be less than zero on the first day of the simulation (every other day is fine)
          pop.mx[,"I_H"][t] <-  0
        }
        
        pop.mx[,"BetaM"][t] <-  (c2*(1-(c1/(c2*pop.mx[,"I_H"][t-1]+c1))))                    #daily risk of a feeding mosquito becoming infected (from 2016 DBH paper)
        pop.mx[,"BetaH"][t] <-  (1-((1-b)^(a*pop.mx[,"infected_biting"][t-1])))    #daily risk of an uninfected human becoming infected
        
        # infected and biting: calculated based on population of each biting compartment and the BetaM (probability it became infected) in any feeding
        # days at least 12 days prior
        pop.mx[,"infected_biting"][t] <- ((pop.mx[,"SS_E0_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SS_E0_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SS_E0_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"SS_E1_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SS_E1_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SS_E1_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"SS_E2_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SS_E2_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SS_E2_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"SR_E0_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SR_E0_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SR_E0_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"SR_E1_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SR_E1_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SR_E1_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"SR_E2_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"SR_E2_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"SR_E2_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"RR_E0_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"RR_E0_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"RR_E0_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"RR_E1_d15"][t]*pop.mx[,"BetaM"][t-12])
                                          +(pop.mx[,"RR_E1_d19"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16]) + pop.mx[,"BetaM"][t-16]))
                                          +(pop.mx[,"RR_E1_d23"][t]*(pop.mx[,"BetaM"][t-12]*(1-pop.mx[,"BetaM"][t-16])*(1-pop.mx[,"BetaM"][t-20]) + pop.mx[,"BetaM"][t-16]*(1-pop.mx[,"BetaM"][t-20]) + (pop.mx[,"BetaM"][t-20])))
                                          +(pop.mx[,"RR_E2_d15"][t]*pop.mx[,"BetaM"][t-12]))
        
        
      }
      
      pop.mx %>%
        as.data.frame() %>%
        add_column("coverage"=cov/coverage_denom) %>%
        add_column("spatial_coefficient"=s) %>%
        bind_rows(pop_master) -> pop_master
      
      write.csv(pop_master, paste0("results/spatial_",s,"_coverage_",cov/coverage_denom,"_dominance_",h,".csv"))
      
    }
  }
}
