#' @export
summary_population<-function(dat,at){
#original summary fxn
  
  #Description: 
  #fill in summary statistics per timestep for
  #variables listed in input_parameter_popsumm_stats()
  #inputs/outputs: self-explanatory
  
#note: births,aids_deaths,natural_deaths stats filled in respective fxns
  

  
index_infected_all_types <- which(dat$pop$Status>=0)
#total infections
dat$popsumm$total_infections[at] <- length(which(dat$pop$Status == 1))
#total infections not treated
dat$popsumm$total_infections_not_treated[at] <- length(which(dat$pop$Status == 1 & dat$pop$treated==0))
#total susceptibles
dat$popsumm$susceptibles[at] <- length(which(dat$pop$Status == 0))
#total population (alive)
dat$popsumm$alive[at] <- dat$popsumm$total_infections[at]+ dat$popsumm$susceptibles[at]

dat$popsumm$no_treated[at] <- length(which(dat$pop$Status == 1 & dat$pop$treated == 1))

dat$popsumm$no_in_aids_gamma[at] <- length(which((at > (dat$pop$Time_Inf + dat$pop$RandomTimeToAIDS))
                                                 & dat$pop$treated!=1))

dat$popsumm$no_in_aids_cd4[at] <- length(which(dat$pop$CD4 == 4  & dat$pop$treated!=1))

#all aids_deaths
#dealt with in aids_death_fxn

#all natural_deaths
#dealt with in "natural_deaths_fxn"

#natural_deaths_infecteds
count <- length(which(dat$pop$Status == (-1) & dat$pop$Time_Death == at & 
                        dat$pop$SetPoint > 0 ))
dat$popsumm$natural_deaths_infecteds[at] <- count

#"natural_deaths_susceptibles"
count <- length(which(dat$pop$Status == (-1) & dat$pop$Time_Death == at & 
                        is.na(dat$pop$SetPoint) ))
dat$popsumm$natural_deaths_susceptibles[at] <- count

#"births"
# births are tabulated in "births_fxn"

#"new_infections"
index_new_infections <- which(dat$pop$Time_Inf == at  )
count <- length(index_new_infections)
dat$popsumm$new_infections[at] <- count
#mean age incidents
if(length(index_new_infections)>0)
  dat$popsumm$mean_age_incident[at]    <- mean(dat$pop$age[index_new_infections])

index_infected <- which(dat$pop$Status== 1)
index_infected_not_treated <- which(dat$pop$Status== 1 & dat$pop$treated == 0)

dat$popsumm$mean_spvl_pop_untreated[at]     <-  mean(dat$pop$LogSetPoint[index_infected_not_treated])
dat$popsumm$median_spvl_pop_untreated[at]   <-  median(dat$pop$LogSetPoint[index_infected_not_treated])
dat$popsumm$variance_spvl_pop_untreated[at] <-  var(dat$pop$LogSetPoint[index_infected_not_treated])

dat$popsumm$mean_spvl_pop_all[at]     <-  mean(dat$pop$LogSetPoint[index_infected])
dat$popsumm$median_spvl_pop_all[at]   <-  median(dat$pop$LogSetPoint[index_infected])
dat$popsumm$variance_spvl_pop_all[at] <-  var(dat$pop$LogSetPoint[index_infected])


dat$popsumm$mean_vl_pop_untreated[at]       <- mean(log10(dat$pop$V[index_infected_not_treated]))
dat$popsumm$median_vl_pop_untreated[at]     <- median(log10(dat$pop$V[index_infected_not_treated]))
dat$popsumm$variance_vl_pop_untreated[at]   <- var(log10(dat$pop$V[index_infected_not_treated]))

dat$popsumm$mean_vl_pop_all[at]       <- mean(log10(dat$pop$V[index_infected]))
dat$popsumm$median_vl_pop_all[at]     <- median(log10(dat$pop$V[index_infected]))
dat$popsumm$variance_vl_pop_all[at]   <- var(log10(dat$pop$V[index_infected]))

dat$popsumm$total_pills_taken[at]   <- dat$popsumm$total_pills_taken[at-1] + length(which(dat$pop$treated == 1))

dat$popsumm$mean_age_infecteds[at]    <- mean(dat$pop$age[index_infected])  
dat$popsumm$mean_age_susceptibles[at] <- mean(dat$pop$age[dat$pop$Status == 0])


aa <- summary(dat$nw~degree(0:1) + concurrent, at = at)
dat$popsumm$no_edges[at]   <- unname(summary(dat$nw~edges,at=at)[1,1])
dat$popsumm$mean_degree[at]   <- dat$popsumm$no_edges[at]*2/network.size(dat$nw)
total_nodes <- sum(aa[1,1]+aa[1,2]+aa[1,3])
dat$popsumm$no_nodes_degree_0[at]   <- aa[1,1]/total_nodes
dat$popsumm$no_nodes_degree_1[at]   <- aa[1,2]/total_nodes
dat$popsumm$no_nodes_concurrent[at] <- aa[1,3]/total_nodes

# Failed attempt to calculate mean degree for infected persons only
#   infected_dat <- dat[index_infected]
#   aa_infected <- summary(infected_dat$nw~degree(0:1) + concurrent, at = at)
#   dat$popsumm$no_edges_infected[at]   <- unname(summary(infected_dat$nw~edges,at=at)[1,1])
# I currently have a ticket for a better R programmer get this to work

return(dat)
}