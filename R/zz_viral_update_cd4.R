#' @export
viral_update_cd4 <- function(dat,at)
{  
  #11/18/15: original viral update cd4 fxn; 
  #superseded by "viral_update_cd4_daily"
  #should be deleted at some point
  
  
  #Description:
  # Shifts CD4 value of infectees to next category if time in current category 
  # has expired, waiting time per CD4 category is either based on exponential distributions 
  # based on Pickles et al. data. or raw waiting times reported from their data (param$cd4_exp_flag=0/1)
  # otherwise cd4 value remains same
  #Input: dat$pop$CD4_TimeToAIDS_exp_cat1
  #dat$pop$CD4_time_cat2
  #dat$pop$CD4_time_cat3
  #dat$pop$CD4_time_cat4
  #dat$pop$CD4_time
  #dat$param$CD4_lookup
  #Output: dat$pop$CD4, CD4_time_death
  
  #index of alive infectees
  infectees <- which(dat$pop$CD4 < 5 & dat$pop$Status==1  &
                     dat$pop$treated!=1)
  
  cd4_waiting_time <- dat$pop$CD4_time[infectees]/365
  
    cd4_threshold_time <- rep(NA_real_,length(infectees))
   cd4_cat_vec <- c("CD4_time_cat1","CD4_time_cat2","CD4_time_cat3",
                   "CD4_time_cat4")
   for(ii in 1:length(cd4_cat_vec)){
     ix <- which(dat$pop$CD4[infectees]==ii)
     if(length(ix)>0)
     {
       ix2<- infectees[ix]
       cd4_threshold_time[ix] <- dat$pop[[cd4_cat_vec[ii]]][ix2]
     } 
   }
  #identify agents whose cd4 values will increase  
  cd4_increment<- which(cd4_waiting_time>cd4_threshold_time)
  
  
  if(length(cd4_increment)>0)
  {    
    infected_and_cd4_increment <- infectees[cd4_increment]
    dat$pop$CD4[infected_and_cd4_increment] <- dat$pop$CD4[infected_and_cd4_increment] + 1
    dat$pop$CD4_time[infected_and_cd4_increment] <- 0
    dat$pop$CD4_nadir[infected_and_cd4_increment] <-dat$pop$CD4[infected_and_cd4_increment]
      
    cd4_dead <- which(dat$pop$CD4[infected_and_cd4_increment] ==5)

    #this starts VL progression to aids level when cd4 aids starts
    if(dat$param$aids_death_model=="cd4"){
    if(any(dat$pop$CD4[infected_and_cd4_increment]==4)){
       new_aids_index <- which(dat$pop$CD4[infected_and_cd4_increment]==4)
       final_index <- infected_and_cd4_increment[new_aids_index]
       dat$pop$RandomTimeToAIDS[final_index] <- at
    }}
    
    
    if(length(cd4_dead)>0)
    {
      cd4_dead_final_index <- infected_and_cd4_increment[cd4_dead]
      dat$pop$CD4_time_death[cd4_dead_final_index] <- at      
    }
  }
  
  cd4_unchanged <-  which(cd4_waiting_time<=cd4_threshold_time)
  if(length(cd4_unchanged)>0){dat$pop$CD4_time[cd4_unchanged] <- dat$pop$CD4_time[cd4_unchanged] +1   }
  
  #for agents on treatment
  treatment_index <- which(dat$pop$treated==1)
  if(length(treatment_index)>0)
  dat$pop$CD4[treatment_index] <- dat$param$cd4_after_treatment 
  
  return(dat)
}
  