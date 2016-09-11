plot_output_summary1<-function(model,nsim)
{
#description:
#plots population size, deaths, infections over time for given simulation
  
  
  model$popsumm <- unlist(model$popsumm[nsim],recursive=FALSE)
  model$pop    <- unlist(model$pop[nsim],recursive=FALSE)
  
  births      <- cumsum(model$popsumm$births)
  nat_deaths  <- cumsum(model$popsumm$natural_deaths)
  aids_deaths <- cumsum(model$popsumm$aids_deaths)
  new_inf     <- cumsum(model$popsumm$new_infections)
  popsize <-    model$popsumm$alive
  
  ymax        <- max(c(births,nat_deaths,aids_deaths,new_inf,popsize))
  time_index_years <- (1:model$param[[nsim]]$n_steps)/365
  
  lwd=2

    plot(time_index_years,births,type='l',ylim=c(0,max(popsize)),lwd=lwd,col=1,
         ylab="no. individuals",xlab="years")
    lines(time_index_years, nat_deaths,type='l',lwd=lwd,col=2)
    lines(time_index_years,aids_deaths,type='l',lwd=lwd,col=3)
    lines(time_index_years,new_inf,type='l',lwd=lwd,col=4)
    lines(time_index_years,popsize,type='l',lwd=lwd,col=1,lty=2)
    
    legend("topleft",legend=c("popn size","cum. infs",
                              "cum. births","cum.aids.deaths",
                              "cum.nat.deaths"),
           col=c("black","blue","black","green","red"),
           lty=c(2,1,1,1,1))
    title(paste("population dynamics summary, sim:",nsim))
}
    
plot_output_summary2<-function(model,nsim)
{
  #description:
  #plots infections/susceptibles/prevalence
  
  model$popsumm <- unlist(model$popsumm[nsim],recursive=FALSE)
  model$pop    <- unlist(model$pop[nsim],recursive=FALSE)
  time_index_years <- (1:model$param[[nsim]]$n_steps)/365
  
  ymax=max(c(model$popsumm$susceptibles,
           model$popsumm$total_infections))
  lwd=2
  plot(time_index_years,model$popsumm$susceptibles,type='l',
       lwd=lwd,col=1,ylab="no. individuals",xlab="years",ylim=c(0,ymax))
  lines(time_index_years,model$popsumm$total_infections,
        type='l',lwd=lwd,col=2)
  par(new=T)
  prev=model$popsumm$total_infections/model$popsumm$susceptibles
  plot(time_index_years,prev,type='l',
       lwd=lwd,col=2,ylab=NA,xlab=NA,ylim=c(0,1),lty=2,axes=F)
  axis(4)
  
   title(paste("prevalence/susceptibles/prevalence summary, sim:",nsim))
}


plot_output_summary3<-function(model,nsim)
{
  temp_list <- model$pop[nsim][[1]]
  popdf <- as.data.frame(temp_list)
  
  inf_index <- which(!is.na(popdf$Donors_V))
  if(length(inf_index) >0)
  {
    hist(log10(popdf$Donors_V[inf_index]),
         breaks=100,
         freq=T, xlab="VL at transmission",
         ylab="Frequency",
         main="Plot 4: VL at transmission",
         xlim=c(0,9),col="blue")
  }
    
}  
#plot_output_summary1(evomodel,1)
#plot_output_summary2(evomodel,1)
#plot_output_summary3(evomodel,1)
