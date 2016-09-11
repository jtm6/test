#' @export
summary_multisims_output_processing_fxn <- function(path)
{
  #note work in progress 
  
  files            <-   list.files(path,full.names=TRUE)
  file_names       <-   sub("[.][^.]*$", "", list.files(path,full.names=FALSE), perl=TRUE)
  namesvec         <-   c("pop","param","popsumm")    
  out_list         <-   vector('list',length= length(files))
  names(out_list)  <-   file_names
  
  for(ii in 1:length(files))
  {
    load(files[[ii]])
    
    temp_model       <-  get(file_names[ii])
    temp_length      <-  length(temp_model$pop)
    temp_list        <-  vector('list',length= length(namesvec))
    names(temp_list) <-  namesvec
    if(length(temp_length)==length(temp_model$param))
    {
      temp_list$param  <-  temp_model$param[[1]]
    }else{temp_list$param  <-  temp_model$param}
    
    
    temp_list$pop    <-  vector('list',length=temp_length)
    temp_list$popsumm <-  vector('list',length=temp_length)
    
    for(jj in 1:temp_length)
    {
      temp_list$pop[[jj]]     <-  temp_model$pop[[jj]]
      temp_list$popsumm[[jj]]  <-  temp_model$popsumm[[jj]]     
    }
    
    out_list[[ii]] <- temp_list
    remove(list=as.character(file_names[ii],temp_model))
  }
  return(out_list)
}

############################################################3

prevalence_plot_fxn<-function(modelobj,titlevec)
{
  par(mfrow=c(2,2),mar=c(2,2,2,2),mgp=c(1,.25,0),tcl=-.2,oma=c(3,3,3,2))
  
  for(ii in 1:length(modelobj))
  {
    index_length <- length(modelobj[[ii]]$pop)
    temp_list    <- vector('list',length=index_length)
    
    for(jj in 1 : index_length)
    {
      temp_list[[jj]] <- ( modelobj[[ii]]$popsumm[[jj]]$total_infections /
                             modelobj[[ii]]$popsumm[[jj]]$alive )
    }
    
    ymax      <- max(unlist(temp_list))
    timesteps <- 1:modelobj[[1]]$param[["n_steps"]]/365
    
    plot(timesteps, temp_list[[1]], ylab="",xlab="",typ='l',ylim=c(0,ymax))
    
    if(!missing(titlevec)){mtext(titlevec[ii],side=3,line=0.5)}
    
    if(index_length>1)
    {
      for(kk in 2:index_length)
        lines(timesteps,temp_list[[kk]])  
    }
    
  }
  mtext("years",side=1,outer=T,line=1)  
  mtext("prevalence",side=2,outer=T,line=1)  
  
}
################################################################

inf_and_sus_plot_fxn<-function(modelobj,titlevec)
{
  par(mfrow=c(2,2),mar=c(2,2,2,2),mgp=c(1,.25,0),tcl=-.2,oma=c(3,3,3,2))
  
  for(ii in 1:length(modelobj))
  {
    index_length <- length(modelobj[[ii]]$pop)
    temp_list1    <- vector('list',length=index_length)
    temp_list2    <- vector('list',length=index_length)
    
    for(jj in 1 : index_length)
    {
      temp_list1[[jj]] <-  modelobj[[ii]]$popsumm[[jj]]$susceptibles 
      temp_list2[[jj]] <-  modelobj[[ii]]$popsumm[[jj]]$total_infections                           
    }
    
    ymax      <- max(c(unlist(temp_list1),unlist(temp_list2)))
    timesteps <- 1:modelobj[[1]]$param[["n_steps"]]/365
    
    plot(timesteps, temp_list1[[1]], ylab="",xlab="",typ='l',ylim=c(0,ymax))
    lines(timesteps, temp_list2[[1]], col=2)
    
    if(!missing(titlevec)){mtext(titlevec[ii],side=3,line=0.5)}
    
    if(index_length>1)
    {
      for(kk in 2:index_length)
      {
        lines(timesteps,temp_list1[[kk]])
        lines(timesteps,temp_list2[[kk]],col=2) 
      }
    }
  }
  
  mtext("years",side=1,outer=T,line=1)  
  mtext("susceptibles / infecteds",side=2,outer=T,line=1)  
  
}

