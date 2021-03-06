#' @export
social_generic_attribute_transition <- function(dat,at)
{
  #Description:
  #probabilistically changes generic attribute values of agents each timestep
  #according to user specified transition values
  #generic_nodal_att_trans_mat (default value:NA): n x n matrix (n= # generic attributes)
  # where each row row represents probabilities of moving from current value to
  # other value, each row must sum to one;  if no changes occur, an identitiy
  #matrix is used

  #inputs: "params$generic_nodal_att_values","pop$att1","params$generic_nodal_att_trans_mat"
  #ouputs: "pop$att1"

  if(is.logical(dat$param$generic_nodal_att_trans_mat)){return(dat)}
  
  tempvals <- dat$pop$att1
  for( ii in dat$param$generic_nodal_att_values)
  {
    index <- which(dat$pop$att1 == ii & dat$pop$Status >= 0)
    size <- length(index)
    probs <- dat$param$generic_nodal_att_trans_mat[ii,]
    new_vals <- sample(dat$param$generic_nodal_att_values,
                       size = size, prob = probs, replace = T)
    tempvals[index] <- new_vals
  }
  dat$pop$att1 <- tempvals
  
  #set new values on network
  #assign values of initial generic_attribue to initial network
  temp_match<- match(dat$attr$id,dat$pop$id)
  #qaqc for now (10/8/15)
  if(any(is.na(temp_match))){browser()}
  
  network::set.vertex.attribute( x = dat$nw, attr = "att1",
                                 value = dat$pop$att1[temp_match])

  dat$attr$att1 <- dat$pop$att1[temp_match]
  return(dat)
}
