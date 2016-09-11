zz_resim_nets<-function (dat, at) 
{
  idsActive <- which(dat$attr$active == 1)
  anyActive <- ifelse(length(idsActive) > 0, TRUE, FALSE)
  if (dat$param$modes == 2) {
    nActiveM1 <- length(intersect(modeids(dat$nw, mode = 1), 
                                  idsActive))
    nActiveM2 <- length(intersect(modeids(dat$nw, mode = 2), 
                                  idsActive))
    anyActive <- ifelse(nActiveM1 > 0 & nActiveM2 > 0, TRUE, 
                        FALSE)
  }
  nwparam <- get_nwparam(dat)
  statOnNw <- ("status" %in% get_formula_terms(nwparam$formation))
  status <- dat$attr$status
  if (statOnNw == TRUE && length(unique(status)) == 1) {
    stop("Stopping simulation because status in formation formula and ", 
         "no longer any discordant nodes", call. = TRUE)
  }
  if (dat$control$save.nwstats == TRUE) {
    if (at == 2) {
      nwstats <- attributes(dat$nw)$stats
      dat$stats$nwstats <- as.data.frame(nwstats)
    }
  }
  if (anyActive > 0 & dat$control$depend == TRUE) {
    suppressWarnings(dat$nw <- simulate(dat$nw, formation = nwparam$formation, 
                                        dissolution = nwparam$coef.diss$dissolution, coef.form = nwparam$coef.form, 
                                        coef.diss = nwparam$coef.diss$coef.adj, constraints = nwparam$constraints, 
                                        time.start = at, time.slices = 1, time.offset = 0, 
                                        monitor = dat$control$nwstats.formula, control = dat$control$set.control.stergm))
    if (dat$control$save.nwstats == TRUE) {
      dat$stats$nwstats <- rbind(dat$stats$nwstats, tail(attributes(dat$nw)$stats, 
                                                         1))
    }
    if (dat$control$delete.nodes == TRUE) {
      dat$nw <- network.extract(dat$nw, at = at)
      inactive <- which(dat$attr$active == 0)
      aa=try(dat$attr <- deleteAttr(dat$attr, inactive))
      if(class(aa)=="try-error"){browser()}
    }
  }
  return(dat)
}
