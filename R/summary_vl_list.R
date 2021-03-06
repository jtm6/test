#' @export
summary_vl_list <- function(dat,at){
  
if(at==1){  
    if(dat$param$save_vl_list){
        dat$vl_list<-vector('list',length=dat$param$n_steps)
    }else{
      dat$vl_list<-NULL
      return(dat)
    }  
}else{
  if(!dat$param$save_vl_list){return(dat)}
}

  
vl_ix <- which(dat$pop$Status==1 | (dat$pop$Status==-2 & dat$pop$Time_Death==at))
if(length(vl_ix)==0){return(dat)}
  
namesvec<-c("id","vl","cd4","cd4c","time","muts0","muts1","muts2","muts3","muts4","muts5","drug1","drug2",
            "drug3","I","M","L","K65R","M184V","K103N","K103NgEFV","M184VK65R","gTDF","gEFV","drug4")


# Define new sequences to identify viruses with 0, 1, 2, 3, or 4 mutations  
  seq0 <- c(1) #0  mutations
  seq1 <- c(2,3,5,9,17) #1 mutations
  seq2 <- c(4,6,7,10,11,13,18,19,21,25) #2 mutations
  seq3 <- c(8,12,14,15,20,22,23,26,27,29) #3 mutatoins
  seq4 <- c(16,24,28,30,31) #4 mutations
  seq5 <- c(32) #5 mutations

K65R <-  c(2,4,6,8,10,12,14,16, 2+16,4+16,6+16,8+16,10+16,12+16,14+16,16+16) # TDF resistance
M184V <- c(3,4,7,8,11,12,15,16, 3+16,4+16,7+16,8+16,11+16,12+16,15+16,16+16)  # 3TC
K103N <- c(5,6,7,8,13,14,15,16, 5+16,6+16,7+16,8+16,13+16,14+16,15+16,16+16)    # EFV
GenericEFV <- c(9:16,25:32) # Generic EFV mutation 
K103N_gEFV <- c(13,14,15,16, 13+16,14+16,15+16,16+16) # EFV-high
M184V_K65R <-  c(4,8,12,16, 4+16,8+16,12+16,16+16)# 3TC high / TDF high
GenericTDF <- c(17:32)

dat$vl_list[[at]] <- cbind(dat$pop$id[vl_ix], #Agent
                        dat$pop$V[vl_ix], #VL
                        dat$pop$CD4[vl_ix],#CD4
                        dat$pop$CD4count[vl_ix]/200, # CD4c
                        at, #Time
                        dat$pop$V_vec[vl_ix,seq0], # Muts0
                        rowSums(dat$pop$V_vec[vl_ix,seq1,drop=F]), # Muts1
                        rowSums(dat$pop$V_vec[vl_ix,seq2,drop=F]), # Muts2
                        rowSums(dat$pop$V_vec[vl_ix,seq3,drop=F]), # Muts3
                        rowSums(dat$pop$V_vec[vl_ix,seq4,drop=F]), # Muts4
                        dat$pop$V_vec[vl_ix,seq5], # Muts5
                        dat$pop$Drug1[vl_ix], #D1
                        dat$pop$Drug2[vl_ix], #D2
                        dat$pop$Drug3[vl_ix], #D3
                        rowSums(dat$pop$I_vec[vl_ix,,drop=F]), #I
                        rowSums(dat$pop$M_vec[vl_ix,,drop=F]), #M
                        rowSums(dat$pop$L_vec[vl_ix,,drop=F]), #L
                        rowSums(dat$pop$V_vec[vl_ix,K65R,drop=F])/dat$pop$V[vl_ix],#K65R
                        rowSums(dat$pop$V_vec[vl_ix,M184V,drop=F])/dat$pop$V[vl_ix], #M184V
                        rowSums(dat$pop$V_vec[vl_ix,K103N,drop=F])/dat$pop$V[vl_ix], #K103N
                        rowSums(dat$pop$V_vec[vl_ix,K103N_gEFV,drop=F])/dat$pop$V[vl_ix], # K103N + Generic EFV (e.g. G190A, Y181C)
                        rowSums(dat$pop$V_vec[vl_ix,M184V_K65R,drop=F])/dat$pop$V[vl_ix], #M184VK65R double mut
                        rowSums(dat$pop$V_vec[vl_ix,GenericTDF,drop=F])/dat$pop$V[vl_ix], 
                        rowSums(dat$pop$V_vec[vl_ix,GenericEFV,drop=F])/dat$pop$V[vl_ix],
                        dat$pop$Drug4[vl_ix]) # Second line therapy

  colnames(dat$vl_list[[at]]) <- namesvec
  
  return(dat)
}#end fxn

  
 
    # Notes about numbers of mutations
#   Num binary   Muts
#     1  00000  - 0
#     2  00001  - 1
#     3  00010  - 1
#     4  00011  - 2
#     5  00100  - 1
#     6  00101  - 2
#     7  00110  - 2
#     8  00111  - 3
#     9  01000  - 1
#    10  01001  - 2
#    11  01010  - 2
#    12  01011  - 3
#    13  01100  - 2
#    14  01101  - 3
#    15  01110  - 3
#    16  01111  - 4
#    17  10000  - 1
#    18  10001  - 2
#    19  10010  - 2
#    20  10011  - 3
#    21  10100  - 2
#    22  10101  - 3
#    23  10110  - 3
#    24  10111  - 4
#    25  11000  - 2
#    26  11001  - 3
#    27  11010  - 3
#    28  11011  - 4
#    29  11100  - 3
#    30  11101  - 4
#    31  11110  - 4
#    32  11111  - 5
