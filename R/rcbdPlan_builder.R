#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: rcbdPlanBuilder.R                                              #
# Contains: rcbdPlanBuilder function                                   #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to build the dashborad app
##' @description This is an internally called function used to build the
##'   dashboard app
##' @usage NULL
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##' @keywords internal

rcbdPlanBuilder <- function(data, seed) {
  if (!is.null(seed)) set.seed(seed)
  #---------------------------------------------------------------------
  # Treatments and Number of Observations
  #---------------------------------------------------------------------
  Trat_Bloco <- list(NA)
  for(i in 1:length(unique(data$Block))){
    Trat_Bloco[[i]] <- sample(unique(data$Treatment),
                              length(unique(data$Treatment)))
  }
  rand_treat <- unlist(Trat_Bloco)
  units <- rep(seq(1, length(unique(data$Treatment)), 1),
               length(unique(data$Block)))
  data_plan <- data.frame(Units = units,
                          Block = data$Block,
                          Treatment = rand_treat)
  return(data_plan)
}
