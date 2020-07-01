#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: crdPlanBuilder.R                                              #
# Contains: crdPlanBuilder function                                   #
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

crdPlanBuilder <- function(data, seed) {
  if (!is.null(seed)) set.seed(seed)
  #---------------------------------------------------------------------
  # Treatments and Number of Observations
  #---------------------------------------------------------------------
  Plan <- data[sample(nrow(data), nrow(data)), ]
  rownames(Plan) <- NULL
  data_plan <- data.frame(Units = seq(1, nrow(Plan), 1),
                          Treatment = Plan$Treatment,
                          Replicate = Plan$Replicate)
  return(data_plan)
}
