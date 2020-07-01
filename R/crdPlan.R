#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: crd.R                                                         #
# Contains: crdPlan function                                          #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Completely randomized design (CRD)
##'
##' @description levels of treatment are randomly assigned to the
##'   experimental units. Through randomization, every experimental unit
##'   has same probability of receiving any treatment. This
##'   provides a basis for making a valid estimate of random
##'   error which is so essential in testing of significance of
##'   genuine differences
##'
##' @usage crdPlan(treat, rep, seed)
##'
##' @param treat \code{\link[base]{numeric}} or complex vector
##'   containing treatments levels.
##'
##' @param rep \code{\link[base]{numeric}} or complex vector containing
##'   thenumber of
##'   replications by treatment.
##'
##' @param seed A single \code{\link[base]{numeric}} value, interpreted
##'   as an integer, that specifies the starting value of the random
##'   number generator.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##'
##' @examples
##' ## 3 treatments with same number of replicates
##' crdPlan(treat = 3, rep = 4)
##'
##' @examples
##' ## 4 treatments with different number of replicates
##' crdPlan(treat = 4, rep = c(rep(4, 3), 6))
##'
##' @examples
##' ## Running the shiny app
##' treat <- LETTERS[seq( from = 1, to = 10 )]
##' design <- crdPlan(treat = treat, rep = 6)
##' \dontrun{
##' buildShiny(design)
##'}
##'
##' @examples
##' ## Treatment with different replicates
##' design2 <- crdPlan(treat = paste("T", seq(1, 7, 1)),
##'                    rep = c(rep(6, 4),rep(7, 3)))
##' \dontrun{
##' buildShiny(design2)
##'}
##'
##' @export

crdPlan <- function(treat, rep, seed = NULL) {
  #---------------------------------------------------------------------
  # Initial checking and dataBuilder
  #---------------------------------------------------------------------
  tol <- 1e-5
  if (length(rep) == 1 || abs(max(rep) - min(rep)) < tol) {
    data <- generator(x = list(Treatment = treat,  Replicate = rep))
  }else {
    all_treat <- generator(x = list(Treatment = treat))
    if (all(treat == length(rep)) || length(treat) == length(rep)) {
      counter <- 1:length(rep)
      n.data <- list(NA)
      for (i in counter) {
        n.data[[i]] <-
          generator(x = list(Treatment = as.character(all_treat[i, 1]),
                             Replicate = seq(1, rep[i], 1)))
      }
      data <- do.call(rbind.data.frame, n.data)
    }else {
      stop("'trat' has length ", nrow(all_treat), " while 'rep' has length ",
           length(rep), ". They must be same length.", call.= FALSE)
    }
  }
  #---------------------------------------------------------------------
  # Planning Builder function
  #---------------------------------------------------------------------
  crdPlan <- list(
    Design = "Completely randomized design",
    Plan = crdPlanBuilder(data, seed = seed),
    data = data,
    initValues = list(treat = treat, rep = rep)
  )
  attr(crdPlan, "class") <- "crdPlan"
  return(crdPlan)
}

#=======================================================================
# Print Method
# ======================================================================
##' @rdname print.crdPlan
##' @method print crdPlan
##' @title Print an \code{crdPlan} object
##' @usage \method{print}{crdPlan}(x, digits, ...)
##' @aliases print.crdPlan
##' @description Prints information about randomization of Completely
##'   randomized design.
##' @return an object inheriting from class \code{print.crdPlan}.
##' @param x an object inheriting from class
##'   \code{\link[planExp]{crdPlan}}, representing a randomization for
##'   Completely randomized design.
##'
##' @param digits a non-null value for \code{digits} specifies the
##'   minimum number of significant digits to be printed in
##'   values. Default \code{NULL}.
##'
##' @param ... further arguments passed to \code{{\link{print}}}.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##' @seealso \code{\link[planExp]{crdPlan}},
##'   \code{\link[planExp]{buildShiny}}
##' @examples
##' treat <- LETTERS[seq( from = 1, to = 10 )]
##' rep <- rep(6, 10)
##' print(crdPlan(treat = treat, rep=rep))
##' @export

print.crdPlan <- function(x, digits = NULL, ...) {
  cat(paste0(x$Design))
  cat("\n\n")
  print(x$Plan, digits = digits, ...)
}
