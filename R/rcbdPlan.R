#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: rcbd.R                                                         #
# Contains: rbdPlan function                                          #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Randomized Complete Block Design (RBD)
##'
##' @description levels of treatment are randomly assigned to the
##'   experimental units within blocks. We assuming that blocks have a
##'   systematic effect on the statistical comparisons among
##'   treatements. Through randomization, every experimental unit within
##'   block has same probability of receiving any treatement. The word
##'   'complete' indicates that each block (group) contains all
##'   treatments.
##'
##' @usage rcbdPlan(treat, blocks, seed)
##'
##' @param treat \code{\link[base]{numeric}} or complex vector
##'   containing treatments levels.
##'
##' @param blocks \code{\link[base]{numeric}} or complex vector
##'   containing blocks levels.
##'
##' @param seed A single \code{\link[base]{numeric}} value, interpreted
##'   as an integer, that specifies the starting value of the random
##'   number generator.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##'
##' @examples
##' ## 3 treatments and 4 blocks
##' rcbdPlan(treat = 3, blocks = 4)
##'
##' @examples
##' ## Running the shiny app
##' treat <- LETTERS[seq( from = 1, to = 10 )]
##' design <- rcbdPlan(treat = treat, block = 6)
##' \dontrun{
##' buildShiny(design)
##'}
##'
##' @examples
##' ## Priori for treatment means
##' blocks <- paste("Block ", seq(1, 6, 1))
##' treat <- LETTERS[seq( from = 1, to = 6 )]
##' design2 <- rcbdPlan(treat = treat, blocks = blocks)
##' \dontrun{
##' buildShiny(design2)
##'}
##'
##' @export

rcbdPlan <- function(treat, blocks, seed = NULL) {
  #---------------------------------------------------------------------
  # Initial checking and dataBuilder
  #---------------------------------------------------------------------
  init <- rcbdCheck(treat = treat, blocks = blocks)
  data <- generator(list(Block = blocks, Treatment = treat))
  #---------------------------------------------------------------------
  # Planning Builder function
  #---------------------------------------------------------------------
  rcbdPlan <- list(
    Design = "Randomized Complete Block Design",
    Plan = rcbdPlanBuilder(data, seed = seed),
    data = data,
    initValues = list(treat = treat, blocks = blocks)
  )
  attr(rcbdPlan, "class") <- "rcbdPlan"
  return(rcbdPlan)
}

#=======================================================================
# Print Method
# ======================================================================
##' @rdname print.rcbdPlan
##' @method print rcbdPlan
##' @title Print an \code{rcbdPlan} object
##' @usage \method{print}{rcbdPlan}(x, digits, ...)
##' @aliases print.rcbdPlan
##' @description Prints information about randomization of Completely
##'   randomized design.
##' @return an object inheriting from class \code{print.rcbdPlan}.
##' @param x an object inheriting from class
##'   \code{\link[planExp]{rcbdPlan}}, representing a randomization for
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
##' @seealso \code{\link[planExp]{rcbdPlan}},
##'   \code{\link[planExp]{buildShiny}}
##' @examples
##' treat <- paste("T", seq(1, 5, 1))
##' blocks <- 6
##' print(rcbdPlan(treat, blocks))
##'
##' @export

print.rcbdPlan <- function(x, digits = NULL, ...) {
  cat(paste0(x$Design))
  cat("\n\n")
  print(x$Plan, digits = digits, ...)
}
