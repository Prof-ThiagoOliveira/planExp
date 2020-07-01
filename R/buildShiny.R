#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: buildShiny.R                                                  #
# Contains: buildShiny function                                        #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Dashboard App for the Randomized Complete Block Design (RCBD)
##'
##' @description Constructs and starts a Shiny application from an object
##'   of class \code{planExp}.
##'
##' @usage buildShiny(obj)
##'
##' @param obj an object inheriting from class \code{planExp} used to build
##'   the Shiny app.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##' @examples
##' \dontrun{
##' treatMeans <- c(50, 60, 61, 14, 5, 68, 52)
##' blocks <- paste("Block BB", seq(1, 6, 1))
##' treat <- LETTERS[seq( from = 1, to = 7)]
##' design2 <- rcbdPlan(treat = treat, blocks = blocks,
##'                     treatMeans = treatMeans)
##' buildShiny(design2)
##'}
##' @export

buildShiny <- function(obj) {
  UseMethod("buildShiny", obj)
}
