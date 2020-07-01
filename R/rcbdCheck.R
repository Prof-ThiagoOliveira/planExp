#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: rcbdCheck.R                                                   #
# Contains: rcbdCheck function                                        #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2020, Thiago P. Oliveira                              #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal Function to check the arguments
##' @description This is an internally called function to get an initial
##'   check
##' @usage NULL
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@usp.br}
##' @keywords internal

rcbdCheck <- function(treat, blocks) {
  #---------------------------------------------------------------------
  # Initial check
  #---------------------------------------------------------------------
  check.integer <- function(N){
    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
  }
  #---------------------------------------------------------------------
  # Checking treatments
  #---------------------------------------------------------------------
  if (is.numeric(treat) && length(treat) == 1 && (treat <= 0 ||
                               check.integer(treat) == FALSE)) {
    stop("'treat' must be coercible to positive integers", call.= FALSE)
  }

  #---------------------------------------------------------------------
  # Checking blocks
  #---------------------------------------------------------------------
  if (is.numeric(blocks) && length(blocks) == 1 && (blocks <= 0 ||
                                check.integer(blocks) == FALSE)) {
    stop("'blocks' must be coercible to positive integers",
         call.= FALSE)
  }
}
