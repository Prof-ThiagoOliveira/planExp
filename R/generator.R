#######################################################################
#                                                                     #
# Package: planExp                                                    #
#                                                                     #
# File: generator.R                                                   #
# Contains: gen.list and generator functions                          #
#                                                                     #
# Modified from package dae                                           #
#                                                                     #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

##' @title Internal function to build the data
##' @description This is an internally called function used to build
##'   data. This function was adapted from \code{fac.gen} function of
##'   package dae.
##' @usage NULL
##' @keywords internal
gen.list <- function(x){
  n <- length(x)
  counter <- 1:n
  kfac <- 0
  for(i in counter) {
    if(!(names(x[i]) == "")) {
      kfac <- kfac+1
      if (kfac == 1) {
        fnames <- list(x[[i]])
        names(fnames) <- names(x)[i]
        freps <- 1
      }else {
        knames <- list(x[[i]])
        names(knames) <- names(x)[i]
        fnames <- c(fnames, knames)
        freps <- c(freps, 1)
      }
    }
    else{
      if (kfac == 0) stop("Must start with a factor name - set times argument instead")
      freps[kfac] <- x[[i]]
    }
  }
  return(list(factors = fnames,reps = freps))
}

##' @title Internal function to build the data
##' @description This is an internally called function used to build
##'   data adapted from package from \code{fac.gen} function of
##'   package dae.
##' @usage NULL
##' @keywords internal
generator <- function(x, each=1, times=1) {
  if(!is.list(x))
    stop("x must be a list")
  facs.reps <- gen.list(x)
  fnames <- facs.reps$factors
  freps <- facs.reps$reps
  nfac <- length(fnames)
  levels <- rep(1, times=nfac)
  for (i in 1:nfac)
  { if (is.numeric(fnames[[i]]) | is.character(fnames[[i]]))
    { if (length(fnames[[i]]) == 1)
        if (is.character(fnames[[i]]))
          levels[i] <- 1
        else
          levels[i] <- fnames[[i]]
      else
        levels[i] <- length(fnames[[i]])
    }
    else
    { stop("Levels of factors must be specified using either numeric or character vectors")
    }
  }
  n <- prod(levels)*prod(freps)*each*times
  counter <- nfac:1
  genlist <- vector("list", nfac)
  keach <- each
  for (i in counter)
  { lev <- 1:levels[i]
    keach <- keach*freps[i]
    ktimes <- n/(levels[i]*keach)
    { if (is.numeric(fnames[[i]]))
      { if (length(fnames[[i]]) != 1)
          lev <- fnames[[i]]
        genlist[[i]] <- factor(rep(lev, times=ktimes, each=keach))
      }
      else
      { genlist[[i]] <- factor(rep(lev, times=ktimes, each=keach), labels=fnames[[i]])
      }
    keach <- keach*levels[i]
    }
  }
  genframe <- as.data.frame(genlist)
  names(genframe) <- names(fnames)
 	return(genframe)
}
