#' gets the current screen width.
#'
#' It's highly OS specific, I do ignore if it works on OSX

getConsoleWidth <- function() {
  width <- getOption("width", NULL)
  if(is.numeric(width)) {
    width
  } else {
    os <- .Platform$OS.type  
    if ( os  %in% c("unix", "Darwin" )) {
      as.numeric(system('tput cols', intern=TRUE))
    } else {
      return(tryCatch({
        txt <- system('cmd /c "mode con /status | grep  \"Colonne:\"',
                      intern=TRUE)
        txt <- unlist(strsplit(txt, ":"))[2]
        as.numeric(txt)
      }, error = function (err) {
        ## please God forgive me
        80
      }))
    }
  }
}


setOldClass("tkProgressBar")

#' Classe S4 per ProgressBar
#'
#' @name ProgressBar
#' @rdname ProgressBar
#' @aliases ProgressBar-class
#' @title ProgressBar with labels
#' @slot value current value 
#' @slot min minimum value
#' @slot max maximum value
#' @slot char char to use as token for progress
#' @slot width current screen width, autoevaluated
#' @slot time time since the beginning of ProgressBar
#' @exportClass ProgressBar
#' @export ProgressBar
#' @importFrom rstudioapi isAvailable 
#' @importFrom tcltk tkProgressBar
#' @importFrom methods getClass setClass

ProgressBar <- setClass(
  "ProgressBar",
  representation(
    value= "numeric",
    min="numeric",                        
    max="numeric",
    char="character",
    width="numeric",
    isrstudio="logical",
    tkp="tkProgressBar",
    time="POSIXct"))

setMethod(
  "initialize",
  signature("ProgressBar"),
  function(.Object, min=0, max=1, char="=") {
    .Object@min <- min
    .Object@max <- max
    .Object@char <- char
    .Object@width <- getConsoleWidth()
    .Object@time <- Sys.time()
    .Object@isrstudio <- rstudioapi::isAvailable()
    if(.Object@isrstudio) {
      .Object@tkp <- tkProgressBar(min=min, max=max)
    }
    .Object
  })

#' Kills current ProgressBar.
#'
#' @name kill
#' @usage kill(x)
#' @param x `ProgressBar` instance
#' @export
#' @docType methods
#' @rdname kill-methods

setGeneric(
  "kill",
  function(x){
    standardGeneric("kill")
  })


#' Kills current ProgressBar
#'
#' @name kill
#' @usage kill(x)
#' @param x `ProgressBar` instance
#' @export
#' @aliases kill,ProgressBar-method

setMethod(
  "kill",
  signature("ProgressBar"),
  function(x) {
    if(x@isrstudio) {
      close(x@tkp)
    } else {
      cat("\n", file = stderr())
      flush.console()
    }
  })

#' Updates `ProgressBar` with `value`
#'
#' `value` has to `min<= value <= max` with `min` and `max` values
#' of the slots
#'
#' `ProgressBar` tries to evaluate an ETA and prints it.
#' 
#' @name updateProgressBar
#' @usage updateProgressBar(x, value, label)
#' @param x `ProgressBar` instance
#' @param value current state of the `ProgressBar` to be updated
#' @param label optional label to be printed with the `ProgressBar`, defaults
#'        to empty string ("")
#' @docType methods
#' @rdname updateProgressBar-methods
#' @export

setGeneric(
  "updateProgressBar",
  function(x, value, label="") {
    standardGeneric("updateProgressBar")
  })


#' Updates `ProgressBar` with `value`
#'
#' `value` has to `min<= value <= max` with `min` and `max` values
#' of the slots
#'
#' `ProgressBar` tries to evaluate an ETA and prints it.
#' 
#' @name .update
#' @usage .update(x, value, label)
#' @param x `ProgressBar` instance
#' @param value current state of the `ProgressBar` to be updated
#' @param label optional label to be printed with the `ProgressBar`, defaults
#'        to empty string ("")
#' @docType methods
#' @rdname update-methods
#' @importFrom tcltk setTkProgressBar
#' @export


.update <- function(x, value, label="") {
  isrstudio <- x@isrstudio
  x@value  <- value
  min <- x@min
  if(value == min) {
    x@time <- Sys.time()
  }    
  max <- x@max
  char <- x@char
  elapsed <- as.numeric(difftime(Sys.time(),  x@time, units="secs"))
  V <- value/elapsed
  eta <- (max - value) / V
  eta <- if(value == min) {
    "--:--"
  } else if(eta > 3600) {
    sprintf("%02i:%02i:%02i", as.integer(floor(eta/3600)),
            as.integer(floor((eta/60) %% 60)),
            as.integer(floor(eta %% 60)))
  } else {
    sprintf("%02i:%02i", as.integer(floor((eta/60) %% 60)),
            as.integer(floor(eta %% 60)))
  }

  if(isrstudio) {
    setTkProgressBar(x@tkp, value, title=eta, label=label)
    return(invisible(x))
  }
  
  if (!is.finite(value) || value < min || value > max)
    return()
  
  nw <- nchar(char,"w")
  pad <- 12 + nchar(eta)
  nlabel <- nchar(label)
  width <- trunc(x@width/nw) - pad - nlabel
  nb <- round(width * (value - min)/(max - min))
  pc <- round(100 * (value - min)/(max - min))
  if(nlabel > 0) {
    cat(paste(c("\r |", rep.int(char, nb),
                  rep.int(" ", nw * (width - nb)),
                sprintf("| %3d%% - %s %s", pc, label, eta)), collapse = ""),
        file = stderr())
  } else {
    cat(paste(c("\r |", rep.int(char, nb),
                rep.int(" ", nw * (width - nb)),
                sprintf("| %3d%% %s", pc, eta)), collapse = ""),
        file = stderr())
    
  }
  flush.console()
  invisible(x)  
}


#' Updates `ProgressBar` with `value`
#'
#' `value` has to `min<= value <= max` with `min` and `max` values
#' of the slots
#'
#' `ProgressBar` tries to evaluate an ETA and prints it.
#' 
#' @name updateProgressBar
#' @usage updateProgressBar(x, value)
#' @usage updateProgressBar(x, value, label)
#' @param x `ProgressBar` instance
#' @param value current state of the `ProgressBar` to be updated
#' @param label optional label to be printed with the `ProgressBar`, defaults
#'        to empty string ("")
#' @export
#' @rdname updateProgressBar

setMethod(
  "updateProgressBar",
  signature("ProgressBar", "ANY"),
  function(x, value, label="") {
    .update(x, value, label)
  })
