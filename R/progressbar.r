getConsoleWidth <- function() {
  if ( .Platform$OS.type == "unix") {
    return(as.numeric(system('tput cols', intern=TRUE)))
  } else {
    return(tryCatch({
      txt <- system('cmd /c "mode con /status | grep  \"Colonne:\"',
                    intern=TRUE)
      txt <- unlist(strsplit(txt, ":"))[2]
      return(as.numeric(txt))
    }, error = function (err) {
      return(80)
    }))
  }
}

#' Classe S4 per ProgressBar
#'
#' @name ProgressBar
#' @rdname ProgressBar
#' @aliases ProgressBar-class
#' @title ProgressBar with labels
#' @param ... don't know if used
#' @slot value current value 
#' @slot min minimum value
#' @slot max maximum value
#' @slot char char to use as token for progress
#' @exportClass ProgressBar
#' @export ProgressBar
#' @import methods

ProgressBar <- setClass(
  "ProgressBar",
  representation(
    value= "numeric",
    min="numeric",                        
    max="numeric",
    char="character",
    width="numeric",
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
    return(.Object)
  })

setGeneric(
  "kill",
  function(x){
    standardGeneric("kill")
  })

#' Kills current ProgressBar.
#'
#' @name kill
#' @usage kill(x)
#' @param x `ProgressBar` instance
#' @export

setMethod(
  "kill",
  signature("ProgressBar"),
  function(x) {
    cat("\n", file = stderr())
    flush.console()
  })

setGeneric(
  "update",
  function(x, value, label="") {
    standardGeneric("update")
  })

#' Updates `ProgressBar` with `value`
#'
#' `value` has to `min<= value <= max` with `min` and `max` values
#' of the slots
#'
#' `ProgressBar` tries to evaluate an ETA and prints it.
#' 
#' @name update
#' @usage update(x, value)
#' @usage upodate(x, value label)
#' @param x `ProgressBar` instance
#' @param value current state of the `ProgressBar` to be updated
#' @param label optional label to be printed with the `ProgressBar`, defaults
#'        to empty string ("")
#' @export

setMethod(
  "update",
  signature("ProgressBar", "ANY"),
  function(x, value, label="") {
    x@value  <- value
    min <- x@min
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
  })
