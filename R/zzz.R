###
### R routines for the R package bdlim (c)
#
#' @importFrom utils packageDescription

.onAttach <-
  function(lib, pkg) {
    #
    ################################################################################
    #
    meta <- packageDescription("bdlim")
    attachmsg <- paste("This is bdlim ",meta$Version,
                       ".\nFor details: help(`bdlim-package`), vignette('bdlim'), or https://anderwilson.github.io/bdlim/",
                       sep="")
    packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
  }
