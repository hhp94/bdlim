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
                       ".\n - For details see help(`bdlim-package`) or https://anderwilson.github.io/bdlim/.\n - A vignette is available at https://anderwilson.github.io/bdlim/articles/.",
                       sep="")
    packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
  }
