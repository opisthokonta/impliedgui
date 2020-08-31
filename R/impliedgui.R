


#' Run the implied GUI
#'
#' Run this function to start the grapichal user interface (GUI) for the implied package. Please refer to the documentation and reference in the implied package for more information.
#'
#' @param ... Passed on to the \code{\link[shiny:runApp]{shiny::runApp}} function.
#'
#' @note
#' This function is based on code from Dean Attali's blog (https://deanattali.com/2015/04/21/r-package-shiny-app/)
#'
#' @export
impliedGUI <- function(...){
  appDir <- system.file("shinyapp", package = "impliedgui")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `impliedgui`.", call. = FALSE)
  }

  shiny::runApp(appDir, ...)
}



