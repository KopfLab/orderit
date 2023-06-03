#' Run the user interface
#'
#' @param port the localhost port where the app will be accessible, e.g. https://127.0.0.1:5000 (note that if it is a port that is open in your firewall such as 3838, the GUI will be accessible on your local network at your IP address https://xxx.xxx.xxx.xxx:3838)
#' @export
start_app <- function(log = TRUE, debug = FALSE, port = 5000) {
  start_gui(
    launch = TRUE,
    log = log,
    debug = debug,
    port = port
  )
}

#' Run the user interface on a server
#'
#' @export
start_app_server <- function(log = TRUE, debug = TRUE) {
  start_gui(
    launch = FALSE,
    log = log,
    debug = debug
  )
}

# start gui
# @param ... parameters passed on to runApp
start_gui <- function(launch, log, debug, ...) {

  # safety check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # safety check for parameters
  stopifnot(!missing(launch))
  stopifnot(!missing(log))
  stopifnot(!missing(debug))

  # set settings
  set_pkg_settings(log = log, debug = debug)

  # generate app
  app <- shinyApp(ui = ui(), server = server())

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}
