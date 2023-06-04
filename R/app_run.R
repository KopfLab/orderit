#' Run the user interface
#'
#' @param data_sheet_id google data sheet id
#' @param data_folder_id google drive folder id
#' @param gs_key_file in json format
#' @param port the localhost port where the app will be accessible, e.g. https://127.0.0.1:5000 (note that if it is a port that is open in your firewall such as 3838, the GUI will be accessible on your local network at your IP address https://xxx.xxx.xxx.xxx:3838)
#' @export
start_app <- function(data_sheet_id, data_folder_id, gs_key_file, timezone = Sys.timezone(), user_id, log = TRUE, debug = FALSE, port = 5000) {
  start_gui(
    data_sheet_id = data_sheet_id,
    data_folder_id = data_folder_id,
    gs_key_file = gs_key_file,
    timezone = timezone,
    user_id = user_id,
    launch = TRUE,
    log = log,
    debug = debug,
    dev = FALSE,
    port = port
  )
}

#' Run the user interface on a server
#' @inheritParams start_app
#' @export
start_app_server <- function(data_sheet_id, data_folder_id, gs_key_file, timezone, user_id = Sys.getenv("SHINYPROXY_USERNAME"), log = TRUE, debug = TRUE) {
  start_gui(
    data_sheet_id = data_sheet_id,
    data_folder_id = data_folder_id,
    gs_key_file = gs_key_file,
    timezone = timezone,
    user_id = user_id,
    launch = FALSE,
    log = log,
    debug = debug,
    dev = FALSE
  )
}

# start gui
# @param ... parameters passed on to runApp
start_gui <- function(data_sheet_id, data_folder_id, gs_key_file, timezone, user_id, launch, log, debug, dev, ...) {

  # safety check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  # safety check for parameters
  stopifnot(!missing(data_sheet_id) && nchar(data_sheet_id) > 0)
  stopifnot(!missing(data_folder_id) && nchar(data_folder_id) > 0)
  stopifnot(!missing(gs_key_file) && nchar(gs_key_file) > 0)
  stopifnot(!missing(timezone) && timezone %in% base::OlsonNames())
  stopifnot(!missing(user_id) && nchar(user_id) > 0)
  stopifnot(!missing(launch))
  stopifnot(!missing(log))
  stopifnot(!missing(debug))
  stopifnot(!missing(dev))
  stopifnot(file.exists(gs_key_file))

  # set settings
  if (debug) Sys.setenv("LOG_LEVEL" = "DEBUG")
  else if (log) Sys.setenv("LOG_LEVEL" = "INFO")
  else Sys.setenv("LOG_LEVEL" = "WARN")
  if (dev) Sys.setenv("ORDERIT_DEV" = "ON")
  else Sys.setenv("ORDERIT_DEV" = "OFF")

  # generate app
  app <- shinyApp(
    ui = ui(timezone = timezone),
    server = server(
      data_sheet_id = data_sheet_id,
      data_folder_id = data_folder_id,
      gs_key_file = gs_key_file,
      timezone,
      user_id = user_id
    )
  )

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}

# internal function to start app in development mode
start_app_dev <- function() {
  stopifnot(file.exists("gdrive_file_key.txt"))
  stopifnot(file.exists("gdrive_access_key.json"))
  start_gui(
    data_sheet_id = readLines("gdrive_file_key.txt")[1],
    data_folder_id = readLines("gdrive_file_key.txt")[2],
    gs_key_file = "gdrive_access_key.json",
    timezone = Sys.timezone(),
    user_id = "dev",
    launch = TRUE,
    log = TRUE,
    debug = TRUE,
    dev = TRUE,
    port = 1234
  )
}
