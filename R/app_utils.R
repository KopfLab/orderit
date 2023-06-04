# general utilities ====

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# gui settings ====

# set package settings
set_pkg_settings <- function(
    log = TRUE,
    debug = FALSE,
    reset_all = FALSE
) {

  # set settings that user supplies
  if (!missing(log) | reset_all)
    set_setting("log", log)
  if (!missing(debug) | reset_all)
    set_setting("debug", debug)

  # return all settings invisibly
  return(invisible(get_pkg_settings()))
}

# get all package settings
get_pkg_settings <- function(pattern = NULL) {
  all_opts <- options()
  pkg_pattern <- sprintf("^%s", get_pkg_settings_prefix())
  pkg_options <- all_opts[grepl(pkg_pattern, names(all_opts))]
  pkg_options <- pkg_options |> setNames(gsub(pkg_pattern, "", names(pkg_options)))
  if (!is.null(pattern)) {
    pkg_options <- pkg_options[grepl(pattern, names(pkg_options))]
  }
  return(pkg_options)
}

# initialize default settings on package loading (internal function)
# this is run once when the first package function is called (even with :::)
.onLoad <- function(libname, pkgname) {
  set_pkg_settings(reset_all = TRUE)
}

# package settings prefix
get_pkg_settings_prefix <- function() {
  "orderit."
}

# retrieve package settings (internal function)
setting <- function(name) {
  # check R options
  value <- getOption(paste0(get_pkg_settings_prefix(), name))
  if (is.null(value)) {
    # setting doesn't exist
    stop("setting '", name, "' does not exist", call. = FALSE)
  }
  # return default value
  return(value)
}

# set package setting (internal function)
set_setting <- function(name, value) {
  options(list(value) |> setNames(paste0(get_pkg_settings_prefix(), name)))
  return(invisible(value))
}


# logging ====
# note: fatal and trace are overkill for this app

# call the other log functions instead for clarity in the code
# @param ... toaster parameters
log_any <- function(msg, log_fun, toaster_fun, ns = NULL, toaster = NULL, ...) {
  ns <- if(!is.null(ns)) paste0("[NS ", ns(NULL), "] ") else ""
  if(!is.null(toaster)) {
    log_fun(paste0(ns, msg, " [GUI msg: '", toaster, "']", collapse = ""))
    toaster_fun(toaster, position = "bottom-right", newestOnTop = TRUE, ...)
  } else {
    log_fun(paste0(ns, msg, collapse = ""))
  }
}

log_error <- function(..., ns = NULL, user_msg = NULL, error = NULL) {
  error_msg <- if (!is.null(error)) paste0(": ", cli::ansi_strip(error)) else ""
  log_any(
    msg = paste0(..., error_msg, collapse = ""), ns = ns,
    log_fun = rlog::log_error,
    toaster_fun = shinytoastr::toastr_error,
    toaster = paste0("Please try again. If the issue persists, report this error", error_msg),
    title = user_msg,
    timeOut = 10000,
    closeButton = TRUE
  )
}

log_warning <- function(..., ns = NULL, user_msg = NULL, warning = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_error,
    toaster_fun = shinytoastr::toastr_error,
    toaster = if (!is.null(warning)) cli::ansi_strip(warning) else user_msg,
    title = if (!is.null(warning)) user_msg else NULL,
    progressBar = TRUE,
    extendedTimeOut = 3000
  )
}

log_info <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_info,
    toaster_fun = shinytoastr::toastr_info,
    toaster = user_msg
  )
}

log_success <- function(..., ns = NULL, user_msg = NULL) {
  log_any(
    msg = paste0(..., collapse = ""), ns = ns,
    log_fun = rlog::log_info,
    toaster_fun = shinytoastr::toastr_success,
    toaster = user_msg
  )
}

log_debug <- function(..., ns = NULL) {
  log_any(msg = paste0(..., collapse = ""), ns = ns, func = rlog::log_debug)
}

# display gui message
gui_message <- function(type = c("info", "debug"), ...) {
  module_message(ns = NULL, type = type, ...)
}

# display module message
# @param type if this is an info meessage or debug (debug only shows if in debug mode)
# @param type if this is an info message or debug (debug only shows if in debug mode)
module_message <- function(ns, type = c("info", "debug"), ...) {
  if (!setting("log") || (type == "debug" && !setting("debug"))) return()
  prefix <- if(type == "info") "INFO" else if (type == "debug") "DEBUG" else stop("don't know message type", type)
  ns <- if(!is.null(ns)) paste0(" (NS '", ns(NULL), "')") else ""
  cat(file=stderr(), prefix, ns, ": ", ..., "\n", sep = "")
}

# error catching ====



# ui elements ======

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}
