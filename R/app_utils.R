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

# ui elements ======

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}
