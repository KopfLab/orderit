# general utilities ====

`%then%` <- function(a, b) {
  if (is.null(a)) return(b)
  out <- a
  out[sapply(a, is.null) | sapply(a, is.na)] <- b
  return(out)
}

is_dev_mode <- function() {
  return(identical(Sys.getenv("ORDERIT_DEV"), "ON"))
}

# logging utilities ====
# note: fatal and trace are overkill for this app

# call the other log functions instead for clarity in the code
# @param ... toaster parameters
log_any <- function(msg, log_fun, toaster_fun, ns = NULL, toaster = NULL, ...) {
  ns <- if(!is.null(ns)) paste0("[", ns(NULL), "] ") else ""
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
  log_any(msg = paste0(..., collapse = ""), ns = ns, log_fun = rlog::log_debug)
}

# ui elements ======

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}
