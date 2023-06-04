# google sheet ======

download_google_sheet <- function(gs_id, gs_key_file = "gdrive_access_key.json") {
  # google drive authentication
  googledrive::drive_deauth()

  # authenticate with key file
  tryCatch(
    token_obj <- gargle::credentials_service_account(
      path = gs_key_file,
      scopes = c(
        "https://www.googleapis.com/auth/spreadsheets", # full access to sheets
        "https://www.googleapis.com/auth/drive" # full access to drive files
      )
    ),
    error = function(e) {
      sprintf("google authentication failed: %s. Please try again.", e$message) |>
        stop(call. = FALSE)
    }
  )

  # check token
  if (is.null(token_obj))
    stop("google authentication failed with the provided key file", call. = FALSE)
  googledrive::drive_auth(token = token_obj)

  # check result
  if (
    !(googledrive::drive_has_token() && identical(googledrive::drive_token()$auth_token$hash(), token_obj$hash()))
  )  {
    stop("google authentication failed with the provided key file", call. = FALSE)
  }

  #op <- options(googledrive_quiet = TRUE)
  #on.exit(options(op))
  download_to <- paste0(tempfile(), ".xlsx")
  result <- googledrive::drive_download(googledrive::as_id(gs_id), path = download_to)
  if(!file.exists(download_to))
    stop("google sheet download failed", call. = FALSE)
  return(download_to)
}

