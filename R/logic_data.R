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
      cli::cli_abort(paste0("google authentication failed: ", e$message))
    }
  )

  # check token
  if (is.null(token_obj))
    cli::cli_abort("google authentication failed with the provided key file")

  googledrive::drive_auth(token = token_obj)

  # check result
  if (
    !(googledrive::drive_has_token() && identical(googledrive::drive_token()$auth_token$hash(), token_obj$hash()))
  )  {
    cli::cli_abort("google authentication failed with the provided key file")
  }

  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  download_to <- paste0(tempfile(), ".xlsx")
  result <- googledrive::drive_download(googledrive::as_id(gs_id), path = download_to)
  if(!file.exists(download_to))
    cli::cli_abort("google sheet download failed")

  return(download_to)
}

