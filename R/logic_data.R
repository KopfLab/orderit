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
      abort(paste0("google authentication failed"), parent = e)
    }
  )

  # check token
  if (is.null(token_obj))
    abort("google authentication failed with the provided key file")

  googledrive::drive_auth(token = token_obj)

  # check result
  if (
    !(googledrive::drive_has_token() && identical(googledrive::drive_token()$auth_token$hash(), token_obj$hash()))
  )  {
    abort("google authentication failed with the provided key file")
  }

  op <- options(googledrive_quiet = TRUE)
  on.exit(options(op))
  download_to <- paste0(tempfile(), ".xlsx")
  result <- googledrive::drive_download(googledrive::as_id(gs_id), path = download_to)
  if(!file.exists(download_to))
    abort("google sheet download failed")

  return(download_to)
}

# excel file ======

read_excel_sheet <- function(file_path, sheet, cols) {

  # parse cols param
  col_types <- unname(cols)
  col_names <- names(cols)
  no_types = if (is.null(col_names)) seq_along(col_types) else nchar(col_names) == 0
  no_types_vals <- col_types[no_types]
  col_types[no_types] <- "character"
  col_names[no_types] <- no_types_vals
  cols <- col_types |> stats::setNames(col_names)

  # type map
  excel_col_types <- c(
    "character" = "text",
    "datetime" = "date",
    "double" = "numeric",
    "factor" = "text",
    "integer" = "numeric",
    "logical" = "logical",
    "number" = "numeric"
  )

  # safety check
  stopifnot(
    "unsupported column type" = all(col_types %in% names(excel_col_types))
  )

  # read excel sheet
  sheet_cols <- names(readxl::read_excel(file_path, sheet = sheet, n_max = 0))
  sheet_col_types <- excel_col_types[cols[sheet_cols]] |> unname()
  sheet_col_types[is.na(sheet_col_types)] <- "skip"
  data <- readxl::read_excel(
    file_path, sheet = sheet,
    col_types = sheet_col_types
  )

  # parse map
  col_type_parsers <- c(
    "integer" = expr(as.integer),
    "factor" = expr(as.factor)
  )

  # parsers
  parsers <-
    purrr::map2(
      names(cols), col_type_parsers[cols],
      ~if (!is.null(.y)) {
        call2(unname(.y), sym(.x))
      } else {
        call2(expr(identity), sym(.x))
      }
    ) |>
    stats::setNames(names(cols))

  # parse data
  data <- data |>
    dplyr::mutate(!!!parsers)

  # return
  return(data)
}

