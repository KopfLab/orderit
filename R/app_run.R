#' Run the user interface
#'
#' @param data_sheet_id google data sheet id
#' @param data_folder_id google drive folder id
#' @param gs_key_file in json format
#' @param timezone the timezone to user for datetime calculations
#' @inheritParams shiny::shinyApp
#' @export
start_app <- function(
  data_sheet_id,
  data_folder_id,
  gs_key_file,
  user_id,
  user_is_admin,
  user_groups,
  user_name,
  timezone,
  options = list(),
  uiPattern = "/",
  enableBookmarking = "url"
) {
  # startup
  log_info("\n\n========================================================")
  log_info(
    "starting orderit GUI",
    if (shiny::in_devmode()) " in DEV mode"
  )

  # safety check for parameters
  stopifnot(
    "'data_sheet_id' required" = !missing(data_sheet_id) &&
      nchar(data_sheet_id) > 0,
    "'data_folder_id' required" = !missing(data_folder_id) &&
      nchar(data_folder_id) > 0,
    "'gs_key_file' required" = !missing(gs_key_file) &&
      nchar(gs_key_file) > 0 &&
      file.exists(gs_key_file),
    "'timezone' required" = !missing(timezone) &&
      timezone %in% base::OlsonNames(),
    "'user_id' required" = !missing(user_id) && nchar(user_id) > 0,
    "'user_is_admin' required" = !missing(user_is_admin) &&
      rlang::is_scalar_logical(user_is_admin),
    "'user_groups' required" = !missing(user_groups),
    "'user_name' required" = !missing(user_name) && nchar(user_name) > 0
  )

  # us and server
  ui <- ui(user = user_name, timezone = timezone)
  server <- server(
    data_sheet_id = data_sheet_id,
    data_folder_id = data_folder_id,
    gs_key_file = gs_key_file,
    timezone = timezone,
    user_id = user_id,
    user_is_admin = user_is_admin,
    user_groups = user_groups
  )

  # generate app
  shinyApp(
    ui = ui,
    server = server,
    options = options,
    enableBookmarking = enableBookmarking,
    uiPattern = uiPattern
  )
}
