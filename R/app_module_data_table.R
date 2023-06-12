# data table server ----
# @param local_file (usually reactive) function retrieving local file
# @param report_error (usually reactive) function reporting an error
module_data_table_server <- function(
    input, output, session, data_sheet_id, gs_key_file,
    local_file, report_error,
    sheet, cols = dplyr::everything()) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    data = NULL,
    hash = NULL
  )

  # reset ========

  reset <- function() {
    values$data <- NULL
    values$hash <- NULL
  }

  # read data ==========

  # grants data
  # grants_data <- read_data("grants",
  #                          cols = c("grant_id", "name", "status", "speed_type", "pi_user_id", "orderer_user_id"))
  # grants_hash <- digest::digest(grants_data)
  # if (!identical(grants_hash, isolate(values$grants_hash))) {
  #   log_info(ns = ns, "found new grants data")
  #   values$grants_data <- grants_data
  #   values$grants_hash <- grants_hash
  # }
  #

  read_data <- function() {

    # info
    log_debug(ns = ns, "reading data from xlsx file for sheet '", sheet, "'")
    validate(need(file.exists(local_file()), "something went wrong retrieving the data"))
    sheets <- readxl::excel_sheets(local_file())

    # check if sheet exists
    if (!sheet %in% sheets) {
      log_error(ns = ns, user_msg = "Cannot read data", error = sprintf("'%s' tab doesn't exist", sheet))
      report_error()
      return(NULL)
    }

    # try to read data
    data <- tryCatch({
      data <- readxl::read_excel(local_file(), sheet = sheet) |>
        dplyr::select({{ cols }})
      data
    },
    error = function(e) {
      log_error(ns = ns, "data reading failed", user_msg = "Missing data", error = e)
      report_error()
      NULL
    })

    # hash
    hash <- digest::digest(data)
    if (!identical(hash, isolate(values$hash))) {
      log_info(ns = ns, "found new '", sheet, "' data")
      values$data <- data
      values$hash <- hash
    }
  }

  # get data ==========

  get_data <- reactive({
    validate(need(values$data, "something went wrong retrieving the data"))
    return(values$data)
  })


  # module functions ======
  list(
    reset = reset,
    read_data = read_data,
    get_data = get_data
  )

}
