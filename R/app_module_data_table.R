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
      data <- read_excel_sheet(local_file(), sheet = sheet, cols = {{ cols }})
      data
    },
    warning = function(w) {
      log_error(ns = ns, "data reading failed", user_msg = "Data warning", error = w)
      report_error()
      NULL
    },
    error = function(e) {
      log_error(ns = ns, "data reading failed", user_msg = "Missing data", error = e)
      report_error()
      NULL
    })

    # hash
    hash <- data |> hash_data()
    if (!identical(hash, isolate(values$hash))) {
      log_info(ns = ns, "found new '", sheet, "' data")
      values$data <- data
      values$hash <- hash
    }
  }

  # hash data
  hash_data <- function(data) {
    data |>
      dplyr::select(-".add", -".update", -".delete") |>
      digest::digest()
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
