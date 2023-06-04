# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_server <- function(input, output, session, data_sheet_id, data_folder_id, gs_key_file, user_id) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    refresh_data = NULL,
    has_data = FALSE
  )

  # refresh data
  refresh_data <- function() {
    values$refresh_data <- if(is.null(values$refresh_data)) 1L else values$refresh_data + 1L
  }

  # download data
  download_data <- reactive({
    values$refresh_data
    module_message(ns, "info", "requesting google spreadsheet data")
    file_path <- download_google_sheet(data_sheet_id, gs_key_file = gs_key_file)
    #file_path <- "local_data.xlsx" # dev only
    module_message(ns, "info", "downloaded google spreadsheet data")
    #stop("something went horribly wrong", call. = FALSE)
    isolate(values$has_data <- TRUE)
    return(file_path)
  })

  # lock/unlock app while loading data
  data_loading_modal <- modalDialog(
    title = h1("Loading data..."),
    h4("Please wait while the data is being loaded."),
    footer = NULL
  )
  observeEvent(values$has_data, {
    if (!values$has_data) showModal(data_loading_modal)
  }, priority = 1000L)
  observeEvent(values$has_data, {
    if (values$has_data) removeModal()
  }, priority = -1000L)

  # data functions
  get_test_data <- reactive({
    readxl::read_excel(download_data(), sheet = "test")
  })
  get_users_data <- reactive({
    req(download_data())
    validate(need(file.exists(download_data()), "something went wrong retrieving the data"))
    readxl::read_excel(download_data(), sheet = "users")
  })

  #  LINKS functions ====
  list(
    refresh_data = refresh_data,
    get_test_data = get_test_data,
    get_users_data = get_users_data
  )
}
