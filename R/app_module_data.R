# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_server <- function(input, output, session, data_sheet_id, data_folder_id, gs_key_file, user_id) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    reload_data = NULL
  )

  # download data
  download_data <- reactive({
    values$reload_data

    # block screen
    #showModal(data_loading_modal)
    log_info(ns = ns, "requesting google spreadsheet data", user_msg = "Loading data. Please wait.")

    out <-
      tryCatch({
        #file_path <- "local_data.xlsx"
        file_path <- download_google_sheet(data_sheet_id, gs_key_file = gs_key_file)
        log_success(ns = ns, "downloaded google spreadsheet data", user_msg = "Data loaded.")
        return(file_path)
      },
      error = function(e) {
        log_error(ns = ns, "download failed", user_msg = "Data loading error", error = e)
        NULL
      })

    # remove modal
    removeModal()

    return(out)
  })

  # reload data
  reload_data <- function() {
    values$reload_data <- if(is.null(values$reload_data)) 1L else values$reload_data + 1L
  }
  observeEvent(input$reload, reload_data())

  # data read functions
  read_data <- function(sheet, cols = dplyr::everything()) {
    req(download_data())
    validate(need(file.exists(download_data()), "something went wrong retrieving the data"))
    log_info(ns = ns, "reading ", sheet, " data")
    sheets <- readxl::excel_sheets(download_data())
    if (!sheet %in% sheets) {
      log_error(ns = ns, user_msg = "Cannot read data", error = sprintf("'%s' tab doesn't exist", sheet))
      return(NULL)
    }

    out <-
      tryCatch({
        data <- readxl::read_excel(download_data(), sheet = sheet) |>
          dplyr::select({{ cols }})
        return(data)
      },
      error = function(e) {
        log_error(ns = ns, "data reading failed", user_msg = "Data missing", error = e)
        NULL
      })

    return(out)
  }
  get_users_data <- reactive({ read_data("users", cols = c("user_id", "first_name", "last_name", "role")) })
  get_grants_data <- reactive({ read_data("grants") })

  # authentication functions
  get_user <- reactive({
    req(get_users_data())
    log_info(ns = ns, "authenticating ", user_id)
    out <-
      tryCatch({
        user <- get_users_data() |>
          dplyr::filter(user_id == !!user_id)
        if(nrow(user) < 1L) cli::cli_abort(paste0("user does not exist: ", user_id))
        if(nrow(user) > 1L) cli::cli_abort(paste0("user_id is not unique: ", user_id))
        log_success(ns = ns, "login complete", user_msg = "User login complete.")
        return(user)
      },
      error = function(e) {
        log_error(ns = ns, "authentication failed", user_msg = "Authentication failed", error = e)
        NULL
      })
    return(out)
  })

  # text output for user info
  output$user_greeting <- renderText({
    req(get_user())
    paste("Welcome ", get_user()$first_name)
  })

  # test output for app info
  output$app_info <- renderText({
    paste("App version ", packageVersion("orderit"))
  })

  #  available functions ====
  list(
    reload_data = reload_data,
    get_users_data = get_users_data,
    get_user = get_user,
    authenticated = reactive({ !is.null(get_user()) }),
    get_grants_data = get_grants_data
  )
}

# data ui components ------
module_data_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("reload"), "Reload all data", icon = icon("cloud-download-alt"))
}

module_data_user_greeting <- function(id) {
  ns <- NS(id)
  textOutput(ns("user_greeting"))
}

module_data_app_info <- function(id) {
  ns <- NS(id)
  textOutput(ns("app_info"))
}
