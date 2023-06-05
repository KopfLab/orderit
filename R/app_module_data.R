# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_server <- function(input, output, session, data_sheet_id, data_folder_id, gs_key_file, user_id) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    file_path = NULL,
    users_data = NULL,
    users_hash = NULL,
    grants_data = NULL,
    grants_hash = NULL,



    active_user_data = NULL,
    active_user_hash = NULL,
    locked = NULL,
    error = FALSE
  )

  # (re-) load data event =====
  reload_data <- function() {
    values$load_data <- values$load_data + 1L
  }
  observeEvent(input$reload, reload_data())

  # download data event =========
  observeEvent(values$load_data, {

    # lock when this cascade starts
    lock_app()
    log_info(ns = ns, "requesting google spreadsheet data", user_msg = "Fetching data")
    values$file_path <-
      tryCatch({
        # don't download from scratch every time if in development mode
        if (is_dev_mode() && file.exists("local_data.xlsx")) {
          file_path <- "local_data.xlsx"
          log_debug(ns = ns, "in DEV mode, using local data file")
          Sys.sleep(1)
        } else
          file_path <- download_google_sheet(data_sheet_id, gs_key_file = gs_key_file)

        # save locally if in dev mode
        if (is_dev_mode() && !file.exists("local_data.xlsx")) {
          file.copy(file_path, "local_data.xlsx")
          log_debug(ns = ns, "in DEV mode, saving downloaded data to local file")
        }
        file_path
      },
      error = function(e) {
        log_error(ns = ns, "download failed", user_msg = "Data loading error", error = e)
        values$error <- TRUE
        NULL
      })
  }, priority = 1000L)

  # read data event =========
  observeEvent(values$load_data, {
    req(!values$error)
    log_debug(ns = ns, "file path: ", values$file_path)
    log_info(ns = ns, "loading data from xlsx file", user_msg = "Loading data")

    # user data
    users_data <- read_data("users", cols = c("user_id", "first_name", "last_name", "role"))
    users_hash <- digest::digest(users_data)
    if (!identical(users_hash, isolate(values$users_hash))) {
      log_info(ns = ns, "found new users data")
      values$users_data <- users_data
      values$users_hash <- users_hash
    }

    # grants data
    grants_data <- read_data("grants",
      cols = c("grant_id", "name", "status", "speed_type", "pi_user_id", "orderer_user_id"))
    grants_hash <- digest::digest(grants_data)
    if (!identical(grants_hash, isolate(values$grants_hash))) {
      log_info(ns = ns, "found new grants data")
      values$grants_data <- grants_data
      values$grants_hash <- grants_hash
    }
  })

  # read data utility function
  read_data <- function(sheet, cols = dplyr::everything()) {
    req(!values$error)
    validate(need(file.exists(values$file_path), "something went wrong retrieving the data"))
    sheets <- readxl::excel_sheets(values$file_path)
    # check if sheet exists
    if (!sheet %in% sheets) {
      log_error(ns = ns, user_msg = "Cannot read data", error = sprintf("'%s' tab doesn't exist", sheet))
      values$error <- TRUE
      return(NULL)
    }
    # try to read data
    tryCatch({
      data <- readxl::read_excel(values$file_path, sheet = sheet) |>
        dplyr::select({{ cols }})
      return(data)
    },
    error = function(e) {
      log_error(ns = ns, "data reading failed", user_msg = "Missing data", error = e)
      values$error <- TRUE
      return(NULL)
    })
  }

  # authentication event =====
  observeEvent(values$users_data, {
    req(!values$error)
    log_info(ns = ns, "authenticating user by checking users data",
             user_msg = sprintf("Authenticating '%s'", user_id))
    active_user_data <-
      tryCatch({
        user <- get_users_data() |>
          dplyr::filter(user_id == !!user_id)
        if(nrow(user) < 1L) cli::cli_abort(paste0("user does not exist: ", user_id))
        if(nrow(user) > 1L) cli::cli_abort(paste0("user_id is not unique: ", user_id))
        user
      },
      error = function(e) {
        log_error(ns = ns, "authentication failed", user_msg = "Authentication failed", error = e)
        values$error <- TRUE
        NULL
      })

    # check if active user has changed
    active_user_hash <- digest::digest(active_user_data)
    if (!identical(active_user_hash, values$active_user_hash)) {
      log_info(ns = ns, "found new active user data")
      values$active_user_data <- active_user_data
      values$active_user_hash <- active_user_hash
      log_info(ns = ns, "login complete")
    }
  })

  is_authenticated <- reactive({ !is.null(values$active_user_data) })

  # lock/unlock events ====
  observe({
    # triggers
    values$load_data
    values$locked
    values$error

    # unlock if authenticated
    isolate({
      if (is_authenticated() && values$locked && !values$error) {
        values$locked <- FALSE
      } else if (!values$locked) {
        unlock_app()
      } else {
        # reset
        values$users_data <- NULL
        values$users_hash <- NULL
        values$grants_data <- NULL
        values$grants_hash <- NULL
        values$active_user_data <- NULL
        values$active_user_hash <- NULL
        shinyjs::hide("menu", asis = TRUE)
        log_info(ns = ns, "app stays locked")
      }
    })
  }, priority = -1000L)

  lock_app <- function() {
    log_info(ns = ns, "locking app")
    values$locked <- TRUE
    values$error <- FALSE
  }

  unlock_app <- function() {
    log_info(ns = ns, "unlocking app")
    shinyjs::show("menu", asis = TRUE)
    log_success(ns = ns, "loading all done", user_msg = "Complete")
  }

  # data functions ======
  get_users_data <- reactive({
    validate(need(values$users_data, "something went wrong retrieving the data"))
    return(values$users_data)
  })

  get_grants_data <- reactive({
    log_debug(ns = ns, "retrieving grants data")
    validate(need(values$grants_data, "something went wrong retrieving the data"))
    return(
      values$grants_data |>
        dplyr::left_join(
          get_users_data() |> dplyr::rename_with(~paste0("pi_", .x), everything()),
          by = "pi_user_id"
        ) |>
        dplyr::left_join(
          get_users_data() |> dplyr::rename_with(~paste0("orderer_", .x), everything()),
          by = "orderer_user_id"
        )
    )
  })

  get_active_user_data <- reactive({
    validate(need(is_authenticated(), "user not authenticated"))
    return(values$active_user_data)
  })

  #  return function ====
  list(
    reload_data = reload_data,
    is_authenticated = is_authenticated,
    get_users_data = get_users_data,
    get_active_user_data = get_active_user_data,
    get_grants_data = get_grants_data
  )
}

# data ui components - reload button ------
module_data_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("reload"), "Reload", icon = icon("cloud-download-alt"))
}
