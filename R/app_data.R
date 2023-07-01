# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_server <- function(input, output, session, data_sheet_id, data_folder_id, gs_key_file, timezone, user_id) {

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    load_data = 1L,
    file_path = NULL,
    active_user_data = NULL,
    active_user_hash = NULL,
    locked = NULL,
    error = FALSE
  )

  # data tables =========
  get_local_file <- reactive({ values$file_path })
  report_error <- function() {
    values$error <- TRUE
  }

  # users
  users <- callModule(
    module_data_table_server, id = "users",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "users",
    cols = c("user_id", "first_name", "last_name", "groups", "role")
  )

  # grants
  grants <- callModule(
    module_data_table_server, id = "grants",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "grants",
    cols = c("grant_id" = "integer", "name", "identifier" = "character", "group", "status", "approver_user_id", "approval_cutoff" = "double", "orderer_user_id")
  )

  # inventory
  inventory <- callModule(
    module_data_table_server, id = "inventory",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "inventory",
    cols = c("item_id" = "integer", "status", "name", "vendor", "catalog_nr", "unit_price" = "double", "unit_size", "added_by", "added_on" = "datetime", "details", "url")
  )

  # orders
  orders <- callModule(
    module_data_table_server, id = "orders",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "orders",
    cols = c("order_id" = "integer", "item_id" = "integer", "quantity" = "integer", "grant_id" = "integer", "notes", "requested_by", "requested_on" = "datetime", "approved_by", "approved_on" = "datetime", "ordered_by", "ordered_on" = "datetime", "received_by", "received_on" = "datetime")
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
        } else
          file_path <- download_gs(data_sheet_id, gs_key_file = gs_key_file)

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
  }, priority = 10L)

  # read data event =========
  observeEvent(values$load_data, {
    req(!values$error)
    log_debug(ns = ns, "file path: ", values$file_path)
    log_info(ns = ns, "loading data from xlsx file", user_msg = "Loading data")

    # reading data
    users$read_data(timezone = timezone)
    grants$read_data(timezone = timezone)
    inventory$read_data(timezone = timezone)
    orders$read_data(timezone = timezone)
  }, priority = 9L)

  # authentication event =====
  observeEvent(users$get_data(), {
    req(!values$error)
    log_info(ns = ns, "authenticating user by checking users data",
             user_msg = sprintf("Authenticating '%s'", user_id))
    active_user_data <-
      tryCatch({
        user <- users$get_data() |>
          dplyr::filter(user_id == !!user_id)
        if(nrow(user) < 1L) abort(paste0("user does not exist: ", user_id))
        if(nrow(user) > 1L) abort(paste0("user_id is not unique: ", user_id))
        user <- user |> as.list()
        # split user groups
        user$groups <- strsplit(user$groups, split = ",", fixed = TRUE)[[1]]
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
  }, priority = 8L)

  is_authenticated <- reactive({ !is.null(values$active_user_data) })

  get_active_user_data <- reactive({
    validate(need(is_authenticated(), "user not authenticated"))
    return(values$active_user_data)
  })

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
        users$reset()
        grants$reset()
        inventory$reset()
        values$active_user_data <- NULL
        values$active_user_hash <- NULL
        shinyjs::hide("menu", asis = TRUE)
        log_info(ns = ns, "app stays locked")
      }
    })
  }, priority = 1L)

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

  # download data =====

  output$download <- downloadHandler(
    filename = function() {
      lubridate::now() |> lubridate::with_tz(timezone) |>
        format("data_%Y_%m_%d_%H_%M_%S.xlsx")
    },
    content = function(file) {
      file.copy(from = isolate(values$file_path), to = file)
    }
  )

  #  return function ====
  list(
    reload_data = reload_data,
    is_authenticated = is_authenticated,
    get_active_user_data = get_active_user_data,
    users = users,
    grants = grants,
    inventory = inventory,
    orders = orders
  )
}

# data ui components - reload button ------
module_data_reload_button <- function(id) {
  ns <- NS(id)
  actionButton(ns("reload"), "Reload", icon = icon("rotate")) |>
    add_tooltip("Reload all data")
}

# data ui components - download button ------
module_data_download_button <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download"), "Backup") |>
    add_tooltip("Download all data")
}
