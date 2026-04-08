# data server ----
# @param data_sheet_id google sheets id with the app data
# @param data_folder_id google drive folder id for file storage
module_data_server <- function(
  input,
  output,
  session,
  data_sheet_id,
  data_folder_id,
  gs_key_file,
  timezone,
  user_id,
  user_is_admin,
  user_groups
) {
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
  get_local_file <- reactive({
    values$file_path
  })
  report_error <- function() {
    values$error <- TRUE
  }

  # admins
  admins <- callModule(
    module_data_table_server,
    id = "admins",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "admins",
    cols = c(
      "user_id",
      "user_email",
      "first_name",
      "last_name",
      "groups"
    )
  )

  # grants
  grants <- callModule(
    module_data_table_server,
    id = "grants",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "grants",
    cols = c(
      "grant_id" = "integer",
      "name",
      "identifier" = "character",
      "group",
      "status",
      "approver_user_id",
      "approval_cutoff" = "double",
      "orderer_user_id"
    )
  )

  # inventory
  inventory <- callModule(
    module_data_table_server,
    id = "inventory",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "inventory",
    cols = c(
      "item_id" = "integer",
      "status",
      "name",
      "vendor",
      "catalog_nr",
      "unit_price" = "double",
      "unit_size",
      "added_by",
      "added_on" = "datetime",
      "last_price_update" = "datetime",
      "details",
      "url"
    )
  )

  # orders
  orders <- callModule(
    module_data_table_server,
    id = "orders",
    data_sheet_id = data_sheet_id,
    gs_key_file = gs_key_file,
    local_file = get_local_file,
    report_error = report_error,
    reload_data = reload_data,
    sheet = "orders",
    cols = c(
      "order_id" = "integer",
      "item_id" = "integer",
      "quantity" = "integer",
      "grant_id" = "integer",
      "notes",
      "urgent" = "logical",
      "requested_by",
      "requested_on" = "datetime",
      "approved_by",
      "approved_on" = "datetime",
      "ordered_by",
      "ordered_on" = "datetime",
      "received_by",
      "received_on" = "datetime",
      "canceled_by"
    )
  )

  # (re-) load data event =====
  reload_data <- function() {
    values$load_data <- values$load_data + 1L
  }
  observeEvent(input$reload, reload_data())

  # download data event =========
  observeEvent(
    values$load_data,
    {
      log_info(
        ns = ns,
        "requesting google spreadsheet data",
        user_msg = "Fetching data"
      )
      values$file_path <-
        tryCatch(
          {
            # don't download from scratch every time if in development mode
            if (is_dev_mode() && file.exists("local_data.xlsx")) {
              file_path <- "local_data.xlsx"
              log_debug(ns = ns, "in DEV mode, using local data file")
            } else {
              file_path <- download_gs(data_sheet_id, gs_key_file = gs_key_file)
            }

            # save locally if in dev mode
            if (is_dev_mode() && !file.exists("local_data.xlsx")) {
              file.copy(file_path, "local_data.xlsx")
              log_debug(
                ns = ns,
                "in DEV mode, saving downloaded data to local file"
              )
            }
            file_path
          },
          error = function(e) {
            log_error(
              ns = ns,
              "download failed",
              user_msg = "Data loading error",
              error = e
            )
            values$error <- TRUE
            NULL
          }
        )
    },
    priority = 10L
  )

  # read data event =========
  observeEvent(
    values$load_data,
    {
      req(!values$error)
      log_debug(ns = ns, "file path: ", values$file_path)
      log_info(
        ns = ns,
        "loading data from xlsx file",
        user_msg = "Loading data"
      )

      # reading data
      admins$read_data(timezone = timezone)
      grants$read_data(timezone = timezone)
      inventory$read_data(timezone = timezone)
      orders$read_data(timezone = timezone)
    },
    priority = 9L
  )

  # authentication event =====
  get_active_user_data <- reactive({
    list(
      user_id = user_id,
      groups = user_groups
    )
  })

  is_active_user_admin <- reactive({
    user_is_admin
  })

  # download data =====

  output$download <- downloadHandler(
    filename = function() {
      lubridate::now() |>
        lubridate::with_tz(timezone) |>
        format("data_%Y_%m_%d_%H_%M_%S.xlsx")
    },
    content = function(file) {
      file.copy(from = isolate(values$file_path), to = file)
    }
  )

  #  return function ====
  list(
    reload_data = reload_data,
    get_active_user_data = get_active_user_data,
    is_active_user_admin = is_active_user_admin,
    admins = admins,
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
    shinyjs::hidden() |>
    shinyjs::disabled() |>
    add_tooltip("Download all data")
}
