# server
server <- function(
  data_sheet_id,
  data_folder_id,
  gs_key_file,
  timezone,
  user_id,
  user_is_admin,
  user_groups
) {
  # server function
  function(input, output, session) {
    log_info("\n\n========================================================")
    log_info("starting orderit GUI", if (is_dev_mode()) " in DEV mode")

    # navigation
    observeEvent(input$nav, log_debug("menu item selected: ", input$nav))

    # data module
    data <- callModule(
      module_data_server,
      id = "data",
      data_sheet_id = data_sheet_id,
      data_folder_id = data_folder_id,
      gs_key_file = gs_key_file,
      timezone = timezone,
      user_id = user_id,
      user_is_admin = user_is_admin,
      user_groups = user_groups
    )

    # grants module
    grants <- callModule(
      module_grants_server,
      id = "grants",
      data = data
    )

    # inventory module
    inventory <- callModule(
      module_inventory_server,
      id = "inventory",
      data = data
    )

    # # orders module
    orders <- callModule(
      module_orders_server,
      id = "orders",
      data = data
    )
  }
}
