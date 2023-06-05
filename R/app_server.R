# server
server <- function(data_sheet_id, data_folder_id, gs_key_file, timezone, user_id) {

  shinyServer(function(input, output, session) {

    log_info("\n\n========================================================")
    log_info("starting orderit GUI", if(is_dev_mode()) " in DEV mode")

    # navigation
    observeEvent(input$nav, log_debug("menu item selected: ", input$nav))

    # user info
    output$user_first_name <- renderText({
      req(data$is_authenticated())
      data$get_active_user_data()$first_name
    })

    # data module
    data <- callModule(
      module_data_server, id = "data",
      data_sheet_id = data_sheet_id,
      data_folder_id = data_folder_id,
      gs_key_file = gs_key_file,
      user_id = user_id
    )

    # # inventory module
    # inventory <- callModule(
    #   module_inventory_server, id = "inventory",
    #   data = data
    # )

    # grants module
    grants <- callModule(
      module_grants_server, id = "grants",
      data = data
    )

  })

}
