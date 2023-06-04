# server
server <- function(data_sheet_id, data_folder_id, gs_key_file, user_id) {

  shinyServer(function(input, output, session) {

    log_info("starting orderit GUI")

    # navigation
    observeEvent(input$nav, gui_message("debug", "menu item selected: ", input$nav))

    # data module
    data <- callModule(
      module_data_server, id = "data",
      data_sheet_id = data_sheet_id,
      data_folder_id = data_folder_id,
      gs_key_file = gs_key_file,
      user_id = user_id
    )

    # inventory module
    inventory <- callModule(
      module_inventory_server, id = "inventory",
      data = data
    )

    output$user_name <- renderText({
      return(user_id)
    })
    output$users <- renderTable({
      my <- data$get_users_data()
      return(my)
    })
    output$user_info <- renderText({ "details test" })

  })

}
