# server
server <- function() {

  shinyServer(function(input, output, session) {

    gui_message("info", "starting orderit GUI")

    # navigation
    observeEvent(input$nav, gui_message("debug", "menu item selected: ", input$nav))

    # inventory module
    inventory <- callModule(module_inventory_server, id = "inventory")

    output$user_name <- renderText({
      user_id <- Sys.getenv("SHINYPROXY_USERNAME")
      user_name <- "Anonymous"
      if (!is.null(user_id)) user_name <- user_id
      return(user_name)
    })
    output$user_info <- renderText({ "details test" })

  })

}
