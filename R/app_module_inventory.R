# test inventory module
test_module_inventory <- function() {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("inventory Module Test"),
        module_inventory_ui(id = "inventory")
      ),
      server = function(input, output) {
        inventory <-
          callModule(module_inventory_server, id = "inventory")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# inventory server ----
module_inventory_server <- function(input, output, session, data) {


  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # out
  output$main <- renderUI({
    validate(need(data$authenticated(), "Authenticating..."))
    log_info(ns = ns, "rendering inventory UI")
    tagList(
      checkboxInput(ns("test_data"), "Use test data", value = FALSE)
    )
  })

  # use test data ----
  observeEvent(input$test_data, {
    module_message(ns, "debug", "inventory test data button pressed")
  })

}

# inventory user interface ------
module_inventory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main"))
}
