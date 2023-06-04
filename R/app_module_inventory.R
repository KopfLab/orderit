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
module_inventory_server <- function(input, output, session) {


  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # use test data ----
  observeEvent(input$test_data, {
    module_message(ns, "debug", "inventory test data button pressed")
  })

}

# inventory user interface ------
module_inventory_ui <- function(id) {


  # namespace
  ns <- NS(id)

  # page
  fluidPage(

    checkboxInput(ns("test_data"), "Use test data", value = FALSE)

  )
}
