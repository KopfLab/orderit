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
      h4(icon("dashboard"), "Inventory"),
      checkboxInput(ns("test_data"), "Use test data", value = FALSE)
    )
  })

  # use test data ----
  observeEvent(input$test_data, {
    log_debug(ns = ns, "inventory test data button pressed")
  })

}

# inventory user interface ------
module_inventory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main"))
}
