# inventory server ----
module_grants_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # UI =====================
  output$main <- renderUI({
    validate(need(data$authenticated(), "Authentication failed"))
    log_info(ns = ns, "rendering grants UI")
    tagList(
      h2("Grants"),
      module_selector_table_ui(ns("grants"))
    )
  })

  # grants table ==============
  # grants <- callModule(
  #   module_selector_table_server,
  #   "grants",
  #   get_data = data$get_grants_data,
  #   id_column = "grant_id",
  #   show_columns = list(
  #     Grant = name, Status = status, `Speed Type` = speed_type,
  #     `PI` = paste(pi_first_name %then% "", pi_last_name %then% ""),
  #     `Orderer` = paste(orderer_first_name %then% "", orderer_last_name %then% "")
  #   ),
  #   selector_buttons = FALSE
  # )

}

module_grants_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
