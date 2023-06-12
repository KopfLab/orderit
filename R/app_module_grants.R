# grants server ----
module_grants_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # UI =====================
  output$main <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering grants UI")
    tagList(
      shinydashboard::box(
        title = span(icon("coins"), "Grants"), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("grants_table")),
        footer = div(
          module_selector_table_columns_button(ns("grants_table"))
        )
      )

    )
  })

  # grants table ==============
  grants <- callModule(
    module_selector_table_server,
    "grants_table",
    get_data = data$get_grants_data,
    id_column = "grant_id",
    show_columns = list(
      Grant = name, Status = status, `Speed Type` = speed_type,
      `PI` = paste(pi_first_name %then% "", pi_last_name %then% ""),
      `Orderer` = paste(orderer_first_name %then% "", orderer_last_name %then% "")
    ),
    allow_view_all = TRUE,
    initial_page_length = -1,
    selection = "none"
  )

}

module_grants_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
