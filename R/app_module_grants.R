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
        title = span(
          icon("coins"), "Grants",
          div(
            style = "position: absolute; right: 10px; top: 5px;",
            module_selector_table_columns_button(ns("grants_table"), border = FALSE),
            module_selector_table_search_button(ns("grants_table"), border = FALSE)
          )
        ), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("grants_table"))
      )

    )
  })

  # grants data ======
  get_grants <- reactive({
    validate(
      need(data$grants$get_data(), "something went wrong retrieving the data"),
      need(data$users$get_data(), "something went wrong retrieving the data"),
      need(data$get_active_user_data(), "something went wrong retrieving the data")
    )

    return(
      data$grants$get_data() |>
        # grants in the same group as the user
        dplyr::filter(.data$group %in% data$get_active_user_data()$groups) |>
        # bring in approver and orderer names
        dplyr::left_join(
          data$users$get_data() |> dplyr::rename_with(~paste0("approver_", .x), dplyr::everything()),
          by = "approver_user_id"
        ) |>
        dplyr::left_join(
          data$users$get_data() |> dplyr::rename_with(~paste0("orderer_", .x), dplyr::everything()),
          by = "orderer_user_id"
        )
    )
  })

  # grants table ==============
  grants <- callModule(
    module_selector_table_server,
    "grants_table",
    get_data = get_grants,
    id_column = "grant_id",
    available_columns = list(
      Grant = name, Status = status, `Speed Type` = identifier,
      `Approver` = paste(approver_first_name %then% "", approver_last_name %then% ""),
      `Needs approval` =
        ifelse(!is.na(approval_cutoff) & approval_cutoff > 0,
          paste(">", scales::label_dollar()(approval_cutoff)),
          "no"
        ),
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
