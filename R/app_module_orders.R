# orders server ----
module_orders_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # generate UI =====================
  output$main <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering orders UI")
    tagList(
      # requested
      tabPanel(
        "Requested",
        shinydashboard::box(
          title = span(
            icon("code-pull-request"), "Requested",
            div(
              style = "position: absolute; right: 20px; top: 5px;",
              module_selector_table_columns_button(ns("requested_table"), border = FALSE),
              module_selector_table_search_button(ns("requested_table"), border = FALSE)
            )
          ), width = 12,
          status = "info", solidHeader = TRUE, collapsible = TRUE,
          module_selector_table_ui(ns("requested_table"))
        )
      ),
      # ordered
      tabPanel(
        "Ordered",
        shinydashboard::box(
          title = span(
            icon("truck"), "Ordered",
            div(
              style = "position: absolute; right: 20px; top: 5px;",
              module_selector_table_columns_button(ns("ordered_table"), border = FALSE),
              module_selector_table_search_button(ns("ordered_table"), border = FALSE)
            )
          ), width = 12,
          status = "info", solidHeader = TRUE, collapsible = TRUE,
          module_selector_table_ui(ns("ordered_table"))
        )
      ),
      # received
      tabPanel(
        "Received",
        shinydashboard::box(
          title = span(
            icon("check"), "Received",
            div(
              style = "position: absolute; right: 20px; top: 5px;",
              module_selector_table_columns_button(ns("received_table"), border = FALSE),
              module_selector_table_search_button(ns("received_table"), border = FALSE)
            )
          ), width = 12,
          status = "info", solidHeader = TRUE, collapsible = TRUE,
          module_selector_table_ui(ns("received_table"))
        )
      )
    )
  })

  # data for tables ======
  get_orders <- reactive({
    validate(
      need(data$orders$get_data(), "something went wrong retrieving the data"),
      need(data$inventory$get_data(), "something went wrong retrieving the data"),
      need(data$users$get_data(), "something went wrong retrieving the data"),
      need(data$grants$get_data(), "something went wrong retrieving the data"),
      need(data$get_active_user_data(), "something went wrong retrieving the data")
    )
    return(
      data$orders$get_data() |>
        # bring in grant
        dplyr::left_join(
          data$grants$get_data() |>
            dplyr::select("grant_id", "grant_name" = "name",
                          "grant_group" = "group", "orderer_user_id"),
          by = "grant_id"
        ) |>
        # grants has to be in the same groups as the user to be visible
        dplyr::filter(.data$grant_group %in% data$get_active_user_data()$groups) |>
        # bring in item
        dplyr::left_join(
          data$inventory$get_data() |>
            dplyr::select("item_id", "item_status" = "status", "item_name" = "name",
                          "vendor", "catalog_nr", "unit_price", "unit_size", "url"),
          by = "item_id"
        ) |>
        # bring in requester
        dplyr::left_join(
          data$users$get_data() |> dplyr::rename_with(~paste0("requester_", .x), dplyr::everything()),
          by = c("requested_by" = "requester_user_id")
        ) |>
        # make factors for advanced search
        dplyr::mutate(
          vendor = factor(vendor),
          requester = paste(requester_first_name %then% "", requester_last_name %then% "") |> factor(),
          grant_name = factor(grant_name)
        )
    )
  })

  get_requested <- reactive({
    get_orders() |>
      dplyr::filter(is.na(.data$ordered_on)) |>
      dplyr::arrange(dplyr::desc(.data$requested_on)) |>
      # bring in orderer
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("orderer_", .x), dplyr::everything()),
        by = c("orderer_user_id" = "orderer_user_id")
      ) |>
      dplyr::mutate(
        orderer = paste(orderer_first_name %then% "", orderer_last_name %then% "") |>
          factor()
      )
  })

  get_ordered <- reactive({
    get_orders() |>
      dplyr::filter(!is.na(.data$ordered_on) & is.na(.data$received_on)) |>
      dplyr::arrange(dplyr::desc(.data$ordered_on))
    # bring in ordered_by
  })

  get_received <- reactive({
    get_orders() |>
      dplyr::filter(!is.na(.data$received_on)) |>
      dplyr::arrange(dplyr::desc(.data$received_on))
    # bring in received by
  })

  # data tables ==============

  # requested
  requested <- callModule(
    module_selector_table_server,
    "requested_table",
    get_data = get_requested,
    id_column = "order_id",
    available_columns = list(
      Item = item_name,
      Status = item_status,
      Vendor = vendor,
      `Catalog #` =
        ifelse(
          !is.na(url) & nchar(url) > 0,
          sprintf(
            "<a href = '%s' target = '_blank'>%s</a>",
            gsub("^(http(s?)://)?", "https://", url), htmltools::htmlEscape(catalog_nr)
          ),
          htmltools::htmlEscape(catalog_nr)
        ),
      Quantity = sprintf("%d x %s", quantity, unit_size),
      Total = quantity * unit_price,
      `Requested by` = requester,
      `Requested on` = as.character(requested_on),
      Grant = grant_name,
      `Orderer` = orderer,
      Notes = notes
    ),
    allow_view_all = TRUE,
    initial_page_length = 10,
    selection = "multiple",
    render_html = "Catalog #",
    formatting_calls = list(
      list(func = DT::formatCurrency, columns = "Total")
    )
  )

  # ordered
  ordered <- callModule(
    module_selector_table_server,
    "ordered_table",
    get_data = get_ordered,
    id_column = "order_id",
    available_columns = list(
      Quantity = quantity,
      Notes = notes
    ),
    allow_view_all = TRUE,
    initial_page_length = 10,
    selection = "multiple"
  )

  # received
  received <- callModule(
    module_selector_table_server,
    "received_table",
    get_data = get_received,
    id_column = "order_id",
    available_columns = list(
      Quantity = quantity,
      Notes = notes
    ),
    allow_view_all = FALSE,
    initial_page_length = 10,
    selection = "none"
  )

}

# orders user interface ------
module_orders_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
