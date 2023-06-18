# inventory server ----
module_inventory_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # UI =====================
  output$main <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering inventory UI")
    tagList(
      shinydashboard::box(
        title = span(
          icon("flask-vial"), "Inventory",
          div(
            style = "position: absolute; right: 10px; top: 5px;",
            module_selector_table_selection_buttons(ns("inventory_table"), border = FALSE),
            module_selector_table_columns_button(ns("inventory_table"), border = FALSE),
            module_selector_table_search_button(ns("inventory_table"), border = FALSE)
          )
        ), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("inventory_table"))
      )

    )
  })

  # inventory table ==============
  inventory <- callModule(
    module_selector_table_server,
    "inventory_table",
    get_data = data$get_inventory_data,
    id_column = "item_id",
    available_columns = list(
      Item = name,
      Vendor = factor(vendor),
      `Catalog #` =
        ifelse(
          !is.na(url) & nchar(url) > 0,
          sprintf("<a href = '%s' target = '_blank'>%s</a>", url, htmltools::htmlEscape(catalog_nr)),
          htmltools::htmlEscape(catalog_nr)
        ),
      `Unit price` = price,
      `Unit size` = unit_size,
      `Notes` = notes
    ),
    visible_columns = 1:5, # all except notes
    allow_view_all = FALSE,
    initial_page_length = 10,
    selection = "multiple",
    render_html = "Catalog #",
    formatting_calls = list(
      list(func = DT::formatCurrency, columns = "Unit price")
    )
  )

}

# inventory user interface ------
module_inventory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
