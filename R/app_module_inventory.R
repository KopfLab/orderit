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
        title = span(icon("flask-vial"), "Inventory"), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("inventory_table")),
        footer = div(
          module_selector_table_buttons(ns("inventory_table")),
          spaces(1),
          module_selector_table_columns_button(ns("inventory_table"))
        )
      )

    )
  })

  # inventory table ==============
  inventory <- callModule(
    module_selector_table_server,
    "inventory_table",
    get_data = data$get_inventory_data,
    id_column = "item_id",
    show_columns = list(
      Item = ifelse(
        !is.na(url) & nchar(url) > 0,
        sprintf("<a href = '%s' target = '_blank'>%s</a>", url, htmltools::htmlEscape(name)),
        htmltools::htmlEscape(name)
      ),
      Vendor = factor(vendor),
      `Catalog #` = catalog_nr,
      `Unit price` = price,
      `Unit size` = unit_size,
      `Notes` = notes
    ),
    allow_view_all = FALSE,
    initial_page_length = 10,
    filter = "top",
    selection = "multiple",
    render_html = "Item",
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
