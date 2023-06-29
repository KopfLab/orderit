# inventory server ----
module_inventory_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # generate UI =====================
  output$main <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering inventory UI")
    tagList(
      shinydashboard::box(
        title = span(
          icon("flask-vial"), "Inventory",
          div(
            style = "position: absolute; right: 10px; top: 5px;",
            actionButton(ns("add"), "New Item", icon = icon("plus"), style = "border: 0;") |>
              add_tooltip("Add a new inventory item."),
            actionButton(ns("edit"), "Edit Item", icon = icon("pen-to-square"), style = "border: 0;") |>
              add_tooltip("Edit the selected inventory item."),
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

  # data for table ======
  get_inventory <- reactive({
    validate(
      need(data$inventory$get_data(), "something went wrong retrieving the data"),
      need(data$users$get_data(), "something went wrong retrieving the data"),
      need(data$get_active_user_data(), "something went wrong retrieving the data")
    )
    return(
      data$inventory$get_data() |>
        # bring in added_by name
        dplyr::left_join(data$users$get_data(), by = c("added_by" = "user_id")) |>
        # make vendor a factor
        dplyr::mutate(vendor = factor(vendor)) |>
        # sort by name
        dplyr::arrange(tolower(.data$name))
    )
  })

  # data table ==============
  inventory <- callModule(
    module_selector_table_server,
    "inventory_table",
    get_data = get_inventory,
    id_column = "item_id",
    available_columns = list(
      Item = name,
      Status = status,
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
      `Unit price` = unit_price,
      `Unit size` = unit_size,
      `Added by` = paste(first_name %then% "", last_name %then% ""),
      `Add timestamp` = as.character(added_on),
      `Details` = details
    ),
    visible_columns = 1:5, # through unit size
    allow_view_all = FALSE,
    initial_page_length = 10,
    selection = "multiple",
    render_html = "Catalog #",
    formatting_calls = list(
      list(func = DT::formatCurrency, columns = "Unit price")
    )
  )

  # add/edit dialog ========
  dialog_inputs <- reactive({
    req(get_inventory())
    log_debug(ns = ns, "generating dialog inputs")
    tagList(
      textInput(ns("name"), "Name"),
      selectInput(ns("status"), "Status", choices = c("new", "ok", "old")) |>
        add_tooltip("Indicate whether this item has been confirmed to be the right one ('ok'), is a new one that needs confirmation ('new'), or shouldn't be ordered anymore ('old')"),
      selectizeInput(ns("vendor"), "Vendor", choices = get_inventory()$vendor |> levels(), options = list(create = TRUE)),
      textInput(ns("catalog_nr"), "Catalog #"),
      numericInput(ns("unit_price"), "Unit price", value = 0, min = 0),
      textInput(ns("unit_size"), "Unit size"),
      textInput(ns("url"), "URL"),
      textAreaInput(ns("details"), "Details", resize = "vertical")
    )
  })

  create_dialog <- function(title) {
    modalDialog(
      size = "s",
      title = title,
      dialog_inputs(),
      footer = tagList(
        actionButton(ns("save"), "Save"),
        modalButton("Cancel")
      )
    )
  }

  # add =====
  observeEvent(input$add, {
    data$inventory$start_add()
    updateTextInput(inputId = "name", placeholder = "Enter name of new item")
    updateSelectInput(inputId = "status", selected = "new")
    updateSelectInput(inputId = "vendor", selected = "")
    updateTextInput(inputId = "catalog_nr", placeholder = "Enter catalog # for new item")
    updateNumericInput(inputId = "unit_price", value = NA)
    updateTextInput(inputId = "unit_size", placeholder = "Enter unit size")
    updateTextInput(inputId = "url", placeholder = "Enter web address for item")
    updateTextInput(inputId = "details", placeholder = "Enter details and notes")
    showModal(create_dialog("Add new inventory item"))
  })

  # edit ===========

  observe({
    # edit available only when exactly 1 record selected
    toggle <- nrow(inventory$get_selected_items()) == 1L
    shinyjs::toggleState("edit", condition = toggle)
  })

  observeEvent(input$edit, {
    req(item <- inventory$get_selected_items())
    data$inventory$start_edit(id = item$item_id)
    updateTextInput(inputId = "name", value = item$name)
    updateSelectInput(inputId = "status", selected = item$status)
    updateSelectInput(inputId = "vendor", selected = item$vendor)
    updateTextInput(inputId = "catalog_nr", value = item$catalog_nr)
    updateNumericInput(inputId = "unit_price", value = item$unit_price)
    updateTextInput(inputId = "unit_size", value = item$unit_size)
    updateTextInput(inputId = "url", value = item$url)
    updateTextInput(inputId = "details", value = item$details)
    showModal(create_dialog("Edit inventory item"))
  })

  # check inputs ====
  check_inputs <- function() {
    log_info(ns = ns, "checking inputs")
    ok <- TRUE
    # input checks
    if (nchar(input$name) == 0) {
      log_warning(ns = ns, "no name provided", user_msg = "Name: please enter a name for the item")
      ok <- FALSE
    }
    return(ok)
  }

  # save =====
  observeEvent(input$save, {

    # save if inputs are good
    if (check_inputs()) {

      # disable inputs while saving
      c("status", "name", "vendor", "catalog_nr", "unit_price", "unit_size", "url", "details", "save") |>
        purrr::walk(shinyjs::disable)

      # values
      values <- list(
        status = input$status,
        name = input$name,
        vendor = input$vendor,
        catalog_nr = input$catalog_nr,
        unit_price = input$unit_price,
        unit_size = input$unit_size,
        url = input$url,
        details = input$details
      )

      # are we adding data?
      if (data$inventory$is_add()) {
        values <- c(
          values,
          list(
            added_by = data$get_active_user_data()$user_id,
            added_on = lubridate::now()
          )
        )
      }

      # update data
      data$inventory$update(.list = values)
      if (data$inventory$commit()) removeModal()
    }
  })


}

# inventory user interface ------
module_inventory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
