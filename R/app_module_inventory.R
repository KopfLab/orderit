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
          icon("flask"), "Inventory",
          div(
            style = "position: absolute; right: 10px; top: 5px;",
            actionButton(ns("request"), "Request", icon = icon("cart-shopping"), style = "border: 0;") |>
              add_tooltip("Request selected item(s)."),
            actionButton(ns("add"), "New Item", icon = icon("plus"), style = "border: 0;") |>
              add_tooltip("Add a new inventory item."),
            actionButton(ns("edit"), "Edit Item", icon = icon("pen"), style = "border: 0;") |>
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
        # make vendor and status factors
        dplyr::mutate(vendor = factor(vendor), status = factor(status, levels = names(get_item_status_levels()))) |>
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
      `Last price update` = as.character(last_price_update),
      `Added by` = paste(first_name %then% "", last_name %then% ""),
      `Add timestamp` = as.character(added_on),
      `Details` = details
    ),
    visible_columns = 1:7, # through price update
    allow_view_all = FALSE,
    initial_page_length = 10,
    selection = "multiple",
    render_html = "Catalog #",
    formatting_calls = list(
      list(func = DT::formatCurrency, columns = "Unit price"),
      list(
        func = DT::formatStyle, columns = "Status",
        backgroundColor = DT::styleEqual(
          get_item_status_levels() |> names(),
          get_item_status_levels() |> as.character()
        )
      )
    )
  )

  # update number next to the request button
  observeEvent(inventory$get_selected_ids(), {
    updateActionButton(
      inputId = "request",
      label =
        if (length(inventory$get_selected_ids()) > 0)
          sprintf("Request (%d)", length(inventory$get_selected_ids()))
        else
          "Request"
    )
  }, ignoreNULL = FALSE)

  # add/edit dialog ========
  add_edit_dialog_inputs <- reactive({
    req(get_inventory())
    log_debug(ns = ns, "generating dialog inputs")
    tagList(
      textInput(ns("name"), "Name"),
      selectInput(ns("status"), "Status", choices = get_inventory()$status |> levels()) |>
        add_tooltip("Indicate whether this item needs confirmation, is current, or is outdated"),
      selectizeInput(ns("vendor"), "Vendor", choices = get_inventory()$vendor |> levels(), options = list(create = TRUE)),
      textInput(ns("catalog_nr"), "Catalog #"),
      numericInput(ns("unit_price"), "Unit price", value = 0, min = 0),
      textInput(ns("unit_size"), "Unit size"),
      textInput(ns("url"), "URL"),
      textAreaInput(ns("details"), "Details", resize = "vertical")
    )
  })

  create_add_edit_dialog <- function(title) {
    modalDialog(
      size = "s",
      title = title,
      add_edit_dialog_inputs(),
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
    updateSelectInput(inputId = "status", selected = "needs confirmation")
    updateSelectInput(inputId = "vendor", selected = "")
    updateTextInput(inputId = "catalog_nr", placeholder = "Enter catalog # for new item")
    updateNumericInput(inputId = "unit_price", value = NA)
    updateTextInput(inputId = "unit_size", placeholder = "Enter unit size")
    updateTextInput(inputId = "url", placeholder = "Enter web address for item")
    updateTextInput(inputId = "details", placeholder = "Enter details and notes")
    showModal(create_add_edit_dialog("Add new inventory item"))
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
    showModal(create_add_edit_dialog("Edit inventory item"))
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

  # item save =====
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
            added_on = lubridate::now(),
            last_price_update = if(!is.na(input$unit_price)) lubridate::now() else NA
          )
        )
      }

      # update data
      data$inventory$update(.list = values)

      # check if the price has changed for an existing item
      if (!data$inventory$is_add()) {
        new_price <- data$inventory$has_value_changed(column = "unit_price")
        if (new_price)
          data$inventory$update(last_price_update = lubridate::now())
      }

      # commit
      if (data$inventory$commit()) removeModal()
    }
  })

  # request dialog ======

  make_request_item_ui <- function(item_id, item_name) {
    tagList(
      h5(item_name),
      numericInput(
        ns(paste0("quantity_", item_id)),
        label = NULL,
        min = 0, step = 1, value = 1
      )
    )
  }

  create_request_dialog <- function(items, grants) {
    modalDialog(
      size = "m",
      title = "Create request",
      selectInput(ns("grant_id"), "Grant", choices = grants) |>
        add_tooltip("Select which grant/acount this request is for."),
      textAreaInput(ns("notes"), "Notes") |> add_tooltip("Add notes and special instructions for ordering/receiving this item."),
      checkboxInput(ns("urgent"), strong("Urgent"), value = FALSE) |> add_tooltip("Is this request really urgent?"),
      tags$hr(),
      h5(tags$strong("Quantities")),
      purrr::map2(items$item_id, items$name, make_request_item_ui),
      footer = tagList(
        actionButton(ns("save_request"), "Request"),
        modalButton("Cancel")
      )
    )
  }

  observe({
    # request available only when at least 1 record selected
    toggle <- nrow(inventory$get_selected_items()) > 0L
    shinyjs::toggleState("request", condition = toggle)
  })

  observeEvent(input$request, {
    req(data$grants$get_data())
    log_info(ns = ns, "loading request screen")
    dlg <- create_request_dialog(
      inventory$get_selected_items(),
      get_grants_list(data$grants$get_data(), data$get_active_user_data())
    )
    updateSelectInput(inputId = "grant", selected = "")
    showModal(dlg)
  })

  # request save =====

  check_request_inputs <- function() {
    log_info(ns = ns, "checking request inputs")
    ok <- TRUE
    # input checks
    if (nchar(input$grant_id) == 0) {
      log_warning(ns = ns, "no grant selected", user_msg = "Grant: please select the grant this order should be placed on")
      ok <- FALSE
    }
    return(ok)
  }

  observeEvent(input$save_request, {

    # save if inputs are good
    if (check_request_inputs()) {

      # disable inputs while saving
      c("grant_id", "note", "save_request", "urgent") |> purrr::walk(shinyjs::disable)

      # FIXME: maybe instead of allowing doubles here
      # includes this in the checks
      ids <- inventory$get_selected_items()$item_id
      quantities <- purrr::map_dbl(
        ids,
        ~input[[paste0("quantity_", .x)]]
      ) |> as.integer()

      # add new orders
      if (any(quantities > 0L)) {
        log_info(ns = ns, "adding ", sum(quantities > 0L), " order requests")
        data$orders$start_add()
        for (i in seq_along(ids)) {
          if(quantities[i] > 0L) {
            data$orders$update(
              item_id = ids[i],
              quantity = quantities[i],
              grant_id = as.integer(input$grant_id),
              urgent = as.logical(input$urgent),
              notes = input$notes,
              requested_by = data$get_active_user_data()$user_id,
              requested_on = lubridate::now()
            )
          }
        }
      }

      # commit changes
      if (data$orders$commit()) removeModal()
    }
  })

}

# inventory user interface ------
module_inventory_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
