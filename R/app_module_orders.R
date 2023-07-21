# orders server ----
module_orders_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # generate requested UI =====================
  output$main_requested <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering requested UI")
    tagList(
      # requested
      tabPanel(
        "Requested",
        shinydashboard::box(
          title = span(
            icon("cart-shopping"), "Requested",
            div(
              style = "position: absolute; right: 20px; top: 5px;",
              if (data$is_active_user_admin())
                actionButton(ns("mark_ordered"), "Mark ordered", icon = icon("truck"), style = "border: 0;") |>
                add_tooltip("Mark selected item(s) as ordered."),
              actionButton(ns("cancel_requested"), "Cancel request", icon = icon("xmark"), style = "border: 0;") |>
                add_tooltip("Cancel the selected request."),
              actionButton(ns("edit_requested"), "Edit request", icon = icon("pen"), style = "border: 0;") |>
                add_tooltip("Edit the selected request."),
              if (data$is_active_user_admin())
                module_selector_table_selection_buttons(ns("requested_table"), border = FALSE),
              module_selector_table_columns_button(ns("requested_table"), border = FALSE),
              module_selector_table_search_button(ns("requested_table"), border = FALSE)
            )
          ), width = 12,
          status = "info", solidHeader = TRUE, collapsible = TRUE,
          module_selector_table_ui(ns("requested_table"))
        )
      )
    )
  })

  # generate ordered ui ============

  output$main_ordered <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering ordered UI")
    tagList(
      # ordered
      tabPanel(
        "Ordered",
        shinydashboard::box(
          title = span(
            icon("truck"), "Ordered",
            div(
              style = "position: absolute; right: 20px; top: 5px;",
              actionButton(ns("mark_received"), "Mark received", icon = icon("check"), style = "border: 0;") |>
                add_tooltip("Mark selected item(s) as received."),
              if (data$is_active_user_admin())
                actionButton(ns("cancel_ordered"), "Cancel order", icon = icon("xmark"), style = "border: 0;") |>
                add_tooltip("Cancel the selected order."),
              if (data$is_active_user_admin())
                module_selector_table_selection_buttons(ns("ordered_table"), border = FALSE),
              module_selector_table_columns_button(ns("ordered_table"), border = FALSE),
              module_selector_table_search_button(ns("ordered_table"), border = FALSE)
            )
          ), width = 12,
          status = "info", solidHeader = TRUE, collapsible = TRUE,
          module_selector_table_ui(ns("ordered_table"))
        )
      )
    )
  })

  # generated received ui ============

  output$main_received <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering requested UI")
    tagList(
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
        # bring in notified
        dplyr::left_join(
          data$users$get_data() |> dplyr::rename_with(~paste0("notify_", .x), dplyr::everything()),
          by = c("notify_user" = "notify_user_id")
        ) |>
        # make factors for advanced search
        dplyr::mutate(
          vendor = factor(vendor),
          item_status = factor(item_status, levels = names(get_item_status_levels())),
          requester = paste(requester_first_name %then% "", requester_last_name %then% "") |> factor(),
          notify = paste(notify_first_name %then% "", notify_last_name %then% "") |> factor(),
          grant_name = factor(grant_name)
        )
    )
  })

  get_requested <- reactive({
    req(get_orders())
    requested <-
      get_orders() |>
      dplyr::filter(is.na(.data$ordered_on), is.na(.data$canceled_by)) |>
      dplyr::arrange(dplyr::desc(.data$requested_on), .data$order_id) |>
      # bring in orderer
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("orderer_", .x), dplyr::everything()),
        by = c("orderer_user_id" = "orderer_user_id")
      )

    # orderer
    if (nrow(requested) > 0) {
      requested <- requested |>
        dplyr::mutate(
          orderer = paste(orderer_first_name %then% "", orderer_last_name %then% "") |>
            factor()
        )
    } else {
      requested <- requested |>
        dplyr::mutate(orderer = character())
    }
    return(requested)
  })

  get_ordered <- reactive({
    req(get_orders())
    ordered <- get_orders() |>
      dplyr::filter(!is.na(.data$ordered_on), is.na(.data$received_on), is.na(.data$canceled_by)) |>
      dplyr::arrange(dplyr::desc(.data$ordered_on), .data$order_id) |>
      # bring in ordered_by
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("ordered_by_", .x), dplyr::everything()),
        by = c("ordered_by" = "ordered_by_user_id")
      )

    # ordered by
    if (nrow(ordered) > 0) {
      ordered <- ordered |>
        dplyr::mutate(
          ordered_by = paste(ordered_by_first_name %then% "", ordered_by_last_name %then% "") |>
            factor()
        )
    } else {
      ordered <- ordered |>
        dplyr::mutate(ordered_by = character())
    }
    return(ordered)

  })

  get_received <- reactive({
    req(get_orders())
    received <- get_orders() |>
      dplyr::filter(!is.na(.data$received_on), is.na(.data$canceled_by)) |>
      dplyr::arrange(dplyr::desc(.data$received_on), .data$order_id) |>
      # bring in received by
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("received_by_", .x), dplyr::everything()),
        by = c("received_by" = "received_by_user_id")
      )

    # received by
    if (nrow(received) > 0) {
      received <- received |>
        dplyr::mutate(
          received_by = paste(received_by_first_name %then% "", received_by_last_name %then% "") |>
            factor()
        )
    } else {
      received <- received |>
        dplyr::mutate(received_by = character())
    }
    return(received)
  })

  # requested data table ==============
  requested <- callModule(
    module_selector_table_server,
    "requested_table",
    get_data = get_requested,
    id_column = "order_id",
    available_columns = list(
      Flag = ifelse(!is.na(urgent) & urgent, "urgent", ""),
      Item = item_name,
      Status = item_status,
      Vendor = vendor,
      `Catalog #` =
        ifelse(
          !is.na(url) & nchar(url) > 0,
          sprintf(
            "<a href = '%s' target = '_blank'>%s</a> <a href = '%s&Keywords=%s' target = '_blank'><i class=\"fa-solid fa-cart-shopping\"></i></a>",
            gsub("^(http(s?)://)?", "https://", url), htmltools::htmlEscape(catalog_nr),
            get_marketplace_url(), htmltools::htmlEscape(catalog_nr)
          ),
          htmltools::htmlEscape(catalog_nr)
        ),
      Quantity = sprintf("%d x %s", quantity, unit_size),
      Total = quantity * unit_price,
      `Requested by` = requester,
      `Notify` = notify,
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
      list(func = DT::formatCurrency, columns = "Total"),
      list(
        func = DT::formatStyle, columns = "Status",
        backgroundColor = DT::styleEqual(
          get_item_status_levels() |> names(),
          get_item_status_levels() |> as.character()
        )
      ),
      list(
        func = DT::formatStyle, columns = "Flag",
        backgroundColor = DT::styleEqual(
          c("urgent", "no"), c("orange", NA_character_)
        )
      )
    )
  )

  # update number next to the mark ordered button
  observeEvent(requested$get_selected_ids(), {
    updateActionButton(
      inputId = "mark_ordered",
      label =
        if (length(requested$get_selected_ids()) > 0)
          sprintf("Mark ordered (%d)", length(requested$get_selected_ids()))
      else
        "Mark ordered"
    )
  }, ignoreNULL = FALSE)

  # ordered data table ===========
  ordered <- callModule(
    module_selector_table_server,
    "ordered_table",
    get_data = get_ordered,
    id_column = "order_id",
    available_columns = list(
      Flag = ifelse(!is.na(urgent) & urgent, "urgent", ""),
      Item = item_name,
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
      `Requested by` = requester,
      `Notify` = notify,
      `Requested on` = as.character(requested_on),
      `Ordered by` = ordered_by,
      `Ordered on` = as.character(ordered_on),
      Notes = notes
    ),
    allow_view_all = TRUE,
    initial_page_length = 10,
    selection = "multiple",
    render_html = "Catalog #",
    formatting_calls = list(
      list(
        func = DT::formatStyle, columns = "Flag",
        backgroundColor = DT::styleEqual(
          c("urgent", "no"), c("orange", NA_character_)
        )
      )
    )
  )

  # update number next to the mark received button
  observeEvent(ordered$get_selected_ids(), {
    updateActionButton(
      inputId = "mark_received",
      label =
        if (length(ordered$get_selected_ids()) > 0)
          sprintf("Mark received (%d)", length(ordered$get_selected_ids()))
      else
        "Mark received"
    )
  }, ignoreNULL = FALSE)

  # received data table ==========
  received <- callModule(
    module_selector_table_server,
    "received_table",
    get_data = get_received,
    id_column = "order_id",
    available_columns = list(
      Flag = ifelse(!is.na(urgent) & urgent, "urgent", ""),
      Item = item_name,
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
      `Requested by` = requester,
      `Notify` = notify,
      `Requested on` = as.character(requested_on),
      `Received by` = received_by,
      `Received on` = as.character(received_on),
      Notes = notes
    ),
    allow_view_all = FALSE,
    initial_page_length = 10,
    selection = "none",
    render_html = "Catalog #",
    formatting_calls = list(
      list(
        func = DT::formatStyle, columns = "Flag",
        backgroundColor = DT::styleEqual(
          c("urgent", "no"), c("orange", NA_character_)
        )
      )
    )
  )

  # mark ordered =======

  observe({
    toggle <- nrow(requested$get_selected_items()) > 0L
    shinyjs::toggleState("mark_ordered", condition = toggle)
  })

  observeEvent(input$mark_ordered, {
    req(items <- requested$get_selected_items())
    if (nrow(items) > 0L) {
      log_info(ns = ns, "marking ", nrow(items), " requests as ordered")
      data$orders$start_edit(id = items$order_id)
      data$orders$update(
        ordered_by = data$get_active_user_data()$user_id,
        ordered_on = lubridate::now()
      )
      data$orders$commit()
    }
  })


  # mark received ======

  observe({
    toggle <- nrow(ordered$get_selected_items()) > 0L
    shinyjs::toggleState("mark_received", condition = toggle)
  })

  observeEvent(input$mark_received, {
    req(items <- ordered$get_selected_items())
    if (nrow(items) > 0L) {
      log_info(ns = ns, "marking ", nrow(items), " requests as received")
      data$orders$start_edit(id = items$order_id)
      data$orders$update(
        received_by = data$get_active_user_data()$user_id,
        received_on = lubridate::now()
      )
      data$orders$commit()
    }
  })

  # cancel request ====

  observe({
    # only allow delete/edit for single items if admin or the user requested them
    toggle <- nrow(requested$get_selected_items()) == 1L &&
      (
        data$is_active_user_admin() ||
          requested$get_selected_items()[1, "requested_by"] == data$get_active_user_data()$user_id
      )

    shinyjs::toggleState("cancel_requested", condition = toggle)
    shinyjs::toggleState("edit_requested", condition = toggle)
  })

  observeEvent(input$cancel_requested, {
    req(items <- requested$get_selected_items())
    if (nrow(items) > 0L) {
      log_info(ns = ns, "canceling ", nrow(items), " request")
      data$orders$start_edit(id = items$order_id)
      data$orders$update(canceled_by = data$get_active_user_data()$user_id)
      data$orders$commit()
    }
  })

  # edit request ====

  create_request_edit_dialog <- function(item, grants) {
    modalDialog(
      size = "s",
      title = "Edit request",
      selectInput(ns("grant_id"), "Grant", choices = grants, selected = item$grant_id),
      textAreaInput(
        ns("notes"), "Notes",
        value = if (!is.na(item$notes)) item$notes else ""),
      checkboxInput(ns("urgent"), strong("Urgent"), value = item$urgent),
      numericInput(ns("quantity"), "Quantity",
        min = 0, step = 1, value = item$quantity
      ),
      footer = tagList(
        actionButton(ns("save_request"), "Save"),
        modalButton("Cancel")
      )
    )
  }

  observeEvent(input$edit_requested, {
    req(data$grants$get_data())
    log_info(ns = ns, "loading request edit screen")
    dlg <- create_request_edit_dialog(
      requested$get_selected_items()[1,],
      get_grants_list(data$grants$get_data(), data$get_active_user_data())
    )
    showModal(dlg)
  })

  # save request ====

  observeEvent(input$save_request, {
    # disable inputs while saving
    c("grant_id", "note", "urgent", "quantity", "save_request") |>
      purrr::walk(shinyjs::disable)

    # update request
    data$orders$start_edit(id = requested$get_selected_items()$order_id)
    data$orders$update(
      urgent = as.logical(input$urgent),
      quantity = as.integer(input$quantity),
      grant_id = as.integer(input$grant_id),
      notes = input$notes
    )

    # commit changes
    if (data$orders$commit()) removeModal()
  })

  # cancel order ====

  observe({
    # only allow cancel for admins
    toggle <- nrow(ordered$get_selected_items()) == 1L && data$is_active_user_admin()
    shinyjs::toggleState("cancel_ordered", condition = toggle)
  })

  observeEvent(input$cancel_ordered, {
    req(items <- ordered$get_selected_items())
    if (nrow(items) > 0L) {
      log_info(ns = ns, "canceling ", nrow(items), " order")
      data$orders$start_edit(id = items$order_id)
      data$orders$update(canceled_by = data$get_active_user_data()$user_id)
      data$orders$commit()
    }
  })

}

# orders user interface ------
module_orders_requested_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_requested")) |> shinycssloaders::withSpinner()
}

module_orders_ordered_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_ordered")) |> shinycssloaders::withSpinner()
}

module_orders_received_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main_received")) |> shinycssloaders::withSpinner()
}
