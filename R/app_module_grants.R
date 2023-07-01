# grants server ----
module_grants_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # reactive values
  values <- reactiveValues(
    dialog = NULL
  )

  # generate UI =====================
  output$main <- renderUI({
    req(data$is_authenticated())
    log_info(ns = ns, "rendering grants UI")
    tagList(
      shinydashboard::box(
        title = span(
          icon("coins"), "Grants",
          div(
            style = "position: absolute; right: 10px; top: 5px;",
            actionButton(ns("add"), "New Grant", icon = icon("plus"), style = "border: 0;") |>
              add_tooltip("Add a new grant."),
            actionButton(ns("edit"), "Edit Grant", icon = icon("pen"), style = "border: 0;") |>
              add_tooltip("Edit the selected grant."),
            module_selector_table_columns_button(ns("grants_table"), border = FALSE),
            module_selector_table_search_button(ns("grants_table"), border = FALSE)
          )
        ), width = 12,
        status = "info", solidHeader = TRUE,
        module_selector_table_ui(ns("grants_table"))
      )

    )
  })

  # data for table ======
  get_grants <- reactive({
    validate(
      need(data$grants$get_data(), "something went wrong retrieving the data"),
      need(data$users$get_data(), "something went wrong retrieving the data"),
      need(data$get_active_user_data(), "something went wrong retrieving the data")
    )

    df <- data$grants$get_data() |>
      # grants in the same groups as the user
      dplyr::filter(.data$group %in% data$get_active_user_data()$groups) |>
      # bring in approver and orderer names
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("approver_", .x), dplyr::everything()),
        by = "approver_user_id"
      ) |>
      dplyr::left_join(
        data$users$get_data() |> dplyr::rename_with(~paste0("orderer_", .x), dplyr::everything()),
        by = "orderer_user_id"
      ) |>
      dplyr::arrange(.data$status, tolower(.data$name))

    return(df)
  })

  # data table ==============
  grants <- callModule(
    module_selector_table_server,
    "grants_table",
    get_data = get_grants,
    id_column = "grant_id",
    available_columns = list(
      Grant = name, Status = status, `Speed Type` = identifier,
      `Orderer` = paste(orderer_first_name %then% "", orderer_last_name %then% ""),
      `Needs approval` =
        dplyr::case_when(
          is.na(approval_cutoff) ~ "no",
          approval_cutoff == 0 ~ "always",
          TRUE ~ paste(">", scales::label_dollar()(approval_cutoff))
        ),
      `Approver` = paste(approver_first_name %then% "", approver_last_name %then% "")
    ),
    allow_view_all = TRUE,
    initial_page_length = -1,
    selection = "single"
  )

  # add/edit dialog ========
  dialog_inputs <- reactive({
    # note: could use input$xyz to restore the previous input values
    # but that might lead to more confusion than it's worth
    log_debug(ns = ns, "generating dialog inputs")
    users <- data$users$get_data() |>
      dplyr::filter(
        # can only select admins in the same main group as the authenticated user
        .data$role == "admin",
        grepl(data$get_active_user_data()$groups[1], .data$groups, fixed = TRUE)
      ) |>
      dplyr::transmute(
        name = paste(.data$first_name, .data$last_name),
        user_id = .data$user_id
      ) |>
      dplyr::arrange(.data$name) |>
      tibble::deframe()
    users <- c(c("Select user" = ""), users)
    tagList(
      checkboxInput(ns("status"), "Active"),
      textInput(ns("group"), "Group") |> shinyjs::disabled(),
      textInput(ns("name"), "Name"),
      textInput(ns("identifier"), "Speed Type"),
      selectInput(ns("orderer_user_id"), "Orderer", choices = users),

      checkboxInput(ns("needs_approval"), "Orders need approval"),

      conditionalPanel(
        ns = ns,
        condition = "input.needs_approval == true",
        selectInput(ns("approver_user_id"), "Approver", choices = users),
        numericInput(ns("approval_cutoff"), "Orders over this $ amount require approval",
                     value = 0, min = 0, step = 100)
      )
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
    data$grants$start_add()
    updateTextInput(inputId = "group", value = data$get_active_user_data()$groups[1])
    updateCheckboxInput(inputId = "status", value = TRUE)
    updateTextInput(inputId = "name", placeholder = "Enter name of new grant")
    updateTextInput(inputId = "identifier", placeholder = "Enter speedtype for new grant")
    updateSelectInput(inputId = "orderer_user_id", selected = "")
    updateSelectInput(inputId = "approver_user_id", selected = "")
    updateNumericInput(inputId = "approval_cutoff", value = 0)
    showModal(create_dialog("Add new grant"))
  })

  # edit ===========

  observe({
    # edit available only when exactly 1 record selected
    toggle <- nrow(grants$get_selected_items()) == 1L
    shinyjs::toggleState("edit", condition = toggle)
  })

  observeEvent(input$edit, {
    req(grant <- grants$get_selected_items())
    data$grants$start_edit(id = grant$grant_id)
    updateTextInput(inputId = "group", value = grant$group)
    updateCheckboxInput(inputId = "status", value = grant$status == "active")
    updateTextInput(inputId = "name", value = grant$name)
    updateTextInput(inputId = "identifier", value = grant$identifier)
    updateSelectInput(inputId = "orderer_user_id", selected = grant$orderer_user_id)
    updateCheckboxInput(inputId = "needs_approval", value = !is.na(grant$approver_user_id))
    updateSelectInput(inputId = "approver_user_id", selected = grant$approver_user_id)
    updateNumericInput(inputId = "approval_cutoff", value = grant$approval_cutoff)
    showModal(create_dialog("Edit grant"))
  })

  # check inputs ====
  check_inputs <- function() {
    log_info(ns = ns, "checking inputs")
    ok <- TRUE
    # input checks
    if (input$needs_approval && (is.na(input$approval_cutoff) || input$approval_cutoff < 0)) {
      log_warning(ns = ns, "invalid approval cutoff", user_msg = "Approval cutoff: please enter a valid number for the approval cutoff")
      ok <- FALSE
    }

    if (input$needs_approval && nchar(input$approver_user_id) == 0) {
      log_warning(ns = ns, "no approver selected", user_msg = "Approver: please select someone who will approve orders for this grant (or de-select the needs approval option)")
      ok <- FALSE
    }

    if (nchar(input$orderer_user_id) == 0) {
      log_warning(ns = ns, "no orderer selected", user_msg = "Orderer: please select someone who will place the orders for this grant")
      ok <- FALSE
    }

    if (nchar(input$name) == 0) {
      log_warning(ns = ns, "no name provided", user_msg = "Name: please enter a name for the grant")
      ok <- FALSE
    }
    return(ok)
  }

  # save =====
  observeEvent(input$save, {

    # save if inputs are good
    if (check_inputs()) {

      # disable inputs while saving
      c("status", "name", "identifier", "orderer_user_id", "needs_approval", "approver_user_id", "approval_cutoff", "save") |>
        purrr::walk(shinyjs::disable)

      # update data
      data$grants$update(
        group = input$group,
        status = if (input$status) "active" else "inactive",
        name = input$name,
        identifier = input$identifier,
        orderer_user_id = input$orderer_user_id,
        approver_user_id =
          if(input$needs_approval) input$approver_user_id else NA_character_,
        approval_cutoff =
          if(input$needs_approval) input$approval_cutoff else NA_real_
      )

      # commit data and remove modal if it worked
      # FIXME: actually if there was an error, the modal is already
      # replaced with the error modal - should this be fixed?
      if (data$grants$commit()) removeModal()
    }
  })


}

module_grants_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
