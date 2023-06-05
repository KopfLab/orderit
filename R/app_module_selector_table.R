# Selector table

#' Selector table server
#'
#' This generates an rhandson table for selecting items to include in downstream operations.
#'
#' @param get_data reactive context providing the data set
#' @param id_column name of the ID column - must have unique values!! make a rownumber or concatenated column if there is no unique identifier, this column does NOT have to be part of show_columns (but can be)
#' @param show_columns list of transmute statements to select columns to show
#' @param page_lengths page length options, first one will be selected
#' @param initial_page_length initially selected page length, first entry of the page_lengths by default
#' @param dom the available table control elements and their order
#' @param selector_buttons whether the selector buttons are present
#' @family selector table module functions
module_selector_table_server <- function(
    input, output, session, get_data,
    id_column,
    show_columns = list(dplyr::across(dplyr::everything(), identity)),
    page_lengths = list( c(5, 10, 20, -1),  c("5", "10", "20", "All")),
    initial_page_length = page_lengths[[1]][1], dom = "fltip", selector_buttons = TRUE) {

  # safety checks
  stopifnot(!missing(get_data))
  stopifnot(!missing(id_column))

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    all_ids = c(),
    selected_ids = c(),
    table = NULL, # what is available
    selected = c(), # what is selected
    update_selected = 0, # trigger selection update (circumventing circular triggers with user selection)
    page_length = initial_page_length, # selected page length
    display_start = 0, # which display page to start on
    search = "", # search term
    order = list() # ordering information
  )

  # render table ========
  output$selection_table <- DT::renderDataTable({
    req(get_data())
    validate(need(has_data(), "No data available"))
    log_info(ns = ns, "initializing selection table", user_msg = "Loading table")

    # get the table
    table <-
      tryCatch(
        isolate({

          # id values
          values$all_ids <- get_data()[[id_column]]
          if(any(duplicated(values$all_ids))) cli::cli_abort("found duplicate IDs in data table")

          # table df
          transmute_cols <- rlang::call_args(rlang::enquo(show_columns))
          df <- get_data() |>
            dplyr::transmute(!!!transmute_cols) |>
            as.data.frame()
          rownames(df) <- values$all_ids

          # make sure selection stays the same
          # update_selected()
          # generate data table
          DT::datatable(
            data = df,
            rownames = FALSE,
            options = list(
              ordering = values$order,
              pageLength = values$page_length,
              search = list(regex = FALSE, caseInsensitive = TRUE, search = values$search),
              displayStart = values$display_start,
              lengthMenu = page_lengths,
              searchDelay = 100,
              dom = dom,
              # save state to get ordering and other information
              stateSave = TRUE,
              # disable the automatic state reload to avoid issues between different table instances
              stateLoadParams = DT::JS("function (settings, data) { return false; }")
            )
          )
        }),
        error = function(e) {
          # try catch error
          log_error(ns = ns, user_msg = "Data table couldn't be created", error = e)
          return(NULL)
        })

      # return
      validate(need(table, "Data table couldn't be created"))
      log_success(ns = ns, "selector table complete", user_msg = "Complete")
      return(table)
    }
  )

  # trigger selection updates
  update_selected <- function() values$update_selected <- values$update_selected + 1
  # observeEvent(values$update_selected, {
  #   log_debug(ns = ns, "updating selections in selection table")
  #   proxy <- DT::dataTableProxy("selection_table")
  #   DT::selectRows(proxy, which(get_data()[[id_column]] %in% values$selected))
  # })

  # save state
  observeEvent(input$selection_table_state, {
    # isolate({
    #   log_debug(ns = ns, "updating state of selection table")
    #   values$page_length <- input$selection_table_state$length
    #   values$display_start <- input$selection_table_state$start
    #   values$search <- input$selection_table_state$search$search
    #   values$order <- input$selection_table_state$order
    # })
  })

  # save selection ========
  observeEvent(input$selection_table_rows_selected, {
    req(has_data())
    selected_indices <- input$selection_table_rows_selected
    selected_ids <- values$all_ids[selected_indices]
    if (!identical(selected_ids, values$selected_ids)) {
      # there were changes
      values$selected_ids <- selected_ids
      if (length(selected_indices) > 0L)
        log_debug(
          ns = ns, "updating selections to: ",
          sprintf("#%d = '%s'", selected_indices, selected_ids) |>
            paste0(collapse = ", ")
        )
      else
        log_debug(ns = ns, "updating selections to nothing selected")
    }
  }, ignoreNULL = FALSE)

  # selector buttons
  if (selector_buttons) {
    # select all that match the current filter
    observeEvent(input$select_all, {
      values$selected <- unique(c(values$selected, get_data()[[id_column]][input$selection_table_rows_all]))
      update_selected()
    })

    # deselect all
    observeEvent(input$deselect_all, {
      values$selected <- c()
      update_selected()
    })

    # data available?
    has_data <- reactive({
      return(!is.null(get_data()) && nrow(get_data()) > 0L)
    })

    # button visibility
    observeEvent(has_data(), {
      shinyjs::toggle("select_all", condition = has_data())
      shinyjs::toggle("deselect_all", condition = has_data())
    })
  }

  # functions
  has_data <- reactive({
    return(!is.null(get_data()) && nrow(get_data()) > 0L)
  })

  set_selected <- function(selected) {
    isolate({
      if (!identical(selected, values$selected) && (length(selected) > 0 || length(values$selected) > 0)) {
        values$selected <- selected
        update_selected()
      }
    })
  }

  get_selected <- reactive({
    # make sure all returned selected are valid
    if (length(values$selected) == 0) return(c())
    else return(values$selected[values$selected %in% values$table[[id_column]]])
  })

  get_selected_items <- reactive({
    # get the actual table items that are selected
    values$table[values$table[[id_column]] %in% values$selected, ]
  })

  # return functions
  list(
    set_selected = set_selected,
    get_selected = get_selected,
    get_selected_items = get_selected_items
  )
}


#' Selector table UI
#' @family selector table module functions
module_selector_table_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("selection_table"))
}

#' Selector table buttons
#' @family selector table module functions
module_selector_table_buttons <- function(id) {
  ns <- NS(id)
  tagList(
    tooltipInput(actionButton, ns("select_all"), "Select all",
                 icon = icon("check-square-o", verify_fa = FALSE),
                 tooltip = "Select all items that match the current search in addition to those already selected."),
    spaces(1),
    tooltipInput(actionButton, ns("deselect_all"), "Deselect",
                 icon = icon("square-o", verify_fa = FALSE),
                 tooltip = "Deselct all items (irrespective of the search).")
  )
}
