# Selector table

#' Selector table server
#' @param get_data reactive context providing the data set
#' @param id_column name of the ID column - must have unique values!! make a rownumber or concatenated column if there is no unique identifier, this column does NOT have to be part of show_columns (but can be)
#' @param show_columns list of transmute statements to select columns to show
#' @param allow_view_all whether to allow the "all" option in the page lengths, default is FALSE
#' @param page_lengths page length options, first one will be selected
#' @param initial_page_length initially selected page length, first entry of the page_lengths by default
#' @param dom the available table control elements and their order
#' @param filter whether to include column filters - note that this does NOT work for restoring after reload so use with caution if that's a desired feature
#' @param class styling of table see class parameter for datatable
#' @param selection see parameter for dat table
#' @family selector table module functions
module_selector_table_server <- function(
    input, output, session, get_data,
    id_column,
    show_columns = list(dplyr::across(dplyr::everything(), identity)),
    allow_view_all = FALSE,
    page_lengths = list( c(5, 20, 50, 100, if(allow_view_all) -1),
                         c("5", "20", "50", "100", if(allow_view_all) "All")),
    initial_page_length = page_lengths[[1]][1],
    dom = "fltip",
    filter = c("none", "bottom", "top"),
    editable = list(),
    class = "cell-border stripe hover order-column",
    selection = c("multiple", "single", "none")
  ) {

  # safety checks
  stopifnot(!missing(get_data))
  stopifnot(!missing(id_column))

  # namespace
  ns <- session$ns

  # reactive values =========
  values <- reactiveValues(
    all_ids = c(),
    selected_ids = c(),
    update_selected = -1L, # trigger selection update (circumventing circular triggers with user selection)
    selected_cols = c(),
    page_length = initial_page_length, # selected page length
    display_start = 0, # which display page to start on
    search = "", # search term
    order = list(), # ordering information
    columns = NULL # column details (including filters)
  )

  # create table df =============
  get_table_df <- reactive({
    req(get_data())
    validate(need(has_data(), "No data available"))
    log_info(ns = ns, "preparing table df with new data", user_msg = "Loading table")
    # get the table
    df <-
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

          # select the transmuted cols to begin with
          isolate({
            if (length(values$selected_cols) == 0)
              values$selected_cols <- seq_along(names(df))
          })

          return(df)
        }),
        error = function(e) {
          # try catch error
          log_error(ns = ns, user_msg = "Data could not be processed", error = e)
          return(NULL)
        })

    # return
    return(df)
  })

  get_table_df_selected_cols <- reactive({
    return(get_table_df()[values$selected_cols])
  })

  # render data table ========
  output$selection_table <- DT::renderDataTable({
    # trigger
    get_table_df_selected_cols()
    log_debug(ns = ns, "rendering selection table")

    # get the table
    table <-
      tryCatch(
        isolate({
          # generate data table
          DT::datatable(
            data = get_table_df_selected_cols(),
            rownames = FALSE,
            filter = filter,
            editable = editable,
            class = class,
            selection = selection,
            autoWidth = TRUE,
            options = list(
              order = values$order,
              pageLength = values$page_length,
              search = list(regex = FALSE, caseInsensitive = TRUE, search = values$search),
              displayStart = values$display_start,
              lengthMenu = page_lengths,
              searchDelay = 100,
              dom = dom,
              #columns= values$columns, # this does not work to restore the search, breaks the table instead
              # could maybe do it in javascript, ideas here: https://datatables.net/forums/discussion/53287/how-to-reset-values-in-individual-column-searching-text-inputs-at-a-button-click
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

      # wrap up
      validate(need(table, "Data table couldn't be created"))
      isolate(update_selected()) # make sure selection stays the same
      log_success(ns = ns, "selector table complete", user_msg = "Complete")
      return(table)
    },
    # make sure this is executed server side
    server = TRUE
  )

  # save selection ========
  observeEvent(input$selection_table_rows_selected, {
    req(has_data())
    select_rows(indices = input$selection_table_rows_selected)
  }, ignoreNULL = FALSE)

  get_id_from_index <- function(indices) {
    return(values$all_ids[indices])
  }

  get_index_from_id <- function(ids) {
    return(which(values$all_ids %in% ids))
  }

  clean_ids <- function(ids) {
    # only return those not duplicated and actually in the dataset
    return(get_id_from_index(get_index_from_id(omit_duplicates(ids))))
  }

  select_rows <- function(ids = get_id_from_index(indices), indices = NULL) {
    ids <- clean_ids(ids)
    if (!identical(ids, values$selected_ids)) {
      # there were actual changes
      values$selected_ids <- ids
      if (length(ids) > 0L)
        log_debug(
          ns = ns, "saving selections: ",
          sprintf("#%d = '%s'", get_index_from_id(ids), ids) |> paste0(collapse = ", ")
        )
      else
        log_debug(ns = ns, "saving selections: nothing")
    }
  }

  # update selection =========
  update_selected <- function() values$update_selected <- values$update_selected + 1L
  observeEvent(values$update_selected, {
    if (values$update_selected > 0) {
      log_debug(ns = ns, "updating selections in selection table")
      proxy <- DT::dataTableProxy("selection_table")
      DT::selectRows(proxy, get_index_from_id(values$selected_ids))
    }
  })

  # select all event ======
  observeEvent(input$select_all, {
    select_rows(ids = c(values$selected_ids, get_id_from_index(input$selection_table_rows_all)))
    update_selected()
  })

  # deselect all event ======
  observeEvent(input$deselect_all, {
    select_rows(c())
    update_selected()
  })

  # pick columns event =====
  observeEvent(input$pick_cols, {
    req(get_data())
    dlg <- modalDialog(
      title = "Show columns",
      checkboxGroupInput(
        ns("selected_cols"), label = NULL,
        choiceNames = names(get_table_df()),
        choiceValues = seq_along(names(get_table_df())),
        selected = values$selected_cols
      ),
      footer = tagList(
        actionButton(ns("apply_cols"), "Apply") |>
          add_tooltip("Switch to showing the selected column(s). Note that if the search is based on a column that is removed, different rows will show."),
        spaces(1),
        modalButton("Cancel")
      )
    )
    showModal(dlg)
  })
  observeEvent(input$apply_cols, {
    selected_cols <- as.integer(input$selected_cols)
    if (!identical(selected_cols, values$selected_cols)) {
      # got some new columns
      values$selected_cols <- selected_cols
      log_info(
        ns = ns, "selecting table columns: ",
        sprintf(
          "%d (%s)",
          values$selected_cols,
          names(get_table_df())[values$selected_cols]
        ) |> paste(collapse = ", "),
        user_msg = "Switching columns"
      )
    }
    removeModal()
  })

  # save state ========

  # save state
  observeEvent(input$selection_table_state, {
    log_debug(ns = ns, "saving state of selection table")
    values$page_length <- input$selection_table_state$length
    values$display_start <- input$selection_table_state$start
    values$search <- input$selection_table_state$search$search
    values$order <- input$selection_table_state$order
    #values$columns <- input$selection_table_state$columns
    # Note: this doesn't work to restore the search fields
  })

  # retrieve data ======
  has_data <- reactive({
    return(!is.null(get_data()) && nrow(get_data()) > 0L)
  })

  get_selected_ids <- reactive({
    return(values$selected_ids)
  })

  get_selected_items <- reactive({
    # get the actual table items that are selected
    return(get_data()[get_index_from_id(values$selected_ids),])
  })

  # enable buttons =====
  observe({
    toggle <- has_data() & length(input$selection_table_rows_all) > 0
    if (isolate(!is.null(input$select_all))) {
      shinyjs::toggleState("select_all", condition = toggle)
    }
  })
  observe({
    toggle <- has_data()
    if (isolate(!is.null(input$deselect_all))) {
      shinyjs::toggleState("deselect_all", condition = toggle)
    }
  })
  observe({
    toggle <- has_data() & length(input$selected_cols) > 0
    if (isolate(!is.null(input$apply_cols))) {
      shinyjs::toggleState("apply_cols", condition = toggle)
    }
  })

  # return functions
  list(
    select_rows = select_rows,
    get_selected_ids = get_selected_ids,
    get_selected_items = get_selected_items
  )
}


#' Selector table UI
module_selector_table_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("selection_table"))
}

#' Selector table buttons
module_selector_table_buttons <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("select_all"), "Select all", icon = icon("square-check")) |>
      add_tooltip("Select all items that match the current search in addition to those already selected."),
    spaces(1),
    actionButton(ns("deselect_all"), "Deselect", icon = icon("square")) |>
      add_tooltip("Deselect all items (even those not visible in the current search)")
  )
}

#' Selector column selector
module_selector_table_columns_button <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("pick_cols"), "Columns", icon = icon("gear")) |>
      add_tooltip("Pick which columns to show")
  )
}


