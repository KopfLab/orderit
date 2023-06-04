# inventory server ----
module_grants_server <- function(input, output, session, data) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    data = NULL
  )

  # UI
  output$main <- renderUI({
    validate(need(data$authenticated(), "Authentication failed"))
    log_info(ns = ns, "rendering grants UI")
    tagList(
      h2(icon("sack-dollar"), "Grants"),
      DT::dataTableOutput(ns("grants_table"))
    )
  })

  # grants table
  output$grants_table <- DT::renderDataTable({
    req(df <- data$get_grants_data())
    validate(need(nrow(df)  > 0L, "No grants available."))
    isolate({
      log_info(ns = ns, "rendering grants table")
      # id col
      row_names <- df$grant_id
      df$grant_id <- NULL
      # main cols
      df <- df |> dplyr::transmute(
        Grant = name, Status = status, `Speed Type` = speed_type,
        `PI` = paste(pi_first_name %then% "", pi_last_name %then% ""),
        `Orderer` = paste(orderer_first_name %then% "", orderer_last_name %then% "")
      )
      # generate data table
      DT::datatable(
        data = df
        # options = list(
        #   ordering = values$order,
        #   pageLength = values$page_length,
        #   search = list(regex = FALSE, caseInsensitive = TRUE, search = values$search),
        #   displayStart = values$display_start,
        #   lengthMenu = page_lengths,
        #   searchDelay = 100,
        #   dom = dom,
        #   # save state to get ordering and other information
        #   stateSave = TRUE,
        #   # disable the automatic state reload to avoid issues between different table instances
        #   stateLoadParams = DT::JS("function (settings, data) { return false; }")
        # )
      )
    })},
    # small table --. client side is faster
    server = FALSE
  )


}

# inventory user interface ------
module_grants_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("main")) |> shinycssloaders::withSpinner()
}
