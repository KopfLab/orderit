# user interface
ui <- function(timezone = NULL) {

  # constants
  app_title <- packageName()
  app_title_width <- 150
  app_sidebar_width <- app_title_width
  app_color <- "yellow"
  spinner_color <- "#2c3b41"
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = spinner_color)

  # header
  header <- shinydashboard::dashboardHeader(title = textOutput("user_first_name"), titleWidth = app_title_width)

  # sidebar
  sidebar <- shinydashboard::dashboardSidebar(
    width = app_sidebar_width,
    div(id = "menu",
      shinydashboard::sidebarMenu(
        id = "nav",
        h5("App version", as.character(packageVersion(packageName())), align = "center"),
        if (!is.null(timezone)) h5(timezone, align = "center"),
        shinydashboard::menuItem(
          "Orders", icon = icon("bolt"), startExpanded = TRUE,
          shinydashboard::menuSubItem("Requests", icon = icon("cart-shopping"), tabName = "requested"),
          shinydashboard::menuSubItem("Ordered", icon = icon("truck"), tabName = "ordered"),
          shinydashboard::menuSubItem("Received", icon = icon("check"), tabName = "received")
        ),
        shinydashboard::menuItem("Inventory", tabName = "inventory", icon = icon("flask")),
        shinydashboard::menuItem("Grants", tabName = "grants", icon = icon("coins"))
      )
    ) |> shinyjs::hidden(),
    module_data_reload_button("data"),
    module_data_download_button("data"),
    shinyjs::useShinyjs(), # enable shinyjs
    shinytoastr::useToastr(), # enable toaster
    prompter::use_prompt(), # enable prompter
    tags$head( # css headers
      # custom
      tags$style(
        type="text/css",
        HTML(paste(
          # navbar download button
          ".skin-yellow .sidebar .shiny-download-link { color: #444; margin-left: 15px; margin-top: 5px; }",
          ".sidebar-menu li a { font-size: 16px; }",
          # NOTE: I don't think this works
          ".treeview-menu li a { font-size: 16px; }",
          # error validation output
          ".shiny-output-error-validation { color: red; font-size: 20px; padding: 20px; }", # do we want this red?
          ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
          # body top padding
          ".box-body {padding-top: 5px; padding-bottom: 0px}",
          # pads on shiny items
          ".form-group, .selectize-control {margin-bottom: 7px;}", #Padding in input widgets
          ".form-group, .selectize-control {margin-top: 2px;}",
          # custom background box
          sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", app_box_default, app_box_default),
          sprintf(".box.box-solid.box-info{border:1px solid %s;}", app_box_default),
          sep="\n"))
      )
    )
  )

  # body
  body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        "requested",
        module_orders_requested_ui(id = "orders")
      ),
      shinydashboard::tabItem(
        "ordered",
        module_orders_ordered_ui(id = "orders")
      ),
      shinydashboard::tabItem(
        "received",
        module_orders_received_ui(id = "orders")
      ),
      shinydashboard::tabItem(
        "inventory",
        module_inventory_ui(id = "inventory")
      ),
      shinydashboard::tabItem(
        "grants",
        module_grants_ui(id = "grants")
      )
    )
  )

  # dashboard page
  shinydashboard::dashboardPage(
    title = app_title, # tab title
    skin = app_color, # styling
    header = header,
    sidebar = sidebar,
    body = body
  )

}
