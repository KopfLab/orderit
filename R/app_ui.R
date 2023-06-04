# user interface
ui <- function(timezone = NULL) {

  # constants
  app_color <- "yellow"
  app_title <- packageName()
  app_title_width <- 150
  app_sidebar_width <- app_title_width
  app_box_default <- "#2c3b41"

  # options
  options(spinner.color = app_color)

  # header
  header <- shinydashboard::dashboardHeader(title = module_data_user_first_name("data"), titleWidth = app_title_width)

  # sidebar
  sidebar <- shinydashboard::dashboardSidebar(
    width = app_sidebar_width,
    shinydashboard::sidebarMenu(
      id = "nav",
      h5("App version", as.character(packageVersion(packageName())), align = "center"),
      if (!is.null(timezone)) h5(timezone, align = "center"),
      shinydashboard::menuItem("Inventory", tabName = "inventory", icon = icon("dashboard")),
      shinydashboard::menuItem("Orders", tabName = "orders", icon = icon("poo-storm")),
      # FIXME change default
      shinydashboard::menuItem("Grants", tabName = "grants", icon = icon("coins"), selected = T)
    ),
    module_data_reload_button("data"),
    shinytoastr::useToastr(), # enable toaster
    shinyjs::useShinyjs(), # enable shinyjs
    tags$head( # css headers
      tags$style(
        type="text/css",
        HTML(paste(
          # error validation output
          ".shiny-output-error-validation { color: red; font-size: 20px; padding: 20px; }", # do we want this red?
          ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
          # body top padding
          ".box-body {padding-top: 0px; padding-bottom: 0px}",
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
        "inventory",
        tableOutput("users"),
        module_inventory_ui(id = "inventory")
      ),
      shinydashboard::tabItem(
        "orders",
        h4(icon("poo-storm"), "Orders")
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
