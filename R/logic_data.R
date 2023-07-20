# users =====

authenticate_user <- function(users, user_id) {
  user <- users |>
    dplyr::filter(user_id == !!user_id)
  if(nrow(user) < 1L) abort(paste0("user does not exist: ", user_id))
  if(nrow(user) > 1L) abort(paste0("user_id is not unique: ", user_id))
  user <- user |> as.list()
  # split user groups
  user$groups <- strsplit(user$groups, split = ",", fixed = TRUE)[[1]]
  return(user)
}

# grants =======

get_grants_list <- function(grants, active_user_data) {
  grants <- grants |>
    dplyr::filter(.data$group %in% active_user_data$groups) |>
    dplyr::filter(.data$status == "active") |>
    dplyr::select("name", "grant_id") |>
    dplyr::arrange(tolower(.data$name)) |>
    tibble::deframe()
  grants <- c(c("Select grant" = ""), grants)
  return(grants)
}

# item status levels =====

get_item_status_levels <- function() {
  c(
    "current" = "lightgreen",
    "needs confirmation" = "lightyellow",
    "outdated" = "lightpink"
  )
}

# markeplace url =====

get_marketplace_url <- function() {
  "https://solutions.sciquest.com/apps/Router/Home?ParamAction=Search&IsInitialSimpleSearch=true&SourceSimpleSearch=true&VerticalId=0"
}
