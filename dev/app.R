# DO NOT CHANGE #
# file used for autoreaload during app development
# to use: follow instructions in Rakefile
devtools::load_all("..")

# # write the private key into the key file from an environmental variable
# lines <- readLines("gdrive_access_key.json")
# lines <- gsub(
#   "{{PRIVATE_KEY}}",
#   Sys.getenv("ORDERING_GS_PRIVATE_KEY"),
#   lines,
#   fixed = TRUE
# )
# writeLines(lines, "gdrive_access_key.json")

# process a typical groups string
groups <- "team-geom,team-mcfarlin,admin" |>
  stringr::str_split_1(stringr::fixed(",")) |>
  stringr::str_to_upper()
is_admin <- "ADMIN" %in% groups
user_groups <- groups |>
  stringr::str_subset("^TEAM-") |>
  stringr::str_remove("^TEAM-")


# example app
start_app(
  data_sheet_id = readLines("gdrive_file_key.txt")[1],
  data_folder_id = readLines("gdrive_file_key.txt")[2],
  gs_key_file = "gdrive_access_key.json",
  user_id = "dev",
  user_is_admin = is_admin,
  user_groups = user_groups,
  user_name = "Test",
  timezone = Sys.timezone(),
  options = list(port = 5555)
)
