% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_run.R
\name{start_app}
\alias{start_app}
\title{Run the user interface}
\usage{
start_app(
  data_sheet_id,
  data_folder_id,
  gs_key_file,
  timezone = Sys.timezone(),
  user_id,
  log = TRUE,
  debug = FALSE,
  port = 5000
)
}
\arguments{
\item{data_sheet_id}{google data sheet id}

\item{data_folder_id}{google drive folder id}

\item{gs_key_file}{in json format}

\item{port}{the localhost port where the app will be accessible, e.g. https://127.0.0.1:5000 (note that if it is a port that is open in your firewall such as 3838, the GUI will be accessible on your local network at your IP address https://xxx.xxx.xxx.xxx:3838)}
}
\description{
Run the user interface
}
