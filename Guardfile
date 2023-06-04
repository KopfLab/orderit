# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

guard 'process', name: 'Shiny', command: ['R', '-e', " \
devtools::load_all('.'); \
data_sheet_id <- readLines('gdrive_file_key.txt')[1]; \
data_folder_id <- readLines('gdrive_file_key.txt')[2]; \
gs_key_file <- 'gdrive_access_key.json'; \
orderit:::start_app(data_sheet_id, data_folder_id, gs_key_file, user_id = 'test', debug = TRUE)"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 5 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
