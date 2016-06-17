options(shiny.maxRequestSize=550*1024^2)
source("load_install_pkgs.R")
server_files <- list.files(path = "server_funcs",full.names = TRUE)
shinyServer(function(input,output,session){
  source("workflow_events/settings_workflow_obs.R",local = TRUE)
  sapply(server_files,function(x) source(x,local = TRUE))
  })
