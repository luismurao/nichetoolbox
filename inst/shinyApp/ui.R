ui_files <- list.files(path = "ui_layers",pattern = "*.R$",full.names = TRUE)
load_ui <- sapply(ui_files,source)
source("load_install_pkgs.R")
shinyUI(
  navbarPage(theme = shinytheme("cerulean"),
             HTML('<div title="Saves state to workflow directory">
                        <strong>NicheToolBox</strong>
                            <button id="saveState" type="button" class="btn action-button btn-link">
                                        <i class="glyphicon glyphicon-hdd"></i>
                                           Save state</button></div>'),

             id = "nb",
             tabPanel("AppSettings",materials),
             navbarMenu("Data",
                        tabPanel("GBIF data",
                                 tabsetPanel(
                                   tabPanel(gbif_data,title = "Search occs"),
                                   tabPanel(gibif_vis,title = "GBIF visualizations")
                                 )),
                        tabPanel("User data",user_data),
                        tabPanel("Dynamic Map",DynamicMap)


                        ),
             navbarMenu("Niche space",
                        tabPanel("Niche data extraction",define_M),
                        tabPanel("Known niche",niche_view)
                        ),
             navbarMenu("Niche clustering",
                        tabPanel("K-means",ui_kmeans)
                        ),
             navbarMenu("Niche correlations",
                        tabPanel("Strong correlations",strong_corre),
                        tabPanel("Table",corre_table_1),
                        tabPanel("Plot",corre_plot_1)
                        ),
             navbarMenu("ENM",
                        tabPanel("Ellipsoids in G",ellipsoid_inter),
                        tabPanel("Ellipsoids in E",ellipsoid_Graph)
             )
  )
)
