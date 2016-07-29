# Bliclim to NicheToolBox Methods
# ui interface


source("load_install_pkgs.R")
bioclim_ui <- sidebarLayout(position = "left",
                              sidebarPanel(
                                titlePanel("NicheToolBox"),
                                h3("Bioclim"),
                                busyIndicator("Computation In progress",wait = 0),
                                selectInput("trainBio","Select a region to train the model",
                                            choices = c("All raster extent"="wWorld","Your shapefile of M"="mLayers")),
                                selectInput("selectMBio","Select a region to project the model",
                                            choices = c("All raster extent"="wWorld","Your shapefile of M"="mLayers")),
                                selectInput(inputId = "biosBioclim","Select the variables",choices = NULL,multiple = TRUE),
                                conditionalPanel("input.selectMBio == 'wWorld'",
                                                 actionButton("run_bioclim_all",label = "Run Bioclim")
                                                 ),
                                conditionalPanel("input.selectMBio == 'mLayers'",
                                                 actionButton("run_bioclim_m",label = "Run Bioclim")
                                ),
                                h5("Download your model in .asc"),
                                downloadButton(outputId = "downBiclimRas",label = "Download model")

                              ),
                              mainPanel(
                                conditionalPanel("input.selectMBio == 'wWorld'",plotOutput("bio_response_all")),
                                conditionalPanel("input.selectMBio == 'mLayers'",plotOutput("bio_response_m"))
                              ))
