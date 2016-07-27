
source("load_install_pkgs.R")
ellipsoid_models_c <- sidebarLayout(position = "left",
                            sidebarPanel(
                              titlePanel("NicheToolBox"),
                              h3("Ellipsoid Model"),
                              busyIndicator("Computation In progress",wait = 0),

                              selectInput("selectM","Select a region to project the ellipsoid",
                                          choices = c("All raster extent"="wWorld","Your shapefile of M"="mLayers")),
                              selectInput(inputId = "biosEllip","Select the variables",choices = NULL,multiple = TRUE),
                              numericInput("prop_points",
                                           "Proportion of niche points inside the ellipsoid",
                                           value = 0.95,min = 0.5,max=0.99),
                              h5("Train the model"),
                              shiny::actionButton("train_ellips",label = "Train model"),
                              h5("Run your model"),
                              conditionalPanel("input.selectM=='wWorld'",
                                               shiny::actionButton("selectBios_all",label = "Run model")
                              ),
                              conditionalPanel("input.selectM=='mLayers'",
                                               shiny::actionButton("selectBios_m",label = "Run model")
                              ),

                              busyIndicator("Computation In progress",wait = 0),
                              h5("Download Ellipsoid Meta-Data"),
                              downloadButton("downShapMat","Download"),
                              h5("Download your model in .asc"),
                              downloadButton(outputId = "downEllipRas",label = "Download model"),
                              #h5("Download plot"),
                              #downloadButton(outputId = "EllipRasterPlot",label = "Download plot"),
                              h5("Enviromental distances to the centroid table"),
                              downloadButton(outputId = "downEllipDistance",label = "Distance DataTable")

                            ),
                            mainPanel(
                              conditionalPanel("input.biosEllip.length == 3 && input.selectM == 'wWorld'",
                                               rglwidgetOutput("Ellip3D_all",
                                                               width =  "650px",
                                                               height  = "650px")
                              ),
                              conditionalPanel("input.biosEllip.length == 3 && input.selectM == 'mLayers'",
                                               rglwidgetOutput("Ellip3D_m",
                                                               width =  "650px",
                                                               height  = "650px")
                              ),

                              conditionalPanel("input.biosEllip.length == 2 && input.selectM == 'wWorld'",
                                               plotOutput("Ellip2D_all")
                              ),
                              conditionalPanel("input.biosEllip.length == 2 && input.selectM == 'mLayers'",
                                               plotOutput("Ellip2D_m")
                              ),
                              conditionalPanel("input.biosEllip.length > 3 && input.selectM == 'wWorld'",
                                               plotOutput("reponse_curves_all")
                              ),
                              conditionalPanel("input.biosEllip.length > 3 && input.selectM == 'mLayers'",
                                               plotOutput("reponse_curves_m")
                              )
                            ))
