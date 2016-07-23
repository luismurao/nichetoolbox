ellipsoid_inter <- div(class="outer",

                       tags$head(
                         # Include our custom CSS
                         includeCSS("ui_layers/style/styles.css")#,
                         #includeScript("gomap.js")
                       ),

                       #plotOutput("EllipRaster",width = "1000px",height = "600px"),
                       leafletOutput(outputId = "ellip_map",width="100%", height="100%"),

                       # Shiny versions prior to 0.11 should use class="modal" instead.
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                     draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                     width = 340, height = "auto",

                                     h2("NicheToolBox"),
                                     h4("Ellipsoid Modeling\n for ENM"),

                                     #checkboxInput("showHeatM",label = "Show Heat Map",value = TRUE),
                                     #selectInput("res1",label = "Select a data set",
                                     #            choices = c("User data"="userData","GBIF data"="gbifData","M data"="mData")),

                                     selectInput("selectM","Select a region to project the ellipsoid",
                                                 choices = c("All raster extent"="wWorld","Your shapefile of M"="mLayers")),

                                     #selectInput(inputId = "biosEllip","Select the variables",choices = names(rstack),multiple = TRUE),
                                     selectInput(inputId = "biosEllip","Select the variables",choices = NULL,multiple = TRUE),
                                     br(),
                                     numericInput("prop_points",
                                                  "Proportion of niche points inside the ellipsoid",
                                                  value = 0.95,min = 0.5,max=0.99),
                                     br(),
                                     h5("Run your model"),
                                     shiny::actionButton("selectBios",label = "Run model"),
                                     busyIndicator("Computation In progress",wait = 0),
                                     h5("Download your model in .asc"),
                                     downloadButton(outputId = "downEllipRas",label = "Download model"),
                                     h5("Download plot"),
                                     downloadButton(outputId = "EllipRasterPlot",label = "Download plot"),
                                     h5("Enviromental distances to the centroid table"),
                                     downloadButton(outputId = "downEllipDistance",label = "Distance DataTable")
                                     #checkboxInput("showBios","Show bioclim layer",value = FALSE),


                       )

)
