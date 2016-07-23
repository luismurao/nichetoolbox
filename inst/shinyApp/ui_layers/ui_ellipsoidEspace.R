ellipsoid_Graph <- div(class="outer",

                       tags$head(
                         # Include our custom CSS
                         includeCSS("ui_layers/style/styles.css"),
                         includeScript("ui_layers/style/gomap.js")
                       ),

                       #uiOutput("myConditionalPanel"),
                       conditionalPanel("input.biosEllip.length == 3",
                                        rglwidgetOutput("Ellip3D",
                                                        width =  "650px",
                                                        height  = "650px")
                       ),

                       conditionalPanel("input.biosEllip.length == 2",
                                        plotOutput("Ellip2D")
                       ),

                       h2("Niche model"),


                       # Shiny versions prior to 0.11 should use class="modal" instead.
                       absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                     draggable = TRUE, top = 45, left = "auto", right = 1, bottom = "auto",
                                     width = 340, height = "auto",

                                     h2("NicheToolBox"),
                                     h4("Ellipsoid Modeling\n for SDM"),
                                     actionButton("showEllipEnv","Show me the Niche","primary"),
                                     busyIndicator("Computation In progress",wait = 0),
                                     h5("Download Ellipsoid Meta-Data"),
                                     downloadButton("downShapMat","Download")
                                     #h5("Download centroid"),
                                     #downloadButton("downCentroid","Download centroid coords")


                       )

)
