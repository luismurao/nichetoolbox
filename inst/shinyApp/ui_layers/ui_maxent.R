ui_maxent  <- sidebarLayout(position = "left",
                            sidebarPanel(
                              tags$head(

                                tags$script(src = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
                              ),
                              titlePanel("NicheToolBox"),
                              h3("MaxEnt"),
                              wellPanel(
                                h4("Main features"),
                                busyIndicator("Computation In progress",wait = 0),
                                selectInput("selectDataMaxEnt","Select a dataset to train the model",
                                            choices =NULL),
                                selectInput("selectM_MaxEnt","Select a region to project the model",
                                            choices = NULL),
                                selectInput(inputId = "biosMaxEnt","Select the variables",choices = NULL,multiple = TRUE),
                                #checkboxGroupInput("maxent_response","Features",choices = c("Linear features"="linear",
                                #                                                            "Quadratic features"="quadratic",
                                #                                                            "Product features"="product",
                                #                                                            "Threshold features"="threshold",
                                #                                                            "Hinge features"="hinge"),
                                #                   selected = c("linear","quadratic","product","threshold","hinge")),
                                strong(p("Features")),
                                checkboxInput("max_linear","Linear features",value = TRUE),
                                checkboxInput("max_quadratic","Quadratic features",value = TRUE),
                                checkboxInput("max_product","Product features",value = TRUE),
                                checkboxInput("max_threshold","Threshold features",value = TRUE),
                                checkboxInput("max_hinge","Hinge features",value = TRUE),
                                checkboxInput("max_logscale","Logscale pictures",value = TRUE),
                                checkboxInput("max_jackknife","Do Jackknife",value = TRUE),
                                checkboxInput("max_responsecurves","Create response curves",value=TRUE),
                                selectInput("maxent_output","Output format",choice=c("Logistic"="logistic",
                                                                                     "Cumulative"="cumulative",
                                                                                     "Raw"="raw")),

                                conditionalPanel("input.selectDataMaxEnt== 'user_data' && input.selectM_MaxEnt == 'all_raster'",
                                                 actionButton("run_maxent_user_all","Run")),
                                conditionalPanel("input.selectDataMaxEnt== 'user_data' && input.selectM_MaxEnt == 'mLayers'",
                                                 actionButton("run_maxent_user_mLayers","Run"))

                              )

                            ),mainPanel(uiOutput("maxent_html_user_all")))
