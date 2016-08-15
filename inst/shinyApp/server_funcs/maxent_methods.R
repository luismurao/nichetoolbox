# ------------------------------------------------------------------------------
# Update options depending on information aviable in NicheToolBox

data_in_ntb1 <- reactiveValues()
layers_in_ntb1 <- reactiveValues()

observe({
  if(!is.null(data_gbif()))
    data_in_nt1b$gbif <- c("GBIF data"="gbif_data")
})

observe({
  if(!is.null(data_user_clean()))
    data_in_ntb1$user <- c("User data"="user_data")
})

observe({
  if(!is.null(data_poly())){

    m_data <- c("m_data")
    names(m_data) <- paste0("M data ","(",input$dataset_dynMap,")")
    data_in_ntb1$M_data <- m_data
  }

})

observe({
  data_in_ntb1 <- unlist(unname(reactiveValuesToList(data_in_ntb1)))
  updateSelectInput(session,"selectDataMaxEnt",choices =data_in_ntb1)
})




observe({
  if(!is.null(rasterLayers())){
    layers_in_ntb1$all_extent <-  c("All raster extent"="all_raster")
  }
})

observe({
  if(!is.null(define_M_raster())){
    layers_in_ntb1$m_extent <-  c("M raster extent"="mLayers")
  }
})
observe({
  layers_in_ntb1 <- unname(reactiveValuesToList(layers_in_ntb1))
  layers_in_ntb1 <- unlist(layers_in_ntb1)
  updateSelectInput(session,"selectM_MaxEnt",choices = layers_in_ntb1)
})

observe({

  var_suggest <- names(rasterLayers())
  if(!is.null(summs_corr_var()) && input$cor_data_from== "wWorld")
    var_suggest <- summs_corr_var()$descriptors
  if(!is.null(summs_corr_var()) && input$cor_data_from== "mLayers")
    var_suggest <- summs_corr_var()$descriptors
  if(input$selectM_MaxEnt == "all_raster"){
    updateSelectInput(session,"biosMaxEnt",choices = names(rasterLayers()),selected = var_suggest)

  }
  if(input$selectM_MaxEnt == "mLayers"){
    updateSelectInput(session,"biosMaxEnt",choices = names(define_M_raster()),selected = var_suggest)

  }
})


data_maxent_M <- reactive({
  if(!is.null(data_poly()) && input$selectDataMaxEnt == "m_data"){
    data <- data_poly()
    data <- data[,c(data_set()$longitude,
                    data_set()$latitude)]
    return(data)
  }
  else
    return()
})

data_maxent_user_all <- reactive({
  if(input$selectDataMaxEnt == "user_data" && !is.null(data_user_clean()))
    return(data_user_clean()[,c(input$xLongitudeUser,input$yLatitudeUser)])
  else
    return()
})

data_maxent_gbif <- reactive({
  if(input$selectDataMaxEnt == "gbif_dat" && !is.null(data_gbif()))
    return(data_gbif()[,c(input$xLongitudeGBIF,input$yLatitudeGBIF)])
  else
    return()
})

# Read user uploaded data

max_test_file <- reactive({

  if (is.null(input$max_testing_file))
    return(NULL)
  else if (identical(input$max_format_test, 'CSV')){
    print(input$max_testing_file$datapath)
    return(input$max_testing_file$datapath)
  }

})

maxent_args <- reactive({
  args <- c("autorun=TRUE", "visible=FALSE", "warnings=FALSE", "tooltips=FALSE", "askoverwrite=FALSE", "skipifexists=FALSE",
            "prefixes=TRUE", "verbose=FALSE", "pictures=TRUE", "writeclampgrid=TRUE",
            "writemess=TRUE", "writebackgroundpredictions=TRUE", "writeplotdata=TRUE", "outputgrids=TRUE", "plots=TRUE",
            "appendtoresultsfile=FALSE", "threads=1", "adjustsampleradius=0", "logfile=\"maxent.log\"", "cache=TRUE",
            "allowpartialdata=FALSE", "perspeciesresults=FALSE", "responsecurvesexponent=FALSE",
            #, "outputfiletype=\"asc\""
            ### based on user modified
            #		paste('outputformat=',outputformat,sep=''),
            #paste('randomseed=',randomseed,sep=''),
            paste0("responsecurves=",input$max_responsecurves),
            paste0("outputformat=",input$maxent_output),
            paste0('logscale=',input$max_logscale),
            paste0("jackknife=",input$max_jackknife),
            paste0('removeduplicates=',input$max_remv_dup),
            paste0('randomtestpoints=',input$max_rand_test_per),
            paste0('betamultiplier=',input$max_beta_multiplier),
            paste0('maximumbackground=',input$max_nbg),
            paste0('replicates=',input$max_repli_numer),
            paste0('replicatetype=',input$max_repli_type),
            #paste0('biasfile=',biasfile),
            #paste0('testsamplesfile=',max_test_file()),
            paste0('linear=',input$max_linear),
            paste0('quadratic=',input$max_quadratic),
            paste0('product=',input$max_product),
            paste0('threshold=',input$max_threshold),
            paste0('hinge=',input$max_hinge),
            paste0('addsamplestobackground=',input$max_add_samp_to_bg),
            paste0('addallsamplestobackground=',input$max_add_all_samp_to_bg),
            #paste0('fadebyclamping=',fadebyclamping),
            paste0('extrapolate=',input$max_extrapolate),
            #paste0('autofeature=',autofeature),
            #paste0('doclamp=',doclamp),
            paste0('maximumiterations=',input$max_num_iterations),
            paste0('convergencethreshold=',input$max_threshold_conv),
            paste0('defaultprevalence=',input$max_prevalence),
            paste0('lq2lqptthreshold=',input$max_lq2lqptthreshold),
            paste0('l2lqthreshold=',input$max_l2lqthreshold),
            paste0('hingethreshold=',input$max_hingethreshold),
            paste0('beta_threshold=',input$max_beta_threshold),
            paste0('beta_categorical=',input$max_beta_categorical),
            paste0('beta_lqp=',input$max_beta_lqp),
            paste0('beta_hinge=',input$max_beta_hinge)
            #paste0('nodata=',input$max_no_data)
            )
  return(args)
})

maxent_model_user_all <- eventReactive(input$run_maxent_user_all,{
  Sys.setenv(NOAWT=TRUE)

  if(!is.null(data_maxent_user_all()) && !is.null(input$biosMaxEnt)){
    occ <- data_maxent_user_all()
    fold <- kfold(occ, k=5)
    occtest <- occ[fold == 1, ]
    occtrain <- occ[fold != 1, ]
    predictors <- rasterLayers()[[input$biosMaxEnt]]
    me <- maxent(predictors, occtrain,args=maxent_args())
    max_dir_files <- gsub(x = me@html,"maxent.html",replacement = "")
    print(max_dir_files)
    max_dir_plots <- paste0(max_dir_files,"plots/")

    model <- predict(me, predictors,args=maxent_args())
    return(list(model=model,html=me@html,max_dir_files=max_dir_files,max_dir_plots=max_dir_plots))
  }
  else
    return()

})

max_tex_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    HTML( paste0(h3("Maxent model")),
          paste0(p("Output created on",date())),paste0(downloadLink("max_results_user_all",label = h5("Download complete results"))),
          paste0( h4("Analysis of omission/commission")),
          paste0(p("The following picture shows the omission rate and predicted area as a function of the cumulative threshold.
                  The omission rate is is calculated both on the training presence records, and (if test data are used) on the test records.
                  The omission rate should be close to the predicted omission, because of the definition of the cumulative threshold. ")),
          paste0(plotOutput("maxent_omission_user_all")),
          paste0(p("The next picture is the receiver operating characteristic (ROC) curve for the same data.
                   Note that the specificity is defined using predicted area, rather than true commission
                   (see the paper by Phillips, Anderson and Schapire cited on the help page for discussion
                   of what this means). This implies that the maximum achievable AUC is less than 1.
                   If test data is drawn from the Maxent distribution itself, then the maximum possible test
                   AUC would be 0.959 rather than 1; in practice the test AUC may exceed this bound. ")),
          paste0(plotOutput("maxent_roc_user_all")),
          paste0(p("Some common thresholds and corresponding omission rates are as follows. If test data
                   are available, binomial probabilities are calculated exactly if the number of test samples
                   is at most 25, otherwise using a normal approximation to the binomial. These are 1-sided
                   p-values for the null hypothesis that test points are predicted no better than by a random
                   prediction with the same fractional predicted area. The 'Balance' threshold minimizes 6 *
                   training omission rate + .04 * cumulative threshold + 1.6 * fractional predicted area.")),
          paste0(dataTableOutput("threshold_tb_user_all")),
          paste0(h4("Analysis of variable contributions")),
          paste0(p("The following table gives estimates of relative contributions of the environmental variables
                    to the Maxent model. To determine the first estimate, in each iteration of the training algorithm,
                    the increase in regularized gain is added to the contribution of the corresponding variable,
                    or subtracted from it if the change to the absolute value of lambda is negative. For the second estimate,
                    for each environmental variable in turn, the values of that variable on training presence and background
                    data are randomly permuted. The model is reevaluated on the permuted data, and the resulting drop
                    in training AUC is shown in the table, normalized to percentages. As with the variable jackknife,
                    variable contributions should be interpreted with caution when the predictor variables are correlated.")),
          paste0(dataTableOutput("varcontri_tb_user_all")),
          #paste0(h4("Jackknife plot")),
          paste0(plotOutput("maxent_jackknife_user_all"))
          #paste0(h4("Download complete results")),


    )
  }
  else
    return()

})


output$max_model_user_all <- downloadHandler(
  filename = function() paste0("max_model_",
                               input$selectDataMaxEnt,
                               input$selectM_MaxEnt,"_",
                               input$maxent_output,".asc"),
  content = function(file){
    if(!is.null(maxent_model_user_all())){
      writeRaster(maxent_model_user_all()$model,file)
    }
  }
)


output$maxent_html_user_all <- renderUI({
  max_tex_user_all()
})


maxent_jackknife_user_all <- reactive({
  if(!is.null(maxent_model_user_all()) && input$max_jackknife){
    plot_dir <- maxent_model_user_all()$max_dir_plots
    jacknife <- paste0(plot_dir,"/","species_jacknife.png")
    jacknife_plot <- readPNG(jacknife)
    plot(c(0,1.4),c(0,1), type='n', xaxt='n', yaxt='n', ann=FALSE)
    limit <- par()
    rasterImage(jacknife_plot, limit$usr[1],
                limit$usr[3], limit$usr[2], limit$usr[4])
  }
  else
    return()
})

output$maxent_jackknife_user_all <- renderPlot({
  if(!is.null(maxent_jackknife_user_all()))
    maxent_jackknife_user_all()
})


maxent_omission_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    plot_dir <- maxent_model_user_all()$max_dir_plots
    omission <- paste0(plot_dir,"/","species_omission.png")
    omission_plot <- readPNG(omission)
    plot(c(0,1.4),c(0,1), type='n', xaxt='n', yaxt='n', ann=FALSE)
    limit <- par()
    rasterImage(omission_plot, limit$usr[1],
                limit$usr[3], limit$usr[2], limit$usr[4])
  }
  else
    return()
})

output$maxent_omission_user_all <- renderPlot({
  if(!is.null(maxent_omission_user_all()))
    maxent_omission_user_all()
})

maxent_roc_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    plot_dir <- maxent_model_user_all()$max_dir_plots
    roc <- paste0(plot_dir,"/","species_roc.png")
    roc_plot <- readPNG(roc)
    plot(c(0,1.4),c(0,1), type='n', xaxt='n', yaxt='n', ann=FALSE)
    limit <- par()
    rasterImage(roc_plot, limit$usr[1], limit$usr[3], limit$usr[2], limit$usr[4])
  }
  else
    return()
})
output$maxent_roc_user_all <- renderPlot({
  if(!is.null(maxent_roc_user_all()))
    maxent_roc_user_all()
})

threshold_tb_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    html_dir <- maxent_model_user_all()$max_dir_files
    html_file <- paste0(html_dir,"/","maxent.html")
    html_table <- readHTMLTable(html_file)[[1]]
    return(html_table)
  }
  else
    return()
})

output$threshold_tb_user_all <- renderDataTable({
  if(!is.null(threshold_tb_user_all()))
    threshold_tb_user_all()
})



varcontri_tb_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    html_dir <- maxent_model_user_all()$max_dir_files
    html_file <- paste0(html_dir,"/","maxent.html")
    html_table <- readHTMLTable(html_file)[[2]]
    return(html_table)
  }
  else
    return()
})

output$varcontri_tb_user_all <- renderDataTable({
  if(!is.null(varcontri_tb_user_all()))
    varcontri_tb_user_all()
})

max_results_user_all <- reactive({
  if(!is.null(maxent_model_user_all())){
    folder <- maxent_model_user_all()$max_dir_files
    folder <- gsub(x = folder,"/maxent/[(0-9)]+/maxent.html",
                   replacement = "")
    tarfile <- tempfile(pattern = "max_results",fileext = ".tgz")
    tar(tarfile,folder,compression='gzip')
    return(tarfile)
  }
})


output$max_results_user_all <- downloadHandler(
  filename = function() paste0("max_results_",
                               input$selectDataMaxEnt,
                               input$selectM_MaxEnt,".tgz"),
  content = function(file){
    if(!is.null(max_results_user_all())){
      file.copy(max_results_user_all(),file)
    }
  }
)
