
maxent_model_all_M <- eventReactive(input$run_maxent_all_mLayers,{
  Sys.setenv(NOAWT=TRUE)

  if(!is.null(data_maxent_all()) && !is.null(input$biosMaxEnt)){
    occ <- data_maxent_all()
    sp_df_occ <- SpatialPoints(coords = occ,
                               proj4string =CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    map_vec <- spTransform(myPolygon(), CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    data_poly_all <- over( sp_df_occ , map_vec , fn = NULL)
    en_poligono_index <- which(!is.na(data_poly_all[,1]))
    occtrain <- occ[en_poligono_index,]
    print(occtrain)
    #fold <- kfold(occ, k=5)
    #occtest <- occ[fold == 1, ]
    #occtrain <- occ[fold != 1, ]
    predictors <- define_M_raster()[[input$biosMaxEnt]]
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

max_tex_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    HTML( paste0(h3("Maxent model")),
          paste0(p("Output created on",date())),paste0(downloadLink("max_results_all_M",label = h5("Download complete results"))),
          paste0( h4("Analysis of omission/commission")),
          paste0(p("The following picture shows the omission rate and predicted area as a function of the cumulative threshold.
                   The omission rate is is calculated both on the training presence records, and (if test data are used) on the test records.
                   The omission rate should be close to the predicted omission, because of the definition of the cumulative threshold. ")),
          paste0(plotOutput("maxent_omission_all_M")),
          paste0(p("The next picture is the receiver operating characteristic (ROC) curve for the same data.
                   Note that the specificity is defined using predicted area, rather than true commission
                   (see the paper by Phillips, Anderson and Schapire cited on the help page for discussion
                   of what this means). This implies that the maximum achievable AUC is less than 1.
                   If test data is drawn from the Maxent distribution itself, then the maximum possible test
                   AUC would be 0.959 rather than 1; in practice the test AUC may exceed this bound. ")),
          paste0(plotOutput("maxent_roc_all_M")),
          paste0(p("Some common thresholds and corresponding omission rates are as follows. If test data
                   are available, binomial probabilities are calculated exactly if the number of test samples
                   is at most 25, otherwise using a normal approximation to the binomial. These are 1-sided
                   p-values for the null hypothesis that test points are predicted no better than by a random
                   prediction with the same fractional predicted area. The 'Balance' threshold minimizes 6 *
                   training omission rate + .04 * cumulative threshold + 1.6 * fractional predicted area.")),
          paste0(dataTableOutput("threshold_tb_all_M")),
          paste0(h4("Analysis of variable contributions")),
          paste0(p("The following table gives estimates of relative contributions of the environmental variables
                   to the Maxent model. To determine the first estimate, in each iteration of the training algorithm,
                   the increase in regularized gain is added to the contribution of the corresponding variable,
                   or subtracted from it if the change to the absolute value of lambda is negative. For the second estimate,
                   for each environmental variable in turn, the values of that variable on training presence and background
                   data are randomly permuted. The model is reevaluated on the permuted data, and the resulting drop
                   in training AUC is shown in the table, normalized to percentages. As with the variable jackknife,
                   variable contributions should be interpreted with caution when the predictor variables are correlated.")),
          paste0(dataTableOutput("varcontri_tb_all_M")),
          #paste0(h4("Jackknife plot")),
          paste0(plotOutput("maxent_jackknife_all_M"))
          #paste0(h4("Download complete results")),


          )
  }
  else
    return()

})


output$max_model_all_M <- downloadHandler(
  filename = function() paste0("max_model_",
                               input$selectDataMaxEnt,
                               input$selectM_MaxEnt,"_",
                               input$maxent_output,".asc"),
  content = function(file){
    if(!is.null(maxent_model_all_M())){
      writeRaster(maxent_model_all_M()$model,file)
    }
  }
)


output$maxent_html_all_M <- renderUI({
  if(is.null(max_in_dismo())){
    print(max_in_dismo())
    includeHTML(compile_inst())
  }
  else
    max_tex_all_M()
})


maxent_jackknife_all_M <- reactive({
  if(!is.null(maxent_model_all_M()) && input$max_jackknife){
    plot_dir <- maxent_model_all_M()$max_dir_plots
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

output$maxent_jackknife_all_M <- renderPlot({
  if(!is.null(maxent_jackknife_all_M()))
    maxent_jackknife_all_M()
})


maxent_omission_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    plot_dir <- maxent_model_all_M()$max_dir_plots
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

output$maxent_omission_all_M <- renderPlot({
  if(!is.null(maxent_omission_all_M()))
    maxent_omission_all_M()
})

maxent_roc_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    plot_dir <- maxent_model_all_M()$max_dir_plots
    roc <- paste0(plot_dir,"/","species_roc.png")
    roc_plot <- readPNG(roc)
    plot(c(0,1.4),c(0,1), type='n', xaxt='n', yaxt='n', ann=FALSE)
    limit <- par()
    rasterImage(roc_plot, limit$usr[1], limit$usr[3], limit$usr[2], limit$usr[4])
  }
  else
    return()
})
output$maxent_roc_all_M <- renderPlot({
  if(!is.null(maxent_roc_all_M()))
    maxent_roc_all_M()
})

threshold_tb_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    html_dir <- maxent_model_all_M()$max_dir_files
    html_file <- paste0(html_dir,"/","maxent.html")
    html_table <- readHTMLTable(html_file)[[1]]
    return(html_table)
  }
  else
    return()
})

output$threshold_tb_all_M <- renderDataTable({
  if(!is.null(threshold_tb_all_M()))
    threshold_tb_all_M()
})



varcontri_tb_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    html_dir <- maxent_model_all_M()$max_dir_files
    html_file <- paste0(html_dir,"/","maxent.html")
    html_table <- readHTMLTable(html_file)[[2]]
    return(html_table)
  }
  else
    return()
})

output$varcontri_tb_all_M <- renderDataTable({
  if(!is.null(varcontri_tb_all_M()))
    varcontri_tb_all_M()
})

max_results_all_M <- reactive({
  if(!is.null(maxent_model_all_M())){
    folder <- maxent_model_all_M()$max_dir_files
    folder <- gsub(x = folder,"/maxent/[(0-9)]+/maxent.html",
                   replacement = "")
    tarfile <- tempfile(pattern = "max_results",fileext = ".tgz")
    tar(tarfile,folder,compression='gzip')
    return(tarfile)
  }
})


output$max_results_all_M <- downloadHandler(
  filename = function() paste0("max_results_",
                               input$selectDataMaxEnt,
                               input$selectM_MaxEnt,".tgz"),
  content = function(file){
    if(!is.null(max_results_all_M())){
      file.copy(max_results_all_M(),file)
    }
  }
)
