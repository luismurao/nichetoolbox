observe({
  if(!is.null(data_extraction())){
    # Suggest variables to fit ellipsoid accoring to strog correlations
    if(!is.null(summs_corr_var()))
      var_suggest <- summs_corr_var()$descriptors
    else
      var_suggest <- NULL
    updateSelectInput(session,"biosBioclim",
                      choices = names(data_extraction()),selected = var_suggest)
  }
})


# ---------------------------------------------------------------------
# Fit Bioclim model
# ---------------------------------------------------------------------


bioclim_model_all <- reactive({
  if(!is.null(data_extraction())){
    input$run_bioclim
    isolate({
      if(input$run_bioclim && input$selectMBio == "wWorld" && !is.null(rasterLayers())){
        model_train <- bioclim(data_extraction()[,input$biosBioclim])
        model <- predict(rasterLayers()[[input$biosBioclim]], model_train)
        return(list(train=model_train,prediction=model))

      }

    })

  }
  else
    return()
})


# ---------------------------------------------------------------------
# Fit Bioclim model m raster
# ---------------------------------------------------------------------


bioclim_model_m <- reactive({
  if(!is.null(data_extraction())){
    input$run_bioclim
    isolate({
      if(input$run_bioclim && input$selectMBio == "mLayers" && !is.null(define_M_raster())){
        model_train <-  bioclim(data_extraction()[,input$biosBioclim])
        model <- predict(define_M_raster()[[input$biosBioclim]], model_train)
        return(list(train=model_train,prediction=model))

      }
    })

  }
  else
    return()
})

output$bio_response <- renderPlot({
  if(!is.null(bioclim_model_all()) && input$selectMBio == "wWorld")
    response(bioclim_model_all()$train)
  if(!is.null(bioclim_model_m()) && input$selectMBio == "mLayers")
    response(bioclim_model_m()$train)
})
