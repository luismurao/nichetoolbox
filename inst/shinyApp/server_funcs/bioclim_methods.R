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


bioclim_model_all <- eventReactive(input$run_bioclim_all,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract()) && !is.null(input$biosBioclim) && input$trainBio == "wWorld"){
      model_train <- bioclim(occ_extract()[,input$biosBioclim])
      model <- predict(rasterLayers()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    if(!is.null(occ_extract_from_mask()) && !is.null(input$biosBioclim) && input$selectShape == "mLayers"){
      model_train <- bioclim(occ_extract_from_mask()[,input$biosBioclim])
      model <- predict(rasterLayers()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})


# ---------------------------------------------------------------------
# Fit Bioclim model m raster
# ---------------------------------------------------------------------


bioclim_model_m <- eventReactive(input$run_bioclim_m,{
  if(!is.null(data_extraction())){
    if(!is.null(occ_extract_from_mask()) && !is.null(input$biosBioclim) && input$trainBio == "wWorld"){
      model_train <- bioclim(occ_extract()[,input$biosBioclim])
      model <- predict(define_M_raster()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    if(!is.null(occ_extract_from_mask()) && !is.null(input$biosBioclim) && input$trainBio == "mLayers"){
      model_train <- bioclim(occ_extract_from_mask()[,input$biosBioclim])
      model <- predict(define_M_raster()[[input$biosBioclim]], model_train)
      return(list(train=model_train,prediction=model))
    }
    else
      return()
  }
})


output$bio_response_all <- renderPlot({
  if(!is.null(bioclim_model_all()) && input$selectMBio == "wWorld")
    return(response(bioclim_model_all()$train))
})


output$bio_response_m <- renderPlot({
  if(!is.null(bioclim_model_m()) && input$selectMBio == "mLayers")
    return(response(bioclim_model_m()$train))
})

output$downBiclimRas <- downloadHandler(
  filename <- function() paste0("BioclimModelNTB_trainArea_",input$trainBio,"projected_area_",input$selectMBio,".asc"),
  content <- function(file){
    if(!is.null(bioclim_model_all()) && input$selectMBio == "wWorld"){
      writeRaster(bioclim_model_all()$prediction,file)
    }
    else if(!is.null(bioclim_model_m()) && input$selectMBio == "mLayers"){
      writeRaster(bioclim_model_m()$prediction,file)
    }

  }
)
