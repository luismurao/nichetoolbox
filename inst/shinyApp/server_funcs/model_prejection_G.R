# Methods to project models in geographic space
# using leaflet maps


observe({
  if(!is.null(ellip_model_all_rast()))
    ellip_all <- "Ellipsoid_all_extent"
  else
    ellip_all <- NULL
  if(!is.null(ellip_model_m_rast()))
    ellip_m <- "Ellipsoid_M_extent"
  else
    ellip_m <- NULL
  if(!is.null(bioclim_model_all()))
    bioclim_all <- "Bioclim_all_extent"
  else
    bioclim_all <- NULL
  if(!is.null(bioclim_model_m()))
    bioclim_m <- "Bioclim_M_extent"
  else
    bioclim_m <- NULL
  in_ntb <- c(ellip_all,ellip_m,
              bioclim_all,bioclim_m)

  updateSelectInput(session, "proj_model",choices = in_ntb)
})



leaf_ellip_all <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(input$proj_model == "Ellipsoid_all_extent"){

    model <- ellip_model_all_rast()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})



leaf_ellip_m <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(input$proj_model == "Ellipsoid_M_extent"){

    model <- ellip_model_m_rast()$suitRaster
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})




leaf_bioclim_all <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(input$proj_model == "Bioclim_all_extent"){

    model <- bioclim_model_all()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})

leaf_bioclim_m <- reactive({

  map <- leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    )

  if(input$proj_model == "Bioclim_M_extent"){

    model <- bioclim_model_m()$prediction
    cbbPalette <- rev(terrain.colors(100))
    pal <- colorNumeric(cbbPalette, values(model),
                        na.color = "transparent")

    crs(model) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    map <- map %>% addRasterImage(model, colors = pal, opacity = 0.5) %>%
      addLegend(pal = pal, values = values(model),
                position = "topleft",labFormat = labelFormat(),
                title = "Suitability")
  }

  else{
    map <- map %>%  setView(lng = 0, lat = 0, zoom = 3)

  }
  return(map)
})


to_plot_model <- reactive({
  if(input$proj_model=="Ellipsoid_all_extent")
    return(leaf_ellip_all())
  if(input$proj_model == "Ellipsoid_M_extent")
    return(leaf_ellip_m())
  if(input$proj_model == "Bioclim_all_extent")
    return(leaf_bioclim_all())
  if(input$proj_model == "Bioclim_M_extent")
    return(leaf_bioclim_m())
  else{
    map <- leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      )
    return(map)
  }

})

output$ras_models <- renderLeaflet({
  to_plot_model()
})

